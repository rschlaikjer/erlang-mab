-module(mab_strategy_bayes).
-behaviour(mab_strategy).

-export([
    init/2,
    pull/1,
    result/4
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Delta at which to consider Lenz's algo to be complete
-define(LENZ_STOP, 1.0e-8).
% Lower limit on float size
-define(LENZ_TINY, 1.0e-30).
% Natural log of the sqrt of tau
-define(LN_SQRT2PI, 0.9189385332046727418).
% Constants for the Lanczos 15 lgamma calculation
-define(LANCZOS_APPROX, [
    0.99999999999999709182,
    57.156235665862923517,
    -59.597960355475491248,
    14.136097974741747174,
    -0.49191381609762019978,
    0.33994649984811888699e-4,
    0.46523628927048575665e-4,
    -0.98374475304879564677e-4,
    0.15808870322491248884e-3,
    -0.21026444172410488319e-3,
    0.21743961811521264320e-3,
    -0.16431810653676389022e-3,
    0.84418223983852743293e-4,
    -0.26190838401581408670e-4,
    0.36899182659531622704e-5
]).


-record(bandit_state, {
    trials = 0 :: non_neg_integer(),
    successes = 0 :: non_neg_integer(),
    success_ringbuffer :: undefined | bit_ringbuffer:bit_ringbuffer()
}).

-record(state, {
    bandit_scores :: #{mab_strategy:bandit() => #bandit_state{}},
    lookback_window :: infinity | pos_integer()
}).

init(Bandits, Args) ->
    Lookback = get_lookback_window(Args),
    InitialScores = lists:foldl(
        fun(Bandit, Acc) ->
            % Initialize the state for each bandit; if lookback is non-infinite,
            % create a ringbuffer to use for tracking success rate.
            maps:put(Bandit, #bandit_state{
                success_ringbuffer=(case Lookback of
                    N when is_integer(N) ->
                        {ok, Ringbuffer} = bit_ringbuffer:new(Lookback),
                        Ringbuffer;
                    _ -> undefinedend
                end
            )}, Acc)
        end,
        #{},
        Bandits
    ),
    State = #state{
        bandit_scores=InitialScores,
        lookback_window=Lookback
    },
    {ok, State}.

get_lookback_window(Args) ->
    case proplists:get_value(lookback_window, Args, infinity) of
        infinity -> infinity;
        N when is_integer(N) andalso N >= 0 -> N
    end.

stats_for_bandit_with_lookback(Bandit, S=#bandit_state{}) ->
    Trials = S#bandit_state.trials,
    Buffer = S#bandit_state.success_ringbuffer,
    {ok, Successes} = bit_ringbuffer:popcnt(Buffer),
    {Bandit, 1 + Successes, 1 + Trials - Successes}.

stats_for_bandit_no_lookback(Bandit, #bandit_state{trials=T, successes=S}) ->
    {Bandit, 1 + S, 1 + T - S}.

pull(State=#state{bandit_scores=BScores, lookback_window=Lookback}) ->
    % Pull the bandit success stats into a form we can use for the beta
    % probability function - 1 + Wins and 1 + Losses as alpha and beta.
    % Need to differentiate the method based on whether there's a limited
    % lookback, since one uses plain int, the other ringbuffer
    StatFun = case Lookback of
        infinity -> fun stats_for_bandit_no_lookback/2;
        N when is_integer(N) -> fun stats_for_bandit_with_lookback/2
    end,
    BanditStats = maps:fold(
        fun(Bandit, BanditState, Acc) ->
            [StatFun(Bandit, BanditState)|Acc]
        end,
        [],
        BScores
    ),

    % Generate a score for each bandit, using a random value and the win/loss
    % info as curve parameters
    Scores = [
        {B, beta_ppf(Wins, Losses, rand:uniform())}
        || {B, Wins, Losses} <- BanditStats
    ],

    % Fold over the results to pick the bandit with the highest score
    {BestBandit, _BestScore} = lists:foldl(
        fun({Bandit, BScore}, {CurBandit, CurBScore}) ->
            case BScore > CurBScore of
                false -> {CurBandit, CurBScore};
                true -> {Bandit, BScore}
            end
        end,
        hd(Scores),
        tl(Scores)
    ),

    {ok, State, BestBandit}.

bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

update_bandit_with_lookback(Bandit=#bandit_state{}, Lookback, Success) ->
    % Append a 1 to the ringbuffer on success, 0 on failure
    Ringbuffer = Bandit#bandit_state.success_ringbuffer,
    bit_ringbuffer:append(Ringbuffer, bool_to_int(Success)),

    % Update the trial count, capping at the buffer size
    Trials = Bandit#bandit_state.trials,
    Bandit#bandit_state{trials=min(Lookback, Trials + 1)}.

update_bandit_no_lookback(Bandit=#bandit_state{}, Success) ->
    Bandit#bandit_state{
        trials=Bandit#bandit_state.trials + 1,
        successes=Bandit#bandit_state.successes + bool_to_int(Success)
    }.

result(State, Bandit, Success, _Extras) ->
    % Update the bandit in question, incrementing the trial count
    % and if Outcome =:= success, the success count
    UpdateFun = case State#state.lookback_window of
        infinity ->
            fun(BanditState) -> update_bandit_no_lookback(BanditState, Success) end;
        N when is_integer(N) ->
            fun(BanditState) -> update_bandit_with_lookback(BanditState, N, Success) end
    end,
    BanditState = maps:update_with(
        Bandit,
        UpdateFun,
        State#state.bandit_scores
    ),
    State1 = State#state{
        bandit_scores=BanditState
    },
    {ok, State1}.

% Percent point function, inverse of cdf
beta_ppf(A, B, X) ->
    try 1.0 / ibeta(A, B, X) of V -> V
    catch error:badarith -> nan
    end.

% Incomplete Beta function, which is also the cdf
ibeta(A, B, X) ->
    case X of
        _ when X < 0.0 -> nan;
        _ when X > 1.0 -> nan;
        _ ->
            case X > (A + 1.0) / (A + B + 2.0) of
                true ->
                    1 - ibeta(B, A, 1.0 - X);
                false ->
                    LBeta_AB = lgamma(A) + lgamma(B) - lgamma(A + B),
                    Front = math:exp(math:log(X) * A + math:log(1.0 - X) * B - LBeta_AB) / A,
                    ibeta_lenz(200, A, B, X, Front)
            end
    end.

ibeta_lenz(Iterations, A, B, X, Front) ->
    ibeta_lenz(Iterations, Iterations, A, B, X, Front, 0.0, 1.0, 1.0).

ibeta_lenz(0, _Max, _A, _B, _X, _Front, _D, _C, _F) ->
    % Failed to converge
    nan;
ibeta_lenz(Iterations, MaxIterations, A, B, X, Front, D, C, F) ->
    I = MaxIterations - Iterations,
    M = I / 2,
    Numerator = case I of
        0 ->
            1.0;
        N when N rem 2 =:= 0 ->
            (M * (B - M) * X) / ((A + 2.0 * M - 1.0) * (A + 2.0 * M));
        _ ->
            -((A + M) * (A + B + M) * X) / ((A + 2.0 * M) * (A + 2.0 * M + 1))
    end,
    D1 = 1.0 + Numerator * D,
    D2 = case fabs(D1) of
        D1 when D1 =< ?LENZ_TINY -> ?LENZ_TINY;
        _ -> D1
    end,
    D3 = 1.0 / D2,

    C1 = 1.0 + Numerator / C,
    C2 = case fabs(C1) of
        C1 when C1 =< ?LENZ_TINY -> ?LENZ_TINY;
        _ -> C1
    end,

    CD = C2 * D3,
    F1 = F * CD,
    case fabs(1.0 - CD) < ?LENZ_STOP of
        true -> Front * (F1 - 1.0);
        false ->
            ibeta_lenz(Iterations - 1, MaxIterations, A, B, X, Front, D3, C2, F1)
    end.


fabs(N) when is_float(N) andalso N >= 0 -> N;
fabs(N) when is_float(N) -> -N.

lgamma(X) ->
    case X of
        _ when X =< -1.0 -> nan;
        _ ->
            {Acc, _} = lists:foldl(
                fun(Lanc, {A, I}) ->
                    {A + (Lanc / (X + I)), I + 1}
                end,
                {hd(?LANCZOS_APPROX), 1},
                tl(?LANCZOS_APPROX)
            ),
            Tmp = X + (607 / 128.0 + 0.5),
            (?LN_SQRT2PI + math:log(Acc)) + (X + 0.5) * math:log(Tmp) - Tmp
    end.

-ifdef(TEST).

mab_no_lookback_test() ->
    application:ensure_all_started(mab),
    Bandits = [a, b, c],
    {ok, Mab} = mab:new(?MODULE, Bandits),
    {ok, V} = mab:pull(Mab),
    true = lists:member(V, Bandits),
    ok = mab:result(Mab, V, true),
    ok = mab:close(Mab).

mab_lookback_test() ->
    application:ensure_all_started(mab),
    Bandits = [a, b, c],
    {ok, Mab} = mab:new(?MODULE, Bandits, [{lookback_window, 10}]),
    {ok, V} = mab:pull(Mab),
    true = lists:member(V, Bandits),
    ok = mab:result(Mab, V, true),
    ok = mab:close(Mab).

-endif.
