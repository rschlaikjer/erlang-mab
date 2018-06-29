-module(mab_strategy_bayes).
-behaviour(mab_strategy).

-export([
    init/2,
    pull/1,
    result/4
]).

-define(LN_SQRT2PI, 0.9189385332046727418).
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
    successes = 0 :: non_neg_integer()
}).

-record(state, {
    bandit_scores :: #{mab_strategy:bandit() => #bandit_state{}}
}).

init(Bandits, _Args) ->
    InitialScores = lists:foldl(
        fun(Bandit, Acc) ->
            maps:put(Bandit, #bandit_state{}, Acc)
        end,
        #{},
        Bandits
    ),
    State = #state{
        bandit_scores=InitialScores
    },
    {ok, State}.

pull(State=#state{bandit_scores=BScores}) ->
    BanditStats = maps:fold(
        fun(B, #bandit_state{trials=T, successes=S}, Acc) ->
            [{B, 1 + S, 1 + T - S}|Acc]
        end,
        [],
        BScores
    ),

    Scores = [
        {B, beta_ppf(Wins, Losses, rand:uniform())}
        || {B, Wins, Losses} <- BanditStats
    ],

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

result(State, Bandit, Outcome, _Extras) ->
    BanditState = maps:update_with(
        Bandit,
        fun(BS=#bandit_state{trials=T, successes=S}) ->
            BS#bandit_state{
                trials=T+1,
                successes=S+case Outcome of success -> 1; _ -> 0 end
            }
        end,
        State#state.bandit_scores
    ),
    State1 = State#state{
        bandit_scores=BanditState
    },
    {ok, State1}.

beta_ppf(A, B, X) ->
    IbetaInv =  1.0 / ibeta(A, B, X),
    X_A_Sub_1 = math:pow(X, A-1),
    X_Sub_1_B_Sub_1 = math:pow(1 - X, B - 1),
    IbetaInv * X_A_Sub_1 * X_Sub_1_B_Sub_1.

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

-define(LENZ_STOP, 1.0e-8).
-define(LENZ_TINY, 1.0e-30).

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
fabs(N) when is_integer(N) andalso N >= 0 -> N;
fabs(N) when is_float(N) -> -N;
fabs(N) when is_integer(N) -> -N.

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
