# Multi-Armed Bandit for Erlang

This module provides some simple MAB strategies for minimizing 'regret' when
allocating resources amongst various possible choices. Example use cases might
be dispatching outbound API calls against multiple different services with
potentially different failure rates.

### MAB Strategies

Currently, there is only one strategy implemented:

- `mab_strategy_bayes`: Uses a beta distribution `B(successes, failures)` to
    determine which bandit is optimal.

There is a defined behaviour, `mab_strategy`, that can be used for implementing
custom strategies that fit in to this framework. The callback interface is
relatively small:
```erlang
% Used to initialize your strategy. Receives the possible choices as a list.
-callback init(Bandits :: list(bandit()), Args :: list()) ->
    {ok, State :: term()}.

% Pull one of the bandits, based on which the strategy belives is optimal.
-callback pull(State :: term()) ->
    {ok, State1 :: term(), Bandit :: bandit()}.

% Feedback method called by the application to inform the strategy whether the
% given bandit executed it's task successfully.
-callback result(State :: term(),
                 Bandit :: bandit(),
                 Outcome :: mab_result(),
                 Extras :: list()) ->
    {ok, State1 :: term()}.
```

To include a MAB in your application logic, you only need the three methods
`mab:new`, `pull` and `result`:

```erlang
% Create a new MAB
{ok, Mab} = mab:new(
    mab_strategy_bayes,
    [VendorA, VendorB, VendorC]
).

%% Generic function using MAB input
make_vendor_request() ->
    % When you need to pick from the mab, use pull to get
    % a bandit based on the current MAB weights
    {ok, Vendor} = mab:pull(Mab),

    % Now, we use the returned object for whatever our application may need
    ActionResult = gen_server:call(Vendor, perform_failable_action),

    % In order to provide feedback to the MAB, we need to check whether or
    % not the bandit performed well.
    Success = case ActionResult of
        {ok, _} -> true;
        {error, _} -> false
    end,

    % And then call mab:result/4 to update the MAB state.
    ok = mab:result(Mab, Vendor, Success),

    % Application logic continues as normal.
    ActionResult.
```
