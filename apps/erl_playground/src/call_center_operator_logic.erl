-module(call_center_operator_logic).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([operator/3]).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

operator(FatherPid, Count, State)
    when Count < 4 ->
    receive
        {Message} when is_binary(Message) ->
            utils:send_response_message("This process Pid is " ++ pid_to_list(self()), State),
            operator(FatherPid, Count + 1, State);

        {Message} when is_integer(Message) ->
            Feedback = case even(Message) of
                true ->
                    "The number is even";
                false ->
                    "The number is odd"
            end,
            utils:send_response_message(Feedback, State),
            operator(FatherPid, Count + 1, State);

        {welcome} ->
            utils:send_response_message("Hi, I'm the operator " ++ pid_to_list(self()) ++ ", how can I help you?", State),
            operator(FatherPid, Count + 1, State);

        {Message} ->
            _ = lager:info("Received ~p", [Message]),
            _ = lager:info("Is list? ~p", [is_list(Message)]),
            _ = lager:info("Is binary? ~p", [is_binary(Message)]),
            utils:send_response_message("I don't understand", State),
            operator(FatherPid, Count + 1, State)

    after
        10000 ->
            utils:send_response_message("Time's up, see you soon", State),
            FatherPid ! {self(), terminated},
            exit(normal)
    end;

operator(FatherPid, Count, State)
    when Count > 3 ->
    utils:send_response_message("Max number of questions reached, see you soon", State),
    FatherPid ! {self(), terminated},
    exit(normal).
            
even(X) when X >= 0 -> (X band 1) == 0.