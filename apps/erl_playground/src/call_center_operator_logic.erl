-module(call_center_operator_logic).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([operator/2, operator/4]).

%% ------------------------------------------------------------------
%% API Function Definition
%% ------------------------------------------------------------------

operator(FatherPid, State) ->
    operator(FatherPid, 0, 10000, 3, State).

operator(FatherPid, TimeToLeave, MaxMessageNumber, State) ->
    operator(FatherPid, 0, TimeToLeave, MaxMessageNumber, State).

operator(FatherPid, Count, TimeToLeave, MaxMessageNumber, State)
    when Count < 4 ->
    receive
        {Message} when is_binary(Message) ->
            utils:send_response_message("This process Pid is " ++ pid_to_list(self()), State),
            operator(FatherPid, Count + 1, TimeToLeave, MaxMessageNumber,State);

        {Message} when is_integer(Message) ->
            Feedback = case even(Message) of
                true ->
                    "The number is even";
                false ->
                    "The number is odd"
            end,
            utils:send_response_message(Feedback, State),
            operator(FatherPid, Count + 1, TimeToLeave, MaxMessageNumber,State);

        {welcome} ->
            utils:send_response_message("Hi, I'm the operator " ++ pid_to_list(self()) ++ ", how can I help you?", State),
            operator(FatherPid, Count + 1, TimeToLeave, MaxMessageNumber,State);

        {Message} ->
            _ = lager:info("Received ~p", [Message]),
            _ = lager:info("Is list? ~p", [is_list(Message)]),
            _ = lager:info("Is binary? ~p", [is_binary(Message)]),
            utils:send_response_message("I don't understand", State),
            operator(FatherPid, Count + 1, TimeToLeave, MaxMessageNumber,State)

    after
        TimeToLeave ->
            utils:send_response_message("Time's up, see you soon", State),
            FatherPid ! {self(), terminated},
            exit(normal)
    end;

operator(FatherPid, Count, _, MaxMessageNumber, State)
    when Count > MaxMessageNumber ->
    utils:send_response_message("Max number of questions reached, see you soon", State),
    FatherPid ! {self(), terminated},
    exit(normal).
            
even(X) when X >= 0 -> (X band 1) == 0.