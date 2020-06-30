-module(joke_of_the_day).

-export([get_joke/0]).

get_joke() ->
    inets:start(),
    case httpc:request("http://official-joke-api.appspot.com/jokes/random") of
        {ok, {{_, 200, _}, _, Body}} ->
            JsonJoke = jsx:decode(list_to_binary(Body)),
            Setup = maps:get(<<"setup">>, JsonJoke),
            Punchline = maps:get(<<"punchline">>, JsonJoke),
            {Setup, Punchline};
        {error, Reason} -> {error, Reason}
    end.
