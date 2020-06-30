-module(joke_of_the_day).

-export([get_joke/0]).

-record(joke, {
    id,
    punchline,
    setup,
    type
}).

get_joke() ->
    inets:start(),
    case httpc:request("http://official-joke-api.appspot.com/jokes/random") of
        {ok, {{_, 200, _}, _, Body}} ->
            JsonJoke = jsx:decode(list_to_binary(Body)),
            jsx:consult(JsonJoke, setup);
        {error, Reason} -> {error, Reason}
    end.

% {ok,{{"HTTP/1.1",200,"OK"},
%      [{"date","Tue, 30 Jun 2020 21:00:58 GMT"},
%       {"etag","W/\"85-u9tPGHz7Gw7DTwMZIO+LTdVBHz4\""},
%       {"server","Google Frontend"},
%       {"vary","Accept-Encoding"},
%       {"content-length","133"},
%       {"content-type","application/json; charset=utf-8"},
%       {"x-powered-by","Express"},
%       {"access-control-allow-origin","*"},
%       {"x-cloud-trace-context",
%        "318f940780de6448090466ad37410749"},
%       {"alt-svc",
%        "h3-29=\":443\"; ma=2592000,h3-27=\":443\"; ma=2592000,h3-25=\":443\"; ma=2592000,h3-T050=\":443\"; ma=2592000,h3-Q050=\":443\"; ma=2592000,h3-Q046=\":443\"; ma=2592000,h3-Q043=\":443\"; ma=2592000,quic=\":443\"; ma=2592000; v=\"46,43\""}],
%      "{\"id\":288,\"type\":\"general\",\"setup\":\"What's the worst thing about ancient history class?\",\"punchline\":\"The teachers tend to Babylon.\"}"}}