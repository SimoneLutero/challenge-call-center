-module(weather_forecast).

-export([get_weather/0]).

get_weather() ->
    inets:start(),
    case httpc:request("http://api.openweathermap.org/data/2.5/onecall?lat=44.414165&lon=8.942184&exclude=current,minutely,hourly&appid=dfd99b153f2fcc53ee63bcb1e2d7ec7a") of
        {ok, {{_, 200, _}, _, Body}} ->
            JsonWeather = jsx:decode(list_to_binary(Body)),
            Daily = maps:get(<<"daily">>, JsonWeather),
            [FirstDay | _] = Daily,
            NextDayWeather = maps:get(<<"weather">>, FirstDay),
            [Desc | _] = NextDayWeather,
            NextDayWeatherDescription = maps:get(<<"description">>, Desc),
            {NextDayWeatherDescription};
        {ok, {{_, Code, _}, _, _}} -> {error, Code};
        {error, Reason} -> {error, Reason}
    end.
