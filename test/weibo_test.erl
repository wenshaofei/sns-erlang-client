-module(test).

-export([print/1, main/1]).

print({json, Json}) ->
    Bits = list_to_binary(Json),
    io:fwrite("~ts~n~n", [Bits]);
print(Error) ->
    io:fwrite("~p~n~n", [Error]).

main(_) ->
    weibo_api:start(["c69d7aa756679ecba0d22b92c2c5725f",
        "2.00kt7KmB1qX6ND044e782b29KJVNjD"]),
    %print(weibo_api:call({followers, "1627883640"})).
    %print(weibo_api:call({home_timeline, 2, 3520177633392064})),
    %print(weibo_api:call({user_timeline, "1930348565", 1})).
    %print(weibo_api:call({user_timeline, "1216095224", 2, 3520177633392064})).
    %print(weibo_api:call({reshare, 3519906299333768})).
    %print(weibo_api:call({delete, 3520271287515068})).
    %print(weibo_api:call({comment_list, 3519906299333768})).
    %print(weibo_api:call({search, "WCG2012"})).
    print(weibo_api:call({update, [20013], "/home/wiza/photo/net_lab4.png"})).
