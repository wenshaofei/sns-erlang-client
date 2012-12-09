-module(test).

-export([print/1, main/1]).

print({json, Json}) ->
    Bits = list_to_binary(Json),
    io:fwrite("~ts~n~n", [Bits]);
print(Error) ->
    io:fwrite("~p~n~n", [Error]).

main(_) ->
    douban_api:start(["4321cad42efae300",
                     "a0c347cbdbcd52dfe249d7639367f89d",
                     "954a25dad983c38ebfde71908c8b4aaa"]),
    %print(douban_api:call({followers, "wiza"})),
    %print(douban_api:call({home_timeline, 2})),
    %print(douban_api:call({user_timeline, "1930348565", 1})).
    %print(douban_api:call({user_timeline, "1216095224", 2, 3520177633392064})).
    %print(douban_api:call({reshare, 3519906299333768})).
    %print(douban_api:call({delete, 3520271287515068})).
    %print(douban_api:call({comment_list, 1060235628})),
    print(douban_api:call({search_user, "json"})),
    %print(douban_api:call({update, [20013], "/home/wiza/photo/net_lab4.png"})),
    douban_api:exit().
