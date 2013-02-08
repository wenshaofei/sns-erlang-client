-module(test).

-export([print/1, main/1]).

print({json, Json}) ->
    Bits = list_to_binary(Json),
    io:fwrite("~ts~n~n", [Bits]);
print(Error) ->
    io:fwrite("~p~n~n", [Error]).

main(_) ->
    tqq_api:start(["00bda0288acb0204baca0d01456c64a8",
                   "CA9799FA5FE7C42FF44D481785B06D04",
                   %"E58BF5ACA15A9FB2F7F4D6562FBD0568",
                   "a0d0ff8f4c34a704e55fbcc91cdaefcf",
                   "435cdfe24d127eda230a11565f288f09"]),
    %print(tqq_api:call({home_timeline, 2})).
    %print(tqq_api:call({home_timeline, 2, 1354473535})),
    %print(tqq_api:call({user_timeline, "xyz7975", 10})),
    %print(tqq_api:call({user_timeline, "KV327033976", 2, 1354473535})),
    %print(tqq_api:call({update, [27979,35797]})),
    %print(tqq_api:call({update, [27979,35797,34567], "/home/wiza/photo/net_lab4.png"})),
    %print(tqq_api:call({reshare, 137828080522805})),
    %print(tqq_api:call({delete, 138836128937709})),
    %print(tqq_api:call({comment_list, 111590114067008})),
    %print(tqq_api:call({following})),
    %print(tqq_api:call({followers})),
    %print(tqq_api:call({search_user, "wizawu", 1})),
    print(tqq_api:call({search, "WCG2012 SKY MOON" ++
                        [25105,20204,30340,38738,26149], 1})),
    tqq_api:exit().
