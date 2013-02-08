%%
%%  Copyright (c) 2012 Hualiang Wu <wizawu@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy
%%  of this software and associated documentation files (the "Software"), to
%%  deal in the Software without restriction, including without limitation the
%%  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%  sell copies of the Software, and to permit persons to whom the Software is
%%  furnished to do so, subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in
%%  all copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%  IN THE SOFTWARE.
%%

-module(weibo_api).

-behaviour(gen_server).

-export([start/1, start/2, call/1, call/2, exit/0, exit/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {code, atk}). 

-define(HttpApi, "http://api.weibo.com/2/").
-define(HttpsApi, "https://api.weibo.com/2/").
-define(Apikey, "2946579160").
-define(Secret, "b2fe081fd04f273f9359378cb76915cf").
-define(Uri, "http://www.luanup24.com").

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start(Args) ->
    start(?MODULE, Args).

start(Server, Args) ->
    inets:start(),
    ssl:start(),
    gen_server:start_link({local, Server}, ?MODULE, Args, []).

% call(Request) -> {Atk, UserInfoJson} | {json, Json} | {error, Why}
call(Request) when is_tuple(Request) ->
    call(?MODULE, Request).

call(Server, Request) when is_tuple(Request) ->
    gen_server:call(Server, Request, 9000).

exit() ->
    erlang:exit(whereis(?MODULE), 'EXIT').
    
exit(Server) ->
    erlang:exit(whereis(Server), 'EXIT').
    
%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([Code]) ->  % only when access_token
    {ok, #state{code=Code}};
init([_Code, Atk]) ->
    {ok, #state{atk=Atk}}.

%%-----------------------------------------------------------------------------
handle_call({access_token}, _From, State) ->
    #state{code=Code} = State,
    Url = "https://api.weibo.com/oauth2/access_token",
    Param = [{"client_id",?Apikey}, {"client_secret",?Secret},
             {"redirect_uri",?Uri}, {"grant_type","authorization_code"},
             {"code",Code}],
    Result = httpost(Url, Param), 
    case Result of
        {error, X} -> {reply, {error, X}, State};
        {json, Json} -> 
            {[{_,A},{_,_},{_,_},{_,U}]} = mochijson2:decode(Json),
            Atk = bitstring_to_list(A),
            Uid = bitstring_to_list(U),
            Param2 = [{"access_token",Atk}, {"uid",Uid}],
            {json, Info} = httpget(?HttpsApi++"users/show.json?", Param2),
            {reply, {Atk, Info}, State#state{atk=Atk}}
    end;

handle_call({home_timeline, N}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count",integer_to_list(N)}],
    Reply = httpget(?HttpsApi++"statuses/home_timeline.json?", Param),
    {reply, Reply, State};

handle_call({home_timeline, N, MaxId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count",integer_to_list(N)},
            {"max_id",integer_to_list(MaxId)}],
    Reply = httpget(?HttpsApi++"statuses/home_timeline.json?", Param),
    {reply, Reply, State};

handle_call({user_timeline, Uid, N}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count",integer_to_list(N)},
            {"uid",Uid}],
    Reply = httpget(?HttpsApi++"statuses/user_timeline.json?", Param),
    {reply, Reply, State};

handle_call({user_timeline, Uid, N, MaxId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count",integer_to_list(N)},
            {"uid",Uid}, {"max_id",integer_to_list(MaxId)}],
    Reply = httpget(?HttpsApi++"statuses/user_timeline.json?", Param),
    {reply, Reply, State};

handle_call({update, Text}, _From, State) ->
    #state{atk=Atk} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Param = [{"access_token",Atk}, {"status",UriText}],
    Reply = httpost(?HttpsApi++"statuses/update.json", Param), 
    {reply, Reply, State};

% a very rude implement
handle_call({update, Text, ImgPath}, _From, State) ->
    #state{atk=Atk} = State,
    Cmd = lists:concat(["curl -s \"", ?HttpsApi, 
          "statuses/upload.json?access_token=", Atk, 
          "\" -F \"status=", Text, "\" -F \"pic=@", ImgPath, "\""]),
    L = binary_to_list(unicode:characters_to_binary(Cmd)),
    Str = os:cmd(L),
    Reply = case string:str(Str, "{\"created_at\":") * 
                 string:str(Str, "comments_count") of
        0 -> {error, Str};
        _ -> {json, Str}
    end,
    {reply, Reply, State};

handle_call({reshare, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"id",integer_to_list(StatId)}],
    Reply = httpost(?HttpsApi++"statuses/repost.json", Param),
    {reply, Reply, State};

handle_call({delete, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"id",integer_to_list(StatId)}],
    Reply = httpost(?HttpsApi++"statuses/destroy.json", Param),
    {reply, Reply, State};

handle_call({comment, StatId, Text}, _From, State) ->
    #state{atk=Atk} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Param = [{"access_token",Atk}, {"id",integer_to_list(StatId)},
            {"comment",UriText}],
    Reply = httpost(?HttpsApi++"comments/create.json", Param),
    {reply, Reply, State};

handle_call({comment_list, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"id",integer_to_list(StatId)},
            {"count","200"}],
    Reply = httpget(?HttpsApi++"comments/show.json?", Param),
    {reply, Reply, State};

handle_call({delete_comment, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"cid",integer_to_list(StatId)}],
    Reply = httpost(?HttpsApi++"comments/destroy.json", Param),
    {reply, Reply, State};

handle_call({following, Uid, N}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count","30"}, {"uid",Uid}, {"cursor",N}],
    Reply = httpget(?HttpsApi++"friendships/friends.json?", Param), 
    {reply, Reply, State};

handle_call({followers, Uid, N}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"count","30"}, {"uid",Uid}, {"cursor",N}],
    Reply = httpget(?HttpsApi++"friendships/followers.json?", Param),
    {reply, Reply, State};

handle_call({follow, Uid}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"uid",Uid}],
    Reply = httpost(?HttpsApi++"friendships/create.json", Param),
    {reply, Reply, State};

handle_call({unfollow, Uid}, _From, State) ->
    #state{atk=Atk} = State,
    Param = [{"access_token",Atk}, {"uid",Uid}],
    Reply = httpost(?HttpsApi++"friendships/destroy.json", Param),
    {reply, Reply, State};

handle_call({search_user, Text}, _From, State) ->
    #state{atk=Atk} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Param = [{"access_token",Atk}, {"q",UriText}, {"count","200"}],
    Reply = httpget(?HttpsApi++"search/suggestions/users.json?", Param),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

%%-----------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%%  Internal
%%-----------------------------------------------------------------------------
httpost(Url, Param) when is_list(Param) ->
    Body = connect("", Param, "&"),
    Result = httpc:request(post, 
             {Url, [], "application/x-www-form-urlencoded", Body},
             [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

httpget(Url, Param) when is_list(Param) ->
    Body = connect("?", Param, "&"),
    Result = httpc:request(get, {Url++Body, []}, [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

connect(Str, [], _S) -> Str;
connect(Str, [H | T], S) ->
    {K, V} = H,
    connect(lists:concat([Str, S, K, "=", V]), T, S).
