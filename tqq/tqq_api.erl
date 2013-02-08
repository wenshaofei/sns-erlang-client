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

-module(tqq_api).

-behaviour(gen_server).

-export([start/1, start/2, call/1, call/2, exit/0, exit/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {code, atk, rtk, id}).

-define(HttpApi, "http://open.t.qq.com/api/").
-define(HttpsApi, "https://open.t.qq.com/api/").
-define(Apikey, "801277002").
-define(Secret, "41968901076668a8cf37fac89757145c").
-define(Uri, "http://www.luanup24.com").
-define(NecParam, [{"format","json"}, {"oauth_consumer_key",?Apikey},
                   {"clientip",?Uri}, {"oauth_version","2.a"},
                   {"scope","all"}]).

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
init([Code, Id]) -> % only when access_token
    {ok, #state{code=Code, id=Id}};
init([_Code, Atk, Id]) ->
    {ok, #state{atk=Atk, id=Id}}.

%%-----------------------------------------------------------------------------
handle_call({access_token}, _From, State) ->
    #state{code=Code, id=Id} = State,
    Url = "https://open.t.qq.com/cgi-bin/oauth2/access_token",
    Param = [{"client_id",?Apikey}, {"client_secret",?Secret},
             {"redirect_uri",?Uri}, {"grant_type","authorization_code"},
             {"code",Code}],
    Result = httpost(Url, Param),
    case Result of
        {error, X} -> {reply, {error, X}, State};
        {json, Str} -> 
            {Atk, Rtk} = tokens(Str),
            Param2 = [{"access_token",Atk}, {"openid",Id} | ?NecParam],
            {json, Info} = httpget(?HttpsApi++"user/info", Param2),
            {reply, {Atk, Info}, State#state{atk=Atk, rtk=Rtk}}
    end;

% refresh_token is currently discarded
handle_call({refresh_token}, _From, State) ->
    #state{rtk=OldRtk} = State,
    Url = "https://open.t.qq.com/cgi-bin/oauth2/access_token",
    Param = [{"client_id",?Apikey}, {"grant_type","refresh_token"},
             {"refresh_token",OldRtk}],
    Result = httpost(Url, Param),
    case Result of
        {error, X} -> {reply, {error, X}, State};
        {json, Str} -> 
            {Atk, Rtk} = tokens(Str),
            {reply, {Atk, Rtk}, State#state{atk=Atk,rtk=Rtk}}
    end;

handle_call({home_timeline, N}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "statuses/home_timeline",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"pageflag","0"}, {"pagetime","0"}, 
             {"reqnum", integer_to_list(N)}, {"type","0"},
             {"contenttype", "0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({home_timeline, N, MaxTs}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "statuses/home_timeline",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"pageflag","1"}, {"pagetime",integer_to_list(MaxTs)}, 
             {"reqnum", integer_to_list(N)}, {"type","0"},
             {"contenttype", "0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({user_timeline, User, N}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "statuses/user_timeline",
    Param = [{"access_token",Atk}, {"openid",Id}, {"name",User},
             {"pageflag","0"}, {"pagetime","0"}, {"lastid","0"}, 
             {"reqnum", integer_to_list(N)}, {"type","0"},
             {"contenttype", "0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({user_timeline, User, N, MaxTs}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "statuses/user_timeline",
    Param = [{"access_token",Atk}, {"openid",Id}, {"name",User},
             {"pageflag","1"}, {"reqnum", integer_to_list(N)},
             {"pagetime", integer_to_list(MaxTs)}, {"lastid","0"}, 
             {"type","0"}, {"contenttype","0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({update, Text}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Url = ?HttpsApi ++ "t/add",
    Param = [{"access_token",Atk}, {"openid",Id}, 
             {"content",UriText} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

% a very rude implement
handle_call({update, Text, ImgPath}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Param = [{"access_token",Atk}, {"openid",Id} | ?NecParam],
    Cmd = lists:concat(["curl -s \"", ?HttpsApi, "t/add_pic?", 
          connect("", Param, "&"), "\" -F \"content=", Text,
          "\" -F \"pic=@", ImgPath, "\""]),
    L = binary_to_list(unicode:characters_to_binary(Cmd)),
    Str = os:cmd(L),
    Reply = case string:str(Str, "{\"data\":") * 
                 string:str(Str, "seqid") of
        0 -> {error, Str};
        _ -> {json, Str}
    end,
    {reply, Reply, State};

handle_call({reshare, StatId}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "t/re_add",
    Param = [{"access_token",Atk}, {"openid",Id}, {"content",""},
             {"reid", integer_to_list(StatId)} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({delete, StatId}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "t/del",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"id", integer_to_list(StatId)} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({comment, StatId, Text}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Url = ?HttpsApi ++ "t/comment",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"reid", integer_to_list(StatId)},
             {"content",UriText} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({comment_list, StatId}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "t/re_list",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"flag","1"}, {"rootid",integer_to_list(StatId)},
             {"pageflag","0"}, {"pagetime","0"},
             {"reqnum","100"}, {"twitterid","0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({delete_comment, StatId}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "t/del",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"id", integer_to_list(StatId)} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({following, _User}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "friends/idollist",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"reqnum","30"}, {"startindex","0"},
             {"mode","0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({followers, _User}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "friends/fanslist",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"reqnum","30"}, {"startindex","0"},
             {"mode","0"}, {"sex","0"} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({follow, User}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "friends/add",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"name",User} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({unfollow, User}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    Url = ?HttpsApi ++ "friends/del",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"name",User} | ?NecParam],
    Reply = httpost(Url, Param),
    {reply, Reply, State};

handle_call({search_user, Text, N}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Url = ?HttpsApi ++ "search/user",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"keyword",UriText}, {"pagesize","15"},
             {"page", integer_to_list(N)} | ?NecParam],
    Reply = httpget(Url, Param),
    {reply, Reply, State};

handle_call({search, Text, N}, _From, State) ->
    #state{atk=Atk, id=Id} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    {H, G, _} = os:timestamp(),
    Ts = integer_to_list(H * 1000000 + G),
    Url = ?HttpsApi ++ "search/t",
    Param = [{"access_token",Atk}, {"openid",Id},
             {"keyword",UriText}, {"pagesize","30"},
             {"contenttype","0"}, {"sorttype","0"}, {"msgtype","0"},
             {"searchtype","0"}, {"starttime","0"}, {"endtime",Ts}, 
             {"page", integer_to_list(N)} | ?NecParam],
    Reply = httpget(Url, Param),
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

tokens(Str) ->
    Param = string:tokens(Str, "&"),
    "access_token=" ++ Atk = lists:nth(1, Param),
    "refresh_token=" ++ Rtk = lists:nth(3, Param),
    {Atk, Rtk}.
