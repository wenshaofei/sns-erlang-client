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

-module(douban_api).

-behaviour(gen_server).

-export([start/1, start/2, call/1, call/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {code, atk, rtk}). 

-define(HttpApi, "http://api.douban.com/").
-define(HttpsApi, "https://api.douban.com/").
-define(Apikey, "0c7a9b3c6305f6b4256fd1b52911e41c").
-define(Secret, "ce9e5a76d5fc4f83").
-define(Uri, "http://222.201.177.33:9000").

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start(Args) ->
    start(?MODULE, Args).

start(Server, Args) ->
    inets:start(),
    ssl:start(),
    gen_server:start({local, Server}, ?MODULE, Args, []).

% call(Request) -> {Code, Atk, Rtk, UserInfoJson} | {Code, NewAtk, NewRtk}
%                  | {json, Json} | {error, Why}
call(Request) when is_tuple(Request) ->
    call(?MODULE, Request).

call(Server, Request) when is_tuple(Request) ->
    gen_server:call(Server, Request).
    
%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([Code, Atk, Rtk]) ->
    {ok, #state{code=Code, atk=Atk, rtk=Rtk}}.

%%-----------------------------------------------------------------------------
handle_call({access_token}, _From, State) ->
    #state{code=Code} = State,
    Url = "https://www.douban.com/service/auth2/token",
    Body = "client_id=" ++ ?Apikey ++ "&client_secret=" ++ ?Secret ++ 
           "&redirect_uri=" ++ ?Uri ++ 
           "&grant_type=authorization_code&code=" ++ Code,
    Result = httpost("", Url, Body), 
    case Result of
        {error, X} -> {reply, {error, X}, State};
        {json, Json} -> 
            {[{_,A},{_,_},{_,_},{_,R}]} = mochijson2:decode(Json),
            Atk = bitstring_to_list(A),
            Rtk = bitstring_to_list(R),
            {json, Info} = httpget(Atk, ?HttpsApi ++ "v2/user/~me"),
            {reply, {Code,Atk,Rtk,Info}, State#state{atk=Atk, rtk=Rtk}}
    end;

handle_call({refresh_token}, _From, State) ->
    #state{code=Code, rtk=OldRtk} = State,
    Url = "https://www.douban.com/service/auth2/token",
    Body = "client_id=" ++ ?Apikey ++ "&client_secret=" ++ ?Secret ++
           "&redirect_uri=" ++ ?Uri ++ 
           "&grant_type=refresh_token&refresh_token=" ++ OldRtk,
    Result = httpost("", Url, Body),
    case Result of
        {error, X} -> {reply, {error, X}, State};
        {json, Json} -> 
            {[{_,A},{_,_},{_,_},{_,R}]} = mochijson2:decode(Json),
            Atk = bitstring_to_list(A),
            Rtk = bitstring_to_list(R),
            {reply, {Code,Atk,Rtk}, State#state{atk=Atk, rtk=Rtk}}
    end;

handle_call({home_timeline, N}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N)),
    {reply, Reply, State};

handle_call({home_timeline, N, MaxId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N)
                    ++ "&until_id=" ++ integer_to_list(MaxId)),
    {reply, Reply, State};

handle_call({user_timeline, User, N}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi++"shuo/v2/statuses/user_timeline/"
                    ++ User ++ "?count=" ++ integer_to_list(N)),
    {reply, Reply, State};

handle_call({user_timeline, User, N, MaxId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi++"shuo/v2/statuses/user_timeline/"
                    ++ User ++ "?count=" ++ integer_to_list(N)
                    ++ "&until_id=" ++ integer_to_list(MaxId)),
    {reply, Reply, State};

handle_call({update, Text}, _From, State) ->
    #state{atk=Atk} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/v2/statuses/",
                    "text=" ++ UriText),
    {reply, Reply, State};

% a very rude implement
handle_call({update, Text, ImgPath}, _From, State) ->
    #state{atk=Atk} = State,
    Cmd = "curl -s \"" ++ ?HttpsApi ++ "shuo/v2/statuses/\" -H "
          ++ "\"Authorization: Bearer " ++ Atk ++ "\" -F \"text=" 
          ++ Text ++ "\" -F \"image=@" ++ ImgPath ++ "\"",
    L = binary_to_list(unicode:characters_to_binary(Cmd)),
    Str = os:cmd(L),
    Reply = case string:str(Str, "{\"category\":") * 
                 string:str(Str, "comments_count") of
        0 -> {error, Str};
        _ -> {json, Str}
    end,
    {reply, Reply, State};

handle_call({reshare, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/v2/statuses/"
                    ++ integer_to_list(StatId) ++ "/reshare", ""),
    {reply, Reply, State};

handle_call({delete, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpdel(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                    ++ integer_to_list(StatId)),
    {reply, Reply, State};

handle_call({comment, StatId, Text}, _From, State) ->
    #state{atk=Atk} = State,
    UriText = encode_uri_rfc3986:encode(Text),
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                    ++ integer_to_list(StatId) ++ "/comments", 
                    "text=" ++ UriText),
    {reply, Reply, State};

handle_call({comment_list, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                    ++ integer_to_list(StatId) ++ "/comments"),
    {reply, Reply, State};

handle_call({get_comment, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpget(Atk, ?HttpsApi ++ "shuo/v2/statuses/comment/" 
                    ++ integer_to_list(StatId)),
    {reply, Reply, State};

handle_call({delete_comment, StatId}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpdel(Atk, ?HttpsApi ++ "shuo/v2/statuses/comment/" 
                    ++ integer_to_list(StatId)),
    {reply, Reply, State};

handle_call({following, User}, _From, State) ->
    Reply = httpget("", ?HttpApi ++ "shuo/v2/users/" ++ User
                     ++ "/following?apikey=" ++ ?Apikey),
    {reply, Reply, State};

handle_call({followers, User}, _From, State) ->
    Reply = httpget("", ?HttpApi ++ "shuo/v2/users/" ++ User
                    ++ "/followers?apikey=" ++ ?Apikey),
    {reply, Reply, State};

handle_call({follow, User}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/friendships/create", 
                    "user_id=" ++ User),
    {reply, Reply, State};

handle_call({unfollow, User}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/friendships/destroy",
                    "user_id=" ++ User),
    {reply, Reply, State};

handle_call({block, User}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/users/" ++ User ++ "/block", ""),
    {reply, Reply, State};

handle_call({unblock, User}, _From, State) ->
    #state{atk=Atk} = State,
    Reply = httpost(Atk, ?HttpsApi ++ "shuo/users/" ++ User ++ "/unblock", ""),
    {reply, Reply, State};

handle_call({search_user, Text}, _From, State) ->
    UriText = encode_uri_rfc3986:encode(Text),
    Reply = httpget("", ?HttpApi ++ "shuo/v2/users/search?q=" ++ UriText
                     ++ "&apikey=" ++ ?Apikey),
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
httpost(Token, Url, Body) ->
    Header = case Token of
        "" -> [];
        _ -> [{"Authorization", "Bearer "++Token}]
    end,
    Result = httpc:request(post, 
             {Url,Header,"application/x-www-form-urlencoded",Body},
             [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

httpget(Token, Url) ->
    Header = case Token of
        "" -> [];
        _ -> [{"Authorization", "Bearer "++Token}]
    end,
    Result = httpc:request(get, {Url,Header}, [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

httpdel(Token, Url) ->
    Header = [{"Authorization", "Bearer "++Token}],
    Result = httpc:request(delete, {Url,Header}, [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.
