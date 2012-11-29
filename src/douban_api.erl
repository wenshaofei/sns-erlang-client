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

-export([start_link/1, start_link/2, call/1, call/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {apikey, secret, uri}). 

-define(HttpApi, "http://api.douban.com/").
-define(HttpsApi, "https://api.douban.com/").
-define(TokenTbl, douban_token_table).

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start_link({Apikey, Secret, Uri}) ->
    start_link(?MODULE, {Apikey, Secret, Uri}).

start_link(Server, {Apikey, Secret, Uri}) when is_atom(Server) ->
    gen_server:start_link({local,Server}, ?MODULE, [Apikey,Secret,Uri], []).

call(Request) when is_tuple(Request) ->
    gen_server:call(?MODULE, Request).

call(Server, Request) when is_tuple(Request) ->
    gen_server:call(Server, Request).
    
%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([Apikey, Secret, Uri]) ->
    inets:start(),
    ssl:start(),
    case lists:member(?TokenTbl, ets:all()) of
        % key()   -> access_code()
        % value() -> {userid(), access_token(), refresh_token()}
        false -> ets:new(?TokenTbl, [named_table, set, public]);
        true -> pass
    end,
    {ok, #state{apikey=Apikey, secret=Secret, uri=Uri}}.

%%-----------------------------------------------------------------------------
handle_call({access_token, Code}, _From, State) ->
    #state{apikey=Apikey, secret=Secret, uri=Uri} = State,
    Url = "https://www.douban.com/service/auth2/token",
    Body = "client_id=" ++ Apikey ++ "&client_secret=" ++ Secret ++ 
           "&redirect_uri=" ++ Uri ++ "&grant_type=authorization_code&code=" 
           ++ Code,
    Result = request(post, Url, Body), 
    Reply = case Result of
        {error, X} -> {error, X};
        Json -> save_token(Code, Json)  % {json, Json}
    end,
    {reply, Reply, State};

handle_call({refresh_token, Code}, _From, State) ->
    #state{apikey=Apikey, secret=Secret, uri=Uri} = State,
    case ets:lookup(?TokenTbl, Code) of
        [] -> handle_call({access_token, Code}, self(), State);
        [{Code, {_, _, Rtk}}] -> 
            Url = "https://www.douban.com/service/auth2/token",
            Body = "client_id=" ++ Apikey ++ "&client_secret=" ++ 
                   Secret ++ "&redirect_uri=" ++ Uri ++ 
                   "&grant_type=refresh_token&refresh_token=" ++ Rtk,
            Result = request(post, Url, Body),
            Reply = case Result of 
                {error, X} -> {error, X};
                Json -> save_token(Code, Json)  % {json, Json}
            end,
            {reply, Reply, State}
    end;

handle_call({home_timeline, Code, N}, _From, State) ->
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> no_access_token;
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N))
    end,
    {reply, Reply, State};

handle_call({home_timeline, Code, MaxId, N}, _From, State) ->
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> no_access_token;
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N)
                    ++ "&until_id=" ++ integer_to_list(MaxId))
    end,
    {reply, Reply, State}.

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
request(post, Url, Body) ->
    Result = httpc:request(post, 
             {Url,[],"application/x-www-form-urlencoded",Body},
             [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> Json;
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

request(Token, Url) ->
    Header = [{"Authorization", "Bearer "++Token}],
    Result = httpc:request(get, {Url,Header}, [], [{full_result,false}]),
    case Result of
        {ok, {200, Json}} -> {json, Json};
        {ok, X} -> {error, X};
        X -> {error, X}
    end.

save_token(Code, Json) ->
    {[{_,_Atk},{_,_Uid},{_,_},{_,_Rtk}]} = mochijson2:decode(Json),
    Uid = bitstring_to_list(_Uid),
    Atk = bitstring_to_list(_Atk),
    Rtk = bitstring_to_list(_Rtk),
    ets:insert(?TokenTbl, {Code, {Uid, Atk, Rtk}}),
    {json, Json}.

% home_timeline(Token, [1, 50]) will return 50 newest statuses
home_timeline(Token, [A, B]) ->
    gen_server:call(?MODULE, {home_timeline, Token, [A, B]}).

user_timeline(UserId, [A, B]) ->
    gen_server:call(?MODULE, {user_timeline, UserId, [A, B]}).

% Content -> [content()]
% content() -> {text, Text}   | {image, Bytes} | 
%              {title, Title} | {url, Url}     | {desc, Desc}
update(Code, Content) when is_list(Content) ->
    gen_server:call(?MODULE, {update, Code, Content}).
    
reshare(Code, StatId) ->
    gen_server:call(?MODULE, {reshare, Code, StatId}).
    
delete(Code, StatId) ->
    gen_server:call(?MODULE, {delete, Code, StatId}).

comment(Code, Text) ->
    gen_server:call(?MODULE, {comment, Code, Text}).

comment_list(StatId) ->
    gen_server:call(?MODULE, {comment_list, StatId}).

get_comment(CommId) ->
    gen_server:call(?MODULE, {get_comment, CommId}).

delete_comment(Code, CommId) ->
    gen_server:call(?MODULE, {delete_comment, Code, CommId}).

follow(Code, UserId) ->
    gen_server:call(?MODULE, {follow, Code, UserId}).

unfollow(Code, UserId) ->
    gen_server:call(?MODULE, {unfollow, Code, UserId}).

block(Code, UserId) ->
    gen_server:call(?MODULE, {block, Code, UserId}).

search_user(Text) ->
    gen_server:call(?MODULE, {search_user, Text}).
