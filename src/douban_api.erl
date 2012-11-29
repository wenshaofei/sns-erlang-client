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

-record(state, {appkey, secret, uri}). 

-define(HttpAPI, "http://api.douban.com/v2").
-define(HttpsAPI, "https://api.douban.com/v2").
-define(TokenTbl, douban_token_table).

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start_link({AppKey, Secret, Uri}) ->
    start_link(?MODULE, {AppKey, Secret, Uri}).

start_link(Server, {AppKey, Secret, Uri}) when is_atom(Server) ->
    gen_server:start_link({local,Server}, ?MODULE, [AppKey,Secret,Uri], []).

call(Request) when is_tuple(Request) ->
    gen_server:call(?MODULE, Request).

call(Server, Request) when is_tuple(Request) ->
    gen_server:call(Server, Request).
    
%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([AppKey, Secret, Uri]) ->
    inets:start(),
    ssl:start(),
    case lists:member(?TokenTbl, ets:all()) of
        % key()   -> access_code()
        % value() -> {userid(), access_token(), refresh_token()}
        false -> ets:new(?TokenTbl, [named_table, set, public]);
        true -> pass
    end,
    {ok, #state{appkey=AppKey, secret=Secret, uri=Uri}}.

handle_call({access_token, Code}, _From, State) ->
    #state{appkey=AppKey, secret=Secret, uri=Uri} = State,
    Url = "https://www.douban.com/service/auth2/token",
    Body = "client_id=" ++ AppKey ++ "&client_secret=" ++ Secret ++ 
           "&redirect_uri=" ++ Uri ++ "&grant_type=authorization_code&code=" 
           ++ Code,
    Result = request(post, Url, Body), 
    Reply = case Result of
        {error, X} -> {error, X};
        Json -> save_token(Code, Json)  % {json, Json}
    end,
    {reply, Reply, State}.

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
request(get, Url) ->
    httpc:request(get, {Url,[]}, [], []).

request(post, Url, Body) ->
    Result = httpc:request(post, 
             {Url,[],"application/x-www-form-urlencoded",Body},
             [], [{full_result,false}]),
    case Result of
        {ok,{200, Json}} -> Json;
        X -> {error, X}
    end.

save_token(Code, Json) ->
    {[{_,_Atk},{_,_Uid},{_,_},{_,_Rtk}]} = ejson:decode(Json),
    Uid = bitstring_to_list(_Uid),
    Atk = bitstring_to_list(_Atk),
    Rtk = bitstring_to_list(_Rtk),
    ets:insert(?TokenTbl, {Code, {Uid, Atk, Rtk}}),
    {json, Json}.

refresh_token(Code) ->
    gen_server:call(?MODULE, {refresh_token, Code}).

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
