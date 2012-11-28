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

-export([start_link/3,
         access_token/1, refresh_token/1,
         home_timeline/2, user_timeline_2,
         update/2, reshare/2, delete/2,
         comment/2, comment_list/1, 
       ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {appkey, secret, uri, table}). 

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start_link(AppKey, Secret, URI) ->
    gen_server:start_link(?MODULE, [AppKey, Secret, URI], []).

access_token(Code) -> 
    gen_server:call(?MODULE, {access_token, Code}).

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
    gen_server:call(?MODULE, {delete, StatId}).

comment(Code, Text) ->
    gen_server:call(?MODULE, {comment, Code, Text}).

commmet_list(StatId) ->
    gen_server:call(?MODULE, {comment_list, StatId}).



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

%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([AppKey, Secret, URI]) ->
    ok = inets:start(),
    ssl:start(),
    T = ets:new(anonyms, [set, private]),
    {ok, #state{appkey=AppKey, secret=Secret, uri=URI, table=T}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
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

