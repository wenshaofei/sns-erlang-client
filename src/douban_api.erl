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

-record(state, {code}). 

-define(TokenTbl, douban_token_table).
-define(HttpApi, "http://api.douban.com/").
-define(HttpsApi, "https://api.douban.com/").
-define(Apikey, "0c7a9b3c6305f6b4256fd1b52911e41c").
-define(Secret, "ce9e5a76d5fc4f83").
-define(Uri, "http://222.201.177.33:9000").

%%-----------------------------------------------------------------------------
%%  API
%%-----------------------------------------------------------------------------
start(Code) ->
    start(?MODULE, Code, infinity).

start(Server, Code, Timeout) ->
    inets:start(),
    ssl:start(),
    case lists:member(?TokenTbl, ets:all()) of
        % key()   -> access_code()
        % value() -> {userid(), access_token(), refresh_token()}
        false -> ets:new(?TokenTbl, [named_table, set, public]);
        true -> pass
    end,
    gen_server:start({local, Server}, ?MODULE, [Code, Timeout], []).

call(Request) when is_tuple(Request) ->
    gen_server:call(?MODULE, Request).

call(Server, Request) when is_tuple(Request) ->
    gen_server:call(Server, Request).
    
%%-----------------------------------------------------------------------------
%%  Callback
%%-----------------------------------------------------------------------------
init([Code, Timeout]) ->
    {ok, #state{code=Code}, Timeout}.

%%-----------------------------------------------------------------------------
handle_call({access_token}, _From, State) ->
    #state{code=Code} = State,
    Url = "https://www.douban.com/service/auth2/token",
    Body = "client_id=" ++ ?Apikey ++ "&client_secret=" ++ ?Secret ++ 
           "&redirect_uri=" ++ ?Uri ++ "&grant_type=authorization_code&code=" 
           ++ Code,
    Result = request("", Url, Body), 
    Reply = case Result of
        {error, X} -> {error, X};
        Json -> save_token(Code, Json)  % {json, Json}
    end,
    {reply, Reply, State};

handle_call({refresh_token}, _From, State) ->
    #state{code=Code} = State,
    case ets:lookup(?TokenTbl, Code) of
        [] -> handle_call({access_token, Code}, self(), State);
        [{Code, {_, _, Rtk}}] -> 
            Url = "https://www.douban.com/service/auth2/token",
            Body = "client_id=" ++ ?Apikey ++ "&client_secret=" ++ 
                   ?Secret ++ "&redirect_uri=" ++ ?Uri ++ 
                   "&grant_type=refresh_token&refresh_token=" ++ Rtk,
            Result = request("", Url, Body),
            Reply = case Result of 
                {error, X} -> {error, X};
                Json -> save_token(Code, Json)  % {json, Json}
            end,
            {reply, Reply, State}
    end;

handle_call({home_timeline, N}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N))
    end,
    {reply, Reply, State};

handle_call({home_timeline, N, MaxId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/home_timeline"
                    ++ "?count=" ++ integer_to_list(N)
                    ++ "&until_id=" ++ integer_to_list(MaxId))
    end,
    {reply, Reply, State};

handle_call({user_timeline, User, N}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi++"shuo/v2/statuses/user_timeline/"++User
                    ++ "?count=" ++ integer_to_list(N))
    end,
    {reply, Reply, State};

handle_call({user_timeline, User, N, MaxId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi++"shuo/v2/statuses/user_timeline/"++User
                    ++ "?count=" ++ integer_to_list(N)
                    ++ "&until_id=" ++ integer_to_list(MaxId))
    end,
    {reply, Reply, State};

handle_call({reshare, StatId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/"
                    ++ integer_to_list(StatId) ++ "/reshare", "")
    end,
    {reply, Reply, State};

handle_call({delete, StatId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            requestd(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                     ++ integer_to_list(StatId))
    end,
    {reply, Reply, State};

handle_call({comment, StatId, Text}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                    ++ integer_to_list(StatId) ++ "/comments",
                    "text=" ++ Text)
    end,
    {reply, Reply, State};

handle_call({comment_list, StatId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/" 
                    ++ integer_to_list(StatId) ++ "/comments")
    end,
    {reply, Reply, State};

handle_call({get_comment, StatId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/statuses/comment/" 
                    ++ integer_to_list(StatId))
    end,
    {reply, Reply, State};

handle_call({delete_comment, StatId}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            requestd(Atk, ?HttpsApi ++ "shuo/v2/statuses/comment/" 
                     ++ integer_to_list(StatId))
    end,
    {reply, Reply, State};

handle_call({following, User}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/users/" ++ User
                    ++ "/following")
    end,
    {reply, Reply, State};

handle_call({follower, User}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token},
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/users/" ++ User
                    ++ "/followers")
    end,
    {reply, Reply, State};

handle_call({follow, User}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token};
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/friendships/create",
                    "source=" ++ ?Apikey ++ "&user_id=" ++ User)
    end,
    {reply, Reply, State};

handle_call({unfollow, User}, _From, State) ->
    #state{code=Code} = State,
    Reply = case ets:lookup(?TokenTbl, Code) of
        [] -> {error, no_access_token};
        [{Code, {_, Atk, _}}] ->
            request(Atk, ?HttpsApi ++ "shuo/v2/friendships/destroy",
                    "source=" ++ ?Apikey ++ "&user_id=" ++ User)
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, invalid_request}, State}.

%%-----------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {reply, {error, timeout}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%%  Internal
%%-----------------------------------------------------------------------------
request(Token, Url, Body) ->
    Header = case Token of
        "" -> [];
        _ -> [{"Authorization", "Bearer "++Token}]
    end,
    Result = httpc:request(post, 
             {Url,Header,"application/x-www-form-urlencoded",Body},
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

requestd(Token, Url) ->
    Header = [{"Authorization", "Bearer "++Token}],
    Result = httpc:request(delete, {Url,Header}, [], [{full_result,false}]),
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

% Content -> [content()]
% content() -> {text, Text}   | {image, Bytes} | 
%              {title, Title} | {url, Url}     | {desc, Desc}
update(Code, Content) when is_list(Content) ->
    gen_server:call(?MODULE, {update, Code, Content}).

block(Code, UserId) ->
    gen_server:call(?MODULE, {block, Code, UserId}).

search_user(Text) ->
    gen_server:call(?MODULE, {search_user, Text}).
