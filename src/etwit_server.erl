%%%-------------------------------------------------------------------
%%% File    : etwit_server.erl
%%% Author  : Andrey Sedinin <sedinin@gmail.com>
%%% Description : Twitter challenge service.
%%%
%%% Created : 23 Dec 2011 by Andrey Sedinin <sedinin@gmail.com>
%%%-------------------------------------------------------------------
-module(etwit_server).

-include("etwit.hrl").
-include_lib("alog/include/alog.hrl").

-behaviour(gen_server).

%% public API
-export([follow/1, follow_revolution/0, get_followers/1, get_weirdchina_followers/0]).

%%
-export([follow_task/3, get_followers_task/2]).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%-define(JOB_TIMEOUT, 30000).
-define(COUNT_TO_FOLLOW, 1000).

-record(job, {initiator, key}).
-record(state, {name, oauth, jobs}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Name) ->
    process_flag(trap_exit, true),
    %% get configuration parameters for oauth
    case application:get_env(oauth) of
        undefined ->
            ?CRITICAL("Could not start ~p, no oauth params in config file.", [Name]),
            {stop, no_config};
        {ok, Params} ->
            ConsumerKey = proplists:get_value(consumer_key, Params),
            ConsumerSecret = proplists:get_value(consumer_secret, Params),
            Token = proplists:get_value(token, Params),
            Secret = proplists:get_value(secret, Params),
            %% TODO: add config params logging.
            OauthData = #oauth_data{consumer_key = ConsumerKey,
                                    consumer_secret = ConsumerSecret,
                                    token = Token, secret = Secret},
            State = #state{name = Name, oauth = OauthData, jobs = dict:new()},
            {ok, State}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({follow, Key}, From, #state{jobs = Jobs, oauth = OauthConf} = State) ->
    %% monitor?
    JobPid = spawn_link(etwit_server, follow_task, [Key, OauthConf, self()]),
    NewJobs = dict:store(JobPid, #job{initiator = From, key = Key}, Jobs),
    {noreply, State#state{jobs = NewJobs}};

handle_call({get_followers, Key}, From, #state{jobs = Jobs} = State) ->
    JobPid = spawn_link(etwit_server, get_followers_task, [Key, self()]),
    NewJobs = dict:store(JobPid, #job{initiator = From, key = Key}, Jobs),
    {noreply, State#state{jobs = NewJobs}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({finished, Result, Pid}, #state{jobs = Jobs} = State) ->
    case dict:find(Pid, Jobs) of
        {ok, Job} ->
            gen_server:reply(Job#job.initiator, Result);
        error ->
            ?ERROR("Impossible: received report from non existing worker:~p", [Pid])
    end,
    NewJobs = dict:erase(Pid, Jobs),
    {noreply, State#state{jobs = NewJobs}};

handle_cast(Res, State) ->
    io:format("cast received: ~p", [Res]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{jobs = Jobs} = State) ->
    ?WARNING("Child died with reson: ~p", [Reason]),
    case dict:find(Pid, Jobs) of
        {ok, Job} ->
            gen_server:reply(Job#job.initiator, {fail, child_died});
        error ->
            ?WARNING("Child died, but no job exists. May be some serous error somewhere?")
    end,
    NewJobs = dict:erase(Pid, Jobs),
    {noreply, State#state{jobs = NewJobs}};

handle_info(Info, State) ->
    io:format("Info received: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% External API
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: follow(Key) -> ok|fail
%% Description: Search for first 1000 twits by 'keyword'
%% Follow all authors of these twits.
%% If could do this in JOB_TIMEOUT milliseconds, returns fail.
%%--------------------------------------------------------------------
follow(Key) ->
    %% schedule a task
    case gen_server:call(?TWIT_SERVER, {follow, Key}, infinity) of
        ok ->
            ?INFO("Done follow authors of first 1000 twits by keyword '~p'", [Key]),
            ok;
        {fail, Reason} ->
            ?ERROR("Could not done job (following authors of first 1000 twits by kw: ~p) reason: ~p", [Key, Reason]),
            fail
    end.

follow_revolution() ->
    follow("revolution").

%%--------------------------------------------------------------------
%% Function: get_followers(Key) -> {ok, Ids}|{fail, timeout}|{fail, Reason}
%% Description: Get followers of Key, returns list of IDs on success,
%% {fail, Reason} on failure
%%--------------------------------------------------------------------
get_followers(Key) ->
    case gen_server:call(?TWIT_SERVER, {get_followers, Key}, infinity) of
        {ok, List} ->
            ?INFO("Success. Received list of ~p followers.~n", [length(List)]),
            {ok, List};
        {fail, Reason} ->
            ?ERROR("Fail. Server returns reason: ~p~n", [Reason]),
            {fail, Reason}
    end.

get_weirdchina_followers() ->
    get_followers("weirdchina").

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
follow_task(Key, OauthConf, ParentPid) ->
    gen_server:cast(ParentPid, {finished, etwit_api:follow_key(Key, ?COUNT_TO_FOLLOW, OauthConf), self()}).

get_followers_task(Key, ParentPid) ->
    gen_server:cast(ParentPid, {finished, etwit_api:get_followers(Key), self()}).
