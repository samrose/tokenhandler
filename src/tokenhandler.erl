%%%-------------------------------------------------------------------
%%% @author Kai Janson <kjanson@CentralStation.local>
%%% @copyright (C) 2012, Kai Janson
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2012 by Kai Janson <kjanson@CentralStation.local>
%%%-------------------------------------------------------------------
-module(tokenhandler).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-export([create_token/1]).


create_token(AgentId) ->
    gen_server:call(?MODULE, {create_token, AgentId}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create_token, AgentId}, _From, State) ->

    Result = case boss_db:find(AgentId) of
		 [] ->
		     {error, nosuchagent};
		 Agent ->
		     %% Agent = boss_db:find(AgentId),
		     RawToken = binary_to_list(Agent:email()) ++ Agent:id() ++ integer_to_list(
										 calendar:datetime_to_gregorian_seconds(
										   calendar:universal_time_to_local_time(
										     calendar:universal_time()))) + 86400,
		     Token = mochihex:to_hex(crypto:sha(RawToken)),
		     error_logger:info_msg("Agent ~p got token ~p from ~p assigned and saved~n", [Agent, Token, RawToken]),
		     NewAgent = Agent:set([
					   {token, Token},
					   {expires_on, calendar:datetime_to_gregorian_seconds(
							  calendar:universal_time_to_local_time(
							    calendar:universal_time())) + 86400}
					  ]),
		     case NewAgent:save() of
			 {ok, _UpdatedAgent} ->
			     {ok, Token};
			 {error, Reason} ->
			     {error, Reason};
			 AnythingElse ->
			     AnythingElse
		     end
	     end,
    {reply, Result, State};
			 
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
