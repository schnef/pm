%%% @doc

-module(pm_pep).

-behaviour(gen_server).

%% API
-export([start_link/1, access_request/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {u :: pc:u()}).
-record(response, {context, uri}).

%%%===================================================================
%%% API
%%%===================================================================

-spec access_request(Session_pid :: pid(), Op :: pm:op(), Args :: list()) -> Result when
      Result :: {ok, Resource :: term()} | {error, Error :: term()}.
%% @doc
access_request(Session_pid, Op, Args) ->
    gen_server:call(Session_pid, {Op, Args}).

start_link(User) ->
    gen_server:start_link(?MODULE, [User], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([User]) ->
    {ok, #state{u = User}}.

%% @private
handle_call({Op, Args}, _From, #state{u = U} = State) ->
    Reply = case ets:member(rop, Op) of
		true ->
		    Resource = proplists:get_value(resource, Args),
		    case pm_pdp:privilege(U, Op, Resource) of
			{grant, Response} ->
			    Rv = pm_rap:execute(Op, Response#response.uri, Args),
			    pm_epp:event(U, Response#response.context),
			    {ok, Rv};
			deny ->
			    {error, denied};
			Error ->
			    Error
		    end;
		false ->
		    ok
	    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
