%%% @doc

-module(pm_rap).

-behaviour(gen_server).

%% API
-export([start_link/0, create/2, read/1, update/2, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ROOT_DIR, "/tmp").

-record(state, {root_dir}).

%%%===================================================================
%%% API
%%%===================================================================

create(URI, Content) ->
    {ok, URI_} = parse_uri(URI),
    gen_server:call(?SERVER, {create, URI_, Content}).

read(URI) ->
    {ok, URI_} = parse_uri(URI),
    gen_server:call(?SERVER, {read, URI_}).
    
update(URI, Content) ->
    {ok, URI_} = parse_uri(URI),
    gen_server:call(?SERVER, {update, URI_, Content}).

delete(URI) ->
    {ok, URI_} = parse_uri(URI),
    gen_server:call(?SERVER, {delete, URI_}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    %% ok = file:set_cwd(?ROOT_DIR),
    {ok, #state{root_dir = ?ROOT_DIR}}.

%% @private
handle_call({create, {file, _User_info, [], _Port, Path, _Query}, Content}, _From, State) ->
    Reply = case lists:last(Path) of
		$/ ->
		    "Make dir";
		_ ->
		    io:format("Path: ~p~n", [Path]),
		    file:write(Path, Content)
	    end,
    {reply, Reply, State};
handle_call({read, {file, _User_info, [], _Port, Path, _Query}}, _From, State) ->
    Reply = file:read(Path),
    {reply, Reply, State};
handle_call({update, {file, _User_info, [], _Port, Path, _Query}, Content}, _From, State) ->
    Reply = file:write(Path, Content),
    {reply, Reply, State};
handle_call({delete, {file, _User_info, [], _Port, Path, _Query}}, _From, State) ->
    Reply = file:delete(Path),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    %% TODO: remove default clause
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    %% TODO: remove default clause
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

parse_uri(URI) ->
    http_uri:parse(URI, [{scheme_defaults, schema_defaults()}]).

schema_defaults() ->
    [{file, 0} | http_uri:scheme_defaults()].
