-module(prop_mell).

-include_lib("proper/include/proper.hrl").
-include("../src/pm.hrl").

-compile(export_all).

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {pcs, user_dag, object_dag}).

%%%===================================================================
%%% Properties
%%%===================================================================

prop_test() ->
    ?SETUP(fun setup/0,
	   ?FORALL(Cmds, commands(?MODULE),
		   begin
		       pm_pap:clear(),
		       {History, State, Result} = run_commands(?MODULE, Cmds),
		       ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
					   [History, State, Result]),
				 aggregate(command_names(Cmds), Result =:= ok))
		   end)
	  ).

%%%===================================================================
%%% Model
%%%===================================================================

%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    #state{pcs = [], user_dag = [], object_dag = {}}.

%% @doc List of possible commands to run against the system
command(#state{pcs = []}) ->
    {call, ?MODULE, c_pc, [pc()]};
command(#state{pcs = PCs}) ->
    N = rand:uniform(length(PCs)),
    PC = lists:nth(N, PCs),
    %% io:format("~p PCs~n", [length(PCs)]),
    oneof([{call, ?MODULE, c_pc, [pc()]},
	   {call, ?MODULE, d_pc, [PC]}]);
command(_State) ->
    oneof([
        {call, actual_system, some_call, [term(), term()]}
    ]).

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, c_pc, [_PC]}) ->
    true;
precondition(#state{pcs = []}, {call, _Mod, d_pc, [_PC]}) ->
    false;
precondition(_State, {call, _Mod, d_pc, [_PC]}) ->
    true;
precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.

%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(_State, {call, _Mod, c_pc, [_PC]}, PC) -> 
    [PC#pc.id] =:= pm_pap:elements(PC);
postcondition(_State, {call, _Mod, d_pc, [PC]}, Res) ->
    Res =:= ok andalso [] =:= pm_pap:elements(PC);
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    true.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(#state{pcs = PCs} = State, PC, {call, _Mod, c_pc, [_PC]}) ->
    State#state{pcs = [PC | PCs]};
next_state(#state{pcs = PCs} = State, _Res, {call, _Mod, d_pc, [PC]}) ->
    State#state{pcs = lists:delete(PC, PCs)};
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

c_pc(PC1) ->
    {ok, PC2} = pm_pap:c_pc(PC1),
    %% io:format("c_pc ~p~n", [PC2]),
    PC2.

d_pc(PC) ->
    %% io:format("d_pc ~p~n", [PC]),
    pm_pap:d_pc(PC).

%%%===================================================================
%%% Generators
%%%===================================================================

%% Generators for PC, U, UA, O and OA
pc() ->
    ?LET(Name, string(), #pc{value = Name}).
u() ->
    ?LET(Name, string(), #u{value = Name}).
ua() ->
    ?LET(Name, string(), #ua{value = Name}).
o() ->
    ?LET(Name, string(), #o{value = Name}).
oa() ->
    ?LET(Name, string(), #oa{value = Name}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Setup the application, including the database, for running the
%% tests. This function returns a tear down function.
%%
%% Use a temporary databse to run the tests.
%% Start the `pm' application.
%% Afterwards, shut down the `pm' application and
%% remove the temporary database
setup() ->
    zuuid:start(),
    {ok, Cwd} = file:get_cwd(), % current working directory
    Uuid = zuuid:string(zuuid:v1()), % generate a UUID
    Dir = filename:join([Cwd, "test", Uuid]), % construct temporary directory
    ok = file:make_dir(Dir),
    %% Tell Mnesia where to put the temporary database and also
    %% increase the dump_log_write_threshold to suppress overload
    %% warnings.
    application:set_env(mnesia, dir, Dir),
    application:set_env(mnesia, dump_log_write_threshold, 5000),
    pm_db:install([node()]), % install a fresh db
    application:ensure_all_started(pm),
    fun() ->
	    application:stop(pm),
	    application:stop(mnesia),
	    {ok, Filenames} = file:list_dir(Dir),
	    [file:delete(filename:join([Dir, Filename])) || Filename <- Filenames],
	    file:del_dir(Dir)
    end.
