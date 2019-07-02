-module(pm_ever).

%%% @doc

%% API
-export([parse/1]).

-define(LEXER, pm_ever_lexer).

%%%===================================================================
%%% API
%%%===================================================================

parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    file:close(InFile),
    pm_ever_parser:parse(Acc).


loop(InFile,Acc) ->
    case io:request(InFile,{get_until, prompt, ?LEXER, token, [1]}) of
        {ok, Token, _EndLine} ->
	    %% ?DEBUG("Token ~p~n, EndLine ~p~n", [Token, EndLine]),
            loop(InFile, Acc ++ [Token]);
        {error, token} ->
            exit(scanning_error);    
        {eof, _} ->
            Acc
    end.
