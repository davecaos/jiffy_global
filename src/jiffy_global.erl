-module(jiffy_global).

-export([
         new/1,
         get/1,
         get/2,
         put/2,
         delete/1
]).

-type type() :: {?MODULE, atom()}.

%%  @doc
%%    Create a module for the Jiffy global instance.
%%  @end
-spec new(atom()) -> type().
new(Key) ->
    {?MODULE, key_to_module(Key)}.

%% @doc
%%   Get the value for `key` or return nil.
%% @end
-spec get(atom()) -> any() | nil.
get(Key) ->
    get(Key, nil).

%%  @doc
%%    Get the value for `key` or return `default`.
%%  @end
-spec get(atom() | type(), any()) -> any().
get({?MODULE, Module}, Default) ->
    do_get(Module, Default);
get(Key, Default) ->
    Module =  key_to_module(Key),
    do_get(Module, Default).

%%  @doc
%%    Store `value` at `key`, replaces an existing value if present.
%%  @end
-spec put(atom() | {?MODULE, module()}, any()) -> ok.
put({?MODULE, Module}, Value) ->
    do_put(Module, Value);
put(Key, Value) ->
     Module =  key_to_module(Key),
     do_put(Module, Value).

%%  @doc
%%    Delete value stored at `key`, no-op if non-existent.
%%  @end
-spec delete(atom() | {?MODULE, module()}) -> ok.
delete({?MODULE, Module}) ->
    do_delete(Module);
delete(Key) ->
     Module =  key_to_module(Key),
     do_delete(Module).


 %% Private

-spec do_get(atom(), any()) -> any().
do_get(Module, Default) ->
    try
      Module:value()
    catch
      error:undef ->
        Default
    end.

-spec do_put(atom(), any())  -> ok.
do_put(Module, Value) ->
    Binary = compile(Module, Value),
    code:purge(Module),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary),
    ok.

-spec do_delete(atom()) ->  ok.
do_delete(Module) ->
    code:purge(Module),
    code:delete(Module).

-spec key_to_module(atom()) ->  atom.
key_to_module(Key) ->
    list_to_atom("jiffy_global:" ++ atom_to_list(Key)).

-spec compile(atom(), any()) ->  binary().
compile(Module, Value) ->
    {ok, Module, Binary} =
        compile:forms(value_to_abstract(Module, Value), [verbose, report_errors]),
    Binary.

-spec value_to_abstract(atom(), any())  ->  [erl_syntax:syntaxTree()].
value_to_abstract(Module, Value) ->
    Data = erl_syntax:revert(erl_syntax:abstract(Value)),
    [
      {attribute, 0, module, Module},
      {attribute, 0, export,  [{value, 0}]},
      {function, 0, value, 0, [{clause, 0, [], [], [Data]}]}
    ].
