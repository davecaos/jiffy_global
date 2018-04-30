-module(jiffy_global).

-export([
         new/1,
         get_atom_key/1,
         get_atom_key/2,
         get_string_key/1,
         get_string_key/2,
         get_binary_key/1,
         get_binary_key/2,
         put_atom_key/2,
         put_string_key/2,
         put_binary_key/2,
         delete_atom_key/1,
         delete_string_key/1,
         delete_binary_key/1
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
-spec get_atom_key(atom()) -> any() | nil.
get_atom_key(Key) ->
    get_atom_key(Key, nil).

%% @doc
%%   Get the value for a string() `key` or return nil.
%% @end
-spec get_string_key(atom()) -> any() | nil.
get_string_key(Key) ->
    get_string_key(Key, nil).

%% @doc
%%   Get the value for a binary() `key` or return nil.
%% @end
-spec get_binary_key(atom()) -> any() | nil.
get_binary_key(Key) ->
    get_binary_key(Key, nil).

%%  @doc
%%    Get the value for atom() `key` or return `default`.
%%  @end
-spec get_atom_key(atom() | type(), any()) -> any().
get_atom_key({?MODULE, Module}, Default) ->
    do_get(Module, Default);
get_atom_key(Key, Default) ->
    Module =  key_to_module(Key),
    do_get(Module, Default).


%%  @doc
%%    Get the  value for a string() `key` or return `default`.
%%  @end
-spec get_string_key(string(), any()) -> any().
get_string_key(Key, Default) ->
    Module =  string_key_to_module(Key),
    do_get(Module, Default).

%%  @doc
%%    Get the value for a binary() `key` or return `default`.
%%  @end
-spec get_binary_key(binary(), any()) -> any().
get_binary_key(Key, Default) ->
    Module =  binary_key_to_module(Key),
    do_get(Module, Default).

%%  @doc
%%    Store an  `value` at atom() `key`, replaces an existing value if present.
%%  @end
-spec put_atom_key(atom() | {?MODULE, module()}, any()) -> ok.
put_atom_key({?MODULE, Module}, Value) ->
    do_put(Module, Value);
put_atom_key(Key, Value) ->
     Module =  key_to_module(Key),
     do_put(Module, Value).

%%  @doc
%%    Store an `value` at a string() `key`, replaces an existing value if present.
%%  @end
-spec put_string_key(string(), any()) -> ok.
put_string_key(Key, Value) ->
     Module = string_key_to_module(Key),
     do_put(Module, Value).

%%  @doc
%%    Store an `value` at a binary() `key`, replaces an existing value if present.
%%  @end
-spec put_binary_key(binary(), any()) -> ok.
put_binary_key(Key, Value) ->
     Module = binary_key_to_module(Key),
     do_put(Module, Value).

%%  @doc
%%    Delete value stored at an atom() `key`, no-op if non-existent.
%%  @end
-spec delete_atom_key(atom() | {?MODULE, module()}) -> ok.
delete_atom_key(Key) ->
     Module =  key_to_module(Key),
     do_delete(Module).

%%  @doc
%%    Delete value stored at a string() `key`, no-op if non-existent.
%%  @end
-spec delete_string_key(atom() | {?MODULE, module()}) -> ok.
delete_string_key(Key) ->
     Module =  string_key_to_module(Key),
     do_delete(Module).

%%  @doc
%%    Delete value stored at a binary() `key`, no-op if non-existent.
%%  @end
-spec delete_binary_key(atom() | {?MODULE, module()}) -> ok.
delete_binary_key({?MODULE, Module}) ->
    do_delete(Module);
delete_binary_key(Key) ->
     Module =  binary_key_to_module(Key),
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

-spec binary_key_to_module(binary()) ->  atom.
binary_key_to_module(Key) ->
    list_to_atom("jiffy_global:" ++ binary_to_list(Key)).

-spec string_key_to_module(binary()) ->  atom.
string_key_to_module(Key) ->
    list_to_atom("jiffy_global:" ++ Key).

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
