-module(kd_tree).

% TODO: implement https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/KDtree.html

-export([new/0, insert/3, get_box/0, lookup/2, delete/0]).

-record(node,
        {key = none :: none | {integer(), integer(), integer()},
         value = none :: none | term(), % TODO: specify a better type for value
         left_node = none :: none | #node{},
         right_node = none :: none | #node{}}).

-define(CYCLE_LIST, [ x , y , z ]).

new() ->
    %#node{}.
    none.

%% inserting into an empty tree sets the key and value for the first node
insert({X, Y, Z} = Key, Value, Tree) when is_integer(X) andalso is_integer(Y) andalso is_integer(Z) ->
    insert(Key, Value, Tree, ?CYCLE_LIST).

insert(Key, Value, none, _CycleList) ->
    #node{key = Key, value = Value};
insert(Key, Value, Tree, []) ->
    insert(Key, Value, Tree, ?CYCLE_LIST);
insert({X, Y, Z}, _Value, #node{key = {X, Y, Z}}, _CycleList) ->
    error(<<"Key already exists">>); % TODO: maybe not crash when key exists?
insert({X, _Y, _Z} = Key, Value, #node{key = {TreeX, _, _}, left_node = LeftNode} = Tree, [x | CycleList])
    when X < TreeX ->
    Tree#node{left_node = insert(Key, Value, LeftNode, CycleList)};
insert(Key, Value, #node{right_node = RightNode} = Tree, [x | CycleList]) ->
    Tree#node{right_node = insert(Key, Value, RightNode, CycleList)};
insert({_X, Y, _Z} = Key, Value, #node{key = {_, TreeY, _}, left_node = LeftNode} = Tree, [y | CycleList])
    when Y < TreeY ->
    Tree#node{left_node = insert(Key, Value, LeftNode, CycleList)};
insert(Key, Value, #node{right_node = RightNode} = Tree, [y | CycleList]) ->
    Tree#node{right_node = insert(Key, Value, RightNode, CycleList)};
insert({_X, _Y, Z} = Key, Value, #node{key = {_, _, TreeZ}, left_node = LeftNode} = Tree, [z | CycleList])
    when Z < TreeZ ->
    Tree#node{left_node = insert(Key, Value, LeftNode, CycleList)};
insert(Key, Value, #node{right_node = RightNode} = Tree, [z | CycleList]) ->
    Tree#node{right_node = insert(Key, Value, RightNode, CycleList)}.

get_box() ->
    ok.

lookup({X, Y, Z} = Key, Tree) when is_integer(X) andalso is_integer(Y) andalso is_integer(Z) ->
    lookup(Key, Tree, ?CYCLE_LIST).

lookup(_Key, none, _CycleList) ->
    {error, key_not_found};
lookup(Key, #node{key = Key, value = Value}, _CycleList) ->
    {ok, Value};
lookup(Key, Tree, []) ->
    lookup(Key, Tree, ?CYCLE_LIST);
lookup({X, _Y, _Z} = Key, #node{key = {TreeX, _, _}, left_node = LeftNode} = _Tree, [x|CycleList]) when X < TreeX ->
    lookup(Key, LeftNode, CycleList);
lookup(Key, #node{right_node = RightNode} = _Tree, [x|CycleList]) ->
    lookup(Key, RightNode, CycleList);
lookup({_X, Y, _Z} = Key, #node{key = {_, TreeY, _}, left_node = LeftNode} = _Tree, [y|CycleList]) when Y < TreeY ->
    lookup(Key, LeftNode, CycleList);
lookup(Key, #node{right_node = RightNode} = _Tree, [y|CycleList]) ->
    lookup(Key, RightNode, CycleList);
lookup({_X, _Y, Z} = Key, #node{key = {_, _, TreeZ}, left_node = LeftNode} = _Tree, [z|CycleList]) when Z < TreeZ ->
    lookup(Key, LeftNode, CycleList);
lookup(Key, #node{right_node = RightNode} = _Tree, [z|CycleList]) ->
    lookup(Key, RightNode, CycleList).

delete() ->
    ok.
