-module(kd_tree).

% TODO: implement https://opendsa-server.cs.vt.edu/ODSA/Books/CS3/html/KDtree.html

-export([new/0, insert/3, get_box/0, get_sphere/3, lookup/2, delete/2]).

-export([fold/3, foreach/2]).

-record(node,
        {key = none :: none | {integer(), integer(), integer()},
         value = none :: none | term(), % TODO: specify a better type for value
         left_node = none :: none | #node{},
         right_node = none :: none | #node{}}).

-type tree() :: #node{}.
-type key() :: {integer(), integer(), integer()}.

-define(CYCLE_LIST, [ x , y , z ]).

new() ->
    %#node{}.
    none.

-spec insert(key(), term(), tree()) -> tree().
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

-spec lookup(key(), tree()) -> term().
lookup({X, Y, Z} = Key, Tree) when is_integer(X) andalso is_integer(Y) andalso is_integer(Z) ->
    case lookup(Key, Tree, ?CYCLE_LIST) of
        #node{key = Key, value = Value} ->
            {ok, Value};
        Err ->
            Err
    end.

lookup(_Key, none, _CycleList) ->
    {error, key_not_found};
lookup(Key, #node{key = Key} = SubTree, _CycleList) ->
    SubTree;
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


% TODO: taking all the leftover nodes after deletion and reinserting them is a bit gross, and I'm not even
%       doing it right.  I should do proper substitution instead.
-spec delete(key(), tree()) -> tree().
delete({X, Y, Z} = Key, Tree) when is_integer(X) andalso is_integer(Y) andalso is_integer(Z) ->
    #node{left_node = LeftCutBranch, right_node = RightCutBranch} = lookup(Key, Tree, ?CYCLE_LIST),
    NewTree = delete(Key, Tree, ?CYCLE_LIST),
    NewTree1 = fold(fun(FunKey, Value, Acc) ->
                            insert(FunKey, Value, Acc)
                    end, NewTree, LeftCutBranch),
    fold(fun(FunKey, Value, Acc) ->
                 insert(FunKey, Value, Acc)
         end, NewTree1, RightCutBranch).

delete(_Key, none, _CycleList) ->
    none;
delete(Key, Tree, []) ->
    delete(Key, Tree, ?CYCLE_LIST);
delete({X, Y, Z}, #node{key = {X, Y, Z}}, _CycleList) ->
    none;
delete({X, _Y, _Z} = Key,  #node{key = {TreeX, _, _}, left_node = LeftNode} = Tree, [x | CycleList])
    when X < TreeX ->
    Tree#node{left_node = delete(Key, LeftNode, CycleList)};
delete(Key,  #node{right_node = RightNode} = Tree, [x | CycleList]) ->
    Tree#node{right_node = delete(Key, RightNode, CycleList)};
delete({_X, Y, _Z} = Key,  #node{key = {_, TreeY, _}, left_node = LeftNode} = Tree, [y | CycleList])
    when Y < TreeY ->
    Tree#node{left_node = delete(Key,  LeftNode, CycleList)};
delete(Key, #node{right_node = RightNode} = Tree, [y | CycleList]) ->
    Tree#node{right_node = delete(Key,  RightNode, CycleList)};
delete({_X, _Y, Z} = Key,  #node{key = {_, _, TreeZ}, left_node = LeftNode} = Tree, [z | CycleList])
    when Z < TreeZ ->
    Tree#node{left_node = delete(Key, LeftNode, CycleList)};
delete(Key,  #node{right_node = RightNode} = Tree, [z | CycleList]) ->
    Tree#node{right_node = delete(Key, RightNode, CycleList)}.


% TODO: make these tail recursive
-spec fold(fun((key(), Value :: term(), FunAcc :: term()) -> term()), Acc :: term(), tree()) -> term().
% fold expects a 3 argument function that takes Key, Value and Acc, and returns an updated Acc.
fold(_Fun, Acc, none) ->
    Acc;
fold(Fun, Acc, #node{key = Key, value = Value, left_node = LeftNode, right_node = RightNode} = _Tree) ->
    Acc2 = Fun(Key, Value, Acc),
    Acc3 = fold(Fun, Acc2, LeftNode),
    fold(Fun, Acc3, RightNode).

-spec foreach(fun((key(), Value :: term()) -> term()), tree()) -> ok.
foreach(_Fun, none) -> ok;
foreach(Fun, #node{key = Key, value = Value, left_node = LeftNode, right_node = RightNode} = _Tree) ->
    Fun(Key, Value),
    foreach(Fun, LeftNode),
    foreach(Fun, RightNode).

% TODO: find an API that makes some sort of sense to represent a box
get_box() ->
    ok.

% TODO: cut the corners of the cube into a sphere
get_sphere({X, Y, Z} = Key, Radius, Tree) when is_integer(X) andalso is_integer(Y) andalso is_integer(Z) andalso is_integer(Radius) andalso Radius >= 0 ->
    get_cube(Key, Radius, Tree, ?CYCLE_LIST, []).

get_cube(_Key, _Radius, none, _CycleList, Acc) ->
    Acc;

get_cube(Key, Radius, Tree, [], Acc) ->
    get_cube(Key, Radius, Tree, ?CYCLE_LIST, Acc);
get_cube({X, _Y, _Z} = Key, Radius, #node{key = {TreeX, _, _}, left_node = LeftNode} = _Tree, [x|CycleList], Acc) when TreeX > X + Radius ->
    get_cube(Key, Radius, LeftNode, CycleList, Acc);
get_cube({X, _Y, _Z} = Key, Radius, #node{key = {TreeX, _, _}, right_node = RightNode} = _Tree, [x|CycleList], Acc) when TreeX < X - Radius ->
    get_cube(Key, Radius, RightNode, CycleList, Acc);
get_cube(Key, Radius, #node{key = NodeKey, value = Value, left_node = LeftNode, right_node = RightNode} = _Tree, [x|CycleList], Acc) ->
    Acc1 = get_cube(Key, Radius, LeftNode, CycleList, [{NodeKey, Value}| Acc]),
    get_cube(Key, Radius, RightNode, CycleList, Acc1);

get_cube({_X, Y, _Z} = Key, Radius, #node{key = {_, TreeY, _}, left_node = LeftNode} = _Tree, [y|CycleList], Acc) when TreeY > Y + Radius ->
    get_cube(Key, Radius, LeftNode, CycleList, Acc);
get_cube({_X, Y, _Z} = Key, Radius, #node{key = {_, TreeY, _}, right_node = RightNode} = _Tree, [y|CycleList], Acc) when TreeY < Y - Radius ->
    get_cube(Key, Radius, RightNode, CycleList, Acc);
get_cube(Key, Radius, #node{key = NodeKey, value = Value, left_node = LeftNode, right_node = RightNode} = _Tree, [y|CycleList], Acc) ->
    Acc1 = get_cube(Key, Radius, LeftNode, CycleList, [{NodeKey, Value}| Acc]),
    get_cube(Key, Radius, RightNode, CycleList, Acc1);

get_cube({_X, _Y, Z} = Key, Radius, #node{key = {_, _, TreeZ}, left_node = LeftNode} = _Tree, [z|CycleList], Acc) when TreeZ > Z + Radius ->
    get_cube(Key, Radius, LeftNode, CycleList, Acc);
get_cube({_X, _Y, Z} = Key, Radius, #node{key = {_, _, TreeZ}, right_node = RightNode} = _Tree, [z|CycleList], Acc) when TreeZ < Z - Radius ->
    get_cube(Key, Radius, RightNode, CycleList, Acc);
get_cube(Key, Radius, #node{key = NodeKey, value = Value, left_node = LeftNode, right_node = RightNode} = _Tree, [z|CycleList], Acc) ->
    Acc1 = get_cube(Key, Radius, LeftNode, CycleList, [{NodeKey, Value}| Acc]),
    get_cube(Key, Radius, RightNode, CycleList, Acc1).
