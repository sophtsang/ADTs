open OUnit2
open Adt.Rbtree

let tree = empty
let tree = insert 6 tree
let tree = insert 4 tree
let tree = insert 9 tree
let tree = insert 1 tree
let tree = insert 5 tree
let tree = insert 8 tree
let tree = insert 2 tree
let tree = insert 3 tree
let _ = print_endline (to_string string_of_int tree 0)
