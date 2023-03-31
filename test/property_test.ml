open! OUnit2
open! QCheck
open! Prefix_tree

let addToTrie =
  QCheck.Test.make ~count:1000 ~name:"add trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let open IntListTrie in
      let tree0 = empty in
      let tree1 = add (string_to_int_list key) value tree0 in
      match find (string_to_int_list key) tree1 with
      | Some v -> v = value
      | None -> false )

let removeFromTrie =
  QCheck.Test.make ~count:1000 ~name:"remove from trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let open IntListTrie in
      let tree0 = empty in
      let tree1 = add (string_to_int_list key) value tree0 in
      let tree2 = remove (string_to_int_list key) tree1 in
      match find (string_to_int_list key) tree2 with
      | Some _ -> false
      | None -> true )

let prop_prefix_tree_map =
  QCheck.Test.make ~count:1000 ~name:"insert trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let open IntListTrie in
      let tree0 = empty in
      let tree1 = add (string_to_int_list key) value tree0 in
      let tree2 = map (fun x -> x * 2) tree1 in
      match find (string_to_int_list key) tree2 with
      | Some v -> v = value * 2
      | None -> false )

let int_list_gen = Gen.(list_size (int_range 1 5) (int_range 0 9))

let rec trie_gen n =
  let open IntListTrie in
  if n <= 0 then
    Gen.oneof
      [ Gen.return Empty
      ; Gen.map (fun vs -> Node (Some vs, Map.empty)) int_list_gen ]
  else
    let smaller_gen = trie_gen (n - 1) in
    Gen.frequency
      [ (1, Gen.return Empty)
      ; (1, Gen.map (fun vs -> Node (Some vs, Map.empty)) int_list_gen)
      ; ( 2
        , Gen.map2
            (fun k t ->
              let children = Map.add k t Map.empty in
              Node (None, children) )
            (Gen.oneofl [0; 1; 2; 3; 4; 5])
            smaller_gen )
      ; ( 4
        , Gen.map2
            (fun k t ->
              let children = Map.add k t Map.empty in
              Node (Some (Gen.generate1 int_list_gen), children) )
            (Gen.oneofl [0; 1; 2; 3; 4; 5])
            smaller_gen ) ]

let trie_arb = make ~print:(fun _ -> "<trie>") (trie_gen 2)

(* merge(t, t) = t *)
let prop_merge_idempotent =
  let open IntListTrie in
  Test.make ~name:"merge_idempotent" ~count:1000 trie_arb (fun t ->
      isSame (merge t t) t )

(* merge(merge(t1, t2), t3) = merge(t1, merge(t2, t3)) *)
let prop_merge_associative =
  let open IntListTrie in
  Test.make ~name:"merge_associative" ~count:1000
    (triple trie_arb trie_arb trie_arb) (fun (t1, t2, t3) ->
      isSame (merge (merge t1 t2) t3) (merge t1 (merge t2 t3)) )

(* merge(t, Empty) = t and merge(Empty, t) = t *)
let prop_merge_neutral =
  let open IntListTrie in
  Test.make ~name:"merge_neutral" ~count:1000 trie_arb (fun t ->
      isSame (merge t Empty) t && isSame (merge Empty t) t )

let _ =
  let open OUnit in
  run_test_tt_main
    ( "tests"
    >::: List.map QCheck_ounit.to_ounit_test
           [ addToTrie
           ; removeFromTrie
           ; prop_prefix_tree_map
           ; prop_merge_idempotent
           ; prop_merge_associative
           ; prop_merge_neutral ] )
