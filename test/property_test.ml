open! OUnit2
open! QCheck
open! Prefix_tree

let insertToTrie =
  QCheck.Test.make ~count:1000 ~name:"insert trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let tree0 = Prefix_tree.empty in
      let tree1 = Prefix_tree.add key value tree0 in
      match Prefix_tree.find key tree1 with
      | Some v -> v = value
      | None -> false )

let removeFromTrie =
  QCheck.Test.make ~count:1000 ~name:"remove from trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let tree0 = Prefix_tree.empty in
      let tree1 = Prefix_tree.add key value tree0 in
      let tree2 = Prefix_tree.remove key tree1 in
      match Prefix_tree.find key tree2 with Some _ -> false | None -> true )

let prop_prefix_tree_map =
  QCheck.Test.make ~count:1000 ~name:"insert trie"
    QCheck.(pair string small_nat)
    (fun (key, value) ->
      let tree0 = Prefix_tree.empty in
      let tree1 = Prefix_tree.add key value tree0 in
      let tree2 = Prefix_tree.map (fun x -> x * 2) tree1 in
      match Prefix_tree.find key tree2 with
      | Some v -> v = value * 2
      | None -> false )

let char_gen =
  Gen.map Char.chr (Gen.int_range (Char.code 'a') (Char.code 'z'))

let rec trie_gen n =
  if n <= 0 then
    Gen.oneof
      [ Gen.return Empty
      ; Gen.map (fun v -> Node (Some v, CharMap.empty)) char_gen ]
  else
    let smaller_gen = trie_gen (n - 1) in
    Gen.frequency
      [ (1, Gen.return Empty)
      ; (1, Gen.map (fun v -> Node (Some v, CharMap.empty)) char_gen)
      ; ( 2
        , Gen.map2
            (fun k t ->
              let children = CharMap.add k t CharMap.empty in
              Node (None, children) )
            char_gen smaller_gen )
      ; ( 4
        , Gen.map2
            (fun k t ->
              let children = CharMap.add k t CharMap.empty in
              Node (Some (Gen.generate1 char_gen), children) )
            char_gen smaller_gen ) ]

let trie_arb = make ~print:(fun _ -> "<trie>") (trie_gen 3)

(* merge(t, t) = t *)
let prop_merge_idempotent =
  Test.make ~name:"merge_idempotent" ~count:1000 trie_arb (fun t ->
      isSame (merge t t) t )

(* merge(merge(t1, t2), t3) = merge(t1, merge(t2, t3)) *)
let prop_merge_associative =
  Test.make ~name:"merge_associative" ~count:1000
    (triple trie_arb trie_arb trie_arb) (fun (t1, t2, t3) ->
      isSame (merge (merge t1 t2) t3) (merge t1 (merge t2 t3)) )

(* merge(t, Empty) = t and merge(Empty, t) = t *)
let prop_merge_neutral =
  Test.make ~name:"merge_neutral" ~count:1000 trie_arb (fun t ->
      isSame (merge t Empty) t && isSame (merge Empty t) t )

let _ =
  let open OUnit in
  run_test_tt_main
    ( "tests"
    >::: List.map QCheck_ounit.to_ounit_test
           [ insertToTrie
           ; removeFromTrie
           ; prop_prefix_tree_map
           ; prop_merge_idempotent
           ; prop_merge_associative
           ; prop_merge_neutral ] )
