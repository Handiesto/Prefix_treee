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

let prop_prefix_tree_merge =
  QCheck.Test.make ~count:1000 ~name:"merge trie"
    QCheck.(
      triple (pair string small_nat)
        (pair small_nat small_nat)
        (pair small_nat small_nat) )
    (fun ((key, value1), (value2, value3), (value4, value5)) ->
      let key_a = key ^ "a" in
      let key_b = key ^ "b" in
      let key_c = key ^ "c" in
      let key_d = key ^ "d" in
      let expected_a = value1 in
      let expected_b = value2 in
      let expected_c = value3 in
      let expected_d = value4 in
      let node1 = Prefix_tree.add key_a value1 Prefix_tree.empty in
      let node2 = Prefix_tree.add key_b value2 node1 in
      let node3 = Prefix_tree.add key_c value3 node2 in
      let node4 = Prefix_tree.add key_b value5 Prefix_tree.empty in
      let node5 = Prefix_tree.add key_d value4 node4 in
      let merged = Prefix_tree.merge node3 node5 in
      let equals1 =
        expected_a
        = Option.value ~default:(-1) (Prefix_tree.find key_a merged)
      in
      let equals2 =
        expected_b
        = Option.value ~default:(-1) (Prefix_tree.find key_b merged)
      in
      let equals3 =
        expected_c
        = Option.value ~default:(-1) (Prefix_tree.find key_c merged)
      in
      let equals4 =
        expected_d
        = Option.value ~default:(-1) (Prefix_tree.find key_d merged)
      in
      equals1 && equals2 && equals3 && equals4 )

let associative =
  QCheck.Test.make ~count:1000 ~name:"associative"
    QCheck.(
      quad (pair string small_nat) (pair string small_nat)
        (pair string small_nat) (pair string small_nat) )
    (fun ((key1, value1), (key2, value2), (key3, value3), (key4, value4)) ->
      let node1 = Prefix_tree.add key1 value1 Prefix_tree.empty in
      let node2 = Prefix_tree.add key2 value2 node1 in
      let t1 = Prefix_tree.add key3 value3 node2 in
      let node4 = Prefix_tree.add key2 value2 Prefix_tree.empty in
      let t2 = Prefix_tree.add key4 value4 node4 in
      let node6 = Prefix_tree.add key3 value3 Prefix_tree.empty in
      let t3 = Prefix_tree.add key4 value4 node6 in
      let lhs = Prefix_tree.merge (Prefix_tree.merge t1 t2) t3 in
      let rhs = Prefix_tree.merge t1 (Prefix_tree.merge t2 t3) in
      Prefix_tree.isSame lhs rhs )

let identity =
  QCheck.Test.make ~count:1000 ~name:"associative"
    QCheck.(
      triple (pair string small_nat) (pair string small_nat)
        (pair string small_nat) )
    (fun ((key1, value1), (key2, value2), (key3, value3)) ->
      let node1 = Prefix_tree.add key1 value1 Prefix_tree.empty in
      let node2 = Prefix_tree.add key2 value2 node1 in
      let t = Prefix_tree.add key3 value3 node2 in
      let lhs = Prefix_tree.merge t Prefix_tree.empty in
      let rhs = t in
      Prefix_tree.isSame lhs rhs )

let commutative =
  QCheck.Test.make ~count:1000 ~name:"commutative"
    QCheck.(
      quad (pair string small_nat) (pair string small_nat)
        (pair string small_nat) (pair string small_nat) )
    (fun ((key1, value1), (key2, value2), (key3, value3), (key4, value4)) ->
      let node1 = Prefix_tree.add key1 value1 Prefix_tree.empty in
      let node2 = Prefix_tree.add key2 value2 node1 in
      let t1 = Prefix_tree.add key3 value3 node2 in
      let node4 = Prefix_tree.add key2 value2 Prefix_tree.empty in
      let t2 = Prefix_tree.add key4 value4 node4 in
      let lhs = Prefix_tree.merge t1 t2 in
      let rhs = Prefix_tree.merge t2 t1 in
      Prefix_tree.isSame lhs rhs )

let _ =
  let open OUnit in
  run_test_tt_main
    ( "tests"
    >::: List.map QCheck_ounit.to_ounit_test
           [ insertToTrie
           ; removeFromTrie
           ; prop_prefix_tree_map
           ; associative
           ; identity
           ; prop_prefix_tree_merge ] )
