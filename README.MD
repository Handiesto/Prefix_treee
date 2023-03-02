## Лабораторная №2

<b>Выполнил:</b> Плясов Климентий Евгеньевич \
<b>Группа:</b> P34112 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Префиксное дерево на OCaml

```ocaml
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let empty = Leaf

let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r

let rec insert value = function
  | Leaf -> Node (value, Leaf, Leaf)
  | Node (curr, l, r) when value < curr -> Node (curr, insert value l, r)
  | Node (curr, l, r) when value > curr -> Node (curr, l, insert value r)
  | Node (curr, l, r) -> Node (curr, l, r)

let rec contains value = function
  | Leaf -> false
  | Node (curr, l, _) when value < curr -> contains value l
  | Node (curr, _, r) when value > curr -> contains value r
  | Node (_, _, _) -> true

let rec min = function
  | Leaf -> failwith "Called on empty tree"
  | Node (curr, Leaf, _) -> curr
  | Node (_, l, _) -> min l

let rec mergeAfterTopErase l r =
  match (l, r) with
  | Leaf, Leaf -> Leaf
  | l, Leaf -> l
  | Leaf, r -> r
  | l, r ->
      let minValue = min r in
      Node (minValue, l, erase minValue r)

and erase value = function
  | Leaf -> Leaf
  | Node (curr, l, _) when value < curr -> erase value l
  | Node (curr, _, r) when value > curr -> erase value r
  | Node (_, l, r) -> mergeAfterTopErase l r

let rec filter f = function
  | Leaf -> Leaf
  | Node (curr, l, r) when f curr -> Node (curr, filter f l, filter f r)
  | Node (_, l, r) -> mergeAfterTopErase (filter f l) (filter f r)

let rec foldr f acc = function
  | Leaf -> acc
  | Node (curr, l, r) ->
      let newAcc = foldr f (f acc curr) r in
      foldl f newAcc l

and foldl f acc = function
  | Leaf -> acc
  | Node (curr, l, r) ->
      let newAcc = foldl f (f acc curr) l in
      foldr f newAcc r

let map f tree = foldl (fun tree x -> insert (f x) tree) Leaf tree

let fromList list = List.fold_left (fun tree x -> insert x tree) Leaf list

let merge t1 t2 = foldl (fun tree x -> insert x tree) t1 t2

let rec isSame t1 t2 =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Leaf, _ -> false
  | _, Leaf -> false
  | Node (curr1, l1, r1), Node (curr2, l2, r2) ->
      curr1 == curr2 && isSame l1 l2 && isSame r1 r2

let isEqualsAsSet t1 t2 =
  size t1 == size t2
  && foldl (fun result x -> result && contains x t2) true t1


```

### Тесты
Property Based тесты при помощи QCheck
Unit тесты при помощи OUnit2

```ocaml

(* association of merge *)
let association =
  QCheck.Test.make ~count:1000 ~name:"association of merge "
    QCheck.(triple (list small_nat) (list small_nat) (list small_nat))
    (fun (a_list, b_list, c_list) ->
      let a = Bitree.fromList a_list in
      let b = Bitree.fromList b_list in
      let c = Bitree.fromList c_list in
      Bitree.isEqualsAsSet
        (Bitree.merge (Bitree.merge a b) c)
        (Bitree.merge a (Bitree.merge b c)) )

(* insert element retains binary tree*)
let insertToBitree =
  QCheck.Test.make ~count:1000 ~name:"insert bitree "
    QCheck.(pair small_nat (list small_nat))
    (fun (to_insert, v_list) ->
      let tree = Bitree.fromList v_list in
      let tree_insert = Bitree.insert to_insert tree in
      isBitreeValid tree && isBitreeValid tree_insert )

(* erase element retains binary tree*)
let eraseToBitree =
  QCheck.Test.make ~count:1000 ~name:"erase bitree "
    QCheck.(list small_nat)
    (fun v_list ->
      let tree = Bitree.fromList v_list in
      let tree_erase =
        match tree with
        | Leaf -> Leaf
        | Node (curr, _, _) -> Bitree.erase curr tree
      in
      isBitreeValid tree && isBitreeValid tree_erase )

(* merge with empty retains tree unchanged *)
let mergeEmpty =
  QCheck.Test.make ~count:1000 ~name:"merge empty"
    QCheck.(list small_nat)
    (fun x_list ->
      let tree = Bitree.fromList x_list in
      let left = Bitree.merge tree Bitree.empty in
      let right = Bitree.merge Bitree.empty tree in
      Bitree.isSame tree left && Bitree.isSame tree right )

```
........
Ran: 8 tests in: 0.00 seconds.
OK
random seed: 48538408
....
Ran: 4 tests in: 0.39 seconds.
OK


### Выводы
Property Based Testing - позволяет автоматически генерировать большое количество тестовых случаев на основе свойств или требований. Были протестированны такие свойства моноида как: ассоциативность, наличие нейтрального элемента, свойства вставки и удаления, которые сохраняют упорядоченность элементов в дереве. В ходе лабораторной работы я научилися использовать Property Based Testing и узнал о существовании monoid