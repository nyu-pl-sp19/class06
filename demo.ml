type 'a tree =
  | Leaf
  | Node of int * 'a * 'a tree * 'a tree

let is_sorted t = 
  let rec check min max = function
  | Leaf -> true
  | Node (k, v, left, right) -> 
    min <= k && k <= max &&
    check min k left && 
    check k max right
  in
  check min_int max_int t

let rec update k v = function
  | Leaf -> Leaf
  | Node (k1, v1, left, right) -> 
    if k < k1 then 
      Node (k1, v1, update k v left, right)
    else if k > k1 then
      Node (k1, v1, left, update k v right)
    else Node (k1, v, left, right)

let rec find k = function
  | Leaf -> None
  | Node (k1, v, left, right) ->
    if k < k1 then
      find k left
    else if k > k1 then
      find k right
    else Some v

let rec remove_duplicates = function
  | [] -> []
  | x1 :: ((x2 :: _) as xs) when x1 = x2 ->
    remove_duplicates xs
  | x :: xs -> 
    x :: remove_duplicates xs

let rec length = function
  | [] -> 0
  | x :: xs1 -> 1 + length xs1            

let rec scale factor = function
  | [] -> []
  | x :: xs -> factor *. x :: scale factor xs

let rec map op = function
  | [] -> []
  | x :: xs -> op x :: map op xs

let scale factor = map (( *.) factor)

let rec incr = function
  | [] -> []
  | x :: xs -> 1 + x :: incr xs

let incr xs = map ((+) 1)

let rec sum = function
  | [] -> 0
  | x :: xs1 -> 
    x + sum xs1            

let rec fold_right op xs z = 
  match xs with
  | [] -> z
  | x :: xs1 -> 
    op x (fold_right op xs1 z)

let sum xs = fold_right (+) xs 0

let rec fold_left op z xs = 
  match xs with
  | [] -> z
  | x :: xs1 -> 
    fold_left op (op z x) xs1

let sum = fold_left (+) 0

let concat xs ys = fold_right (fun z zs -> z :: zs) xs ys

let reverse xs = fold_left (fun zs z -> z :: zs) [] xs
