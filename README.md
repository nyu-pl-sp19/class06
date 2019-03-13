# Class 6

## Algebraic Datatypes

An important and very convenient feature supported by many functional
programming languages is the notion of *algebraic datatypes* (ADTs).
ADTs provide type constructors for building user-defined immutable
tree-like data structures. They are known as *variant types* and
*(disjoint) sum types*. Intuitively, you can think of ADTs as a mix of
enumeration types and record types that you'll find in most imperative
languages. Together with pattern matching, ADTs enable very powerful
programming techniques.

Here is an example of an ADT in OCaml for representing binary search
trees:

```ocaml
type tree =
  | Leaf
  | Node of int * tree * tree
```

This type definition specifies that a value of type `tree` is either a
leaf node, `Leaf`, or an internal node, `Node`, which consists of an
integer value, and two subtrees (the left and right subtree of the
node). `Leaf` and `Node` are referred to as the *variant constructors*
of the ADT `tree`.

The variant constructors `Leaf` and `Node` also serve as the value
constructors for type `tree`:

```ocaml
# let empty = Leaf ;;
val empty : tree = Leaf

# let t = Node (3, Node (1, Leaf, Leaf), Node (6, Leaf, Leaf)) ;;
val t : tree = Node (3, Node (1, Leaf, Leaf), Node (6, Leaf, Leaf))
```

One nice feature of ADTs is that equality `=` is defined structurally
on ADT values, similar to lists and tuples.

### Pattern Matching ADTs

Just like lists and tuples, ADT values can be deconstructed using
pattern matching. Given an ADT variant `C of t1` of some ADT, this
variant automatically introduces a *constructor pattern* `C p1` where
`p1` must be a pattern that matches values of type `t1`. The
constructor pattern then matches a value `v` if `v` is of the form `C v1`
for some value `v1` matched by `p1`.
    
Here is how we can use pattern matching to define a function that checks
whether a binary search tree is sorted:

```ocaml
let is_sorted t = 
  let rec check min max t = 
    match t with
    | Leaf -> true
    | Node (x, left, right) ->
      min <= x && x <= max && 
      check min x left &&
      check x max right
  in
  check min_int max_int t
```

Using the more compact `function` notation for functions that pattern
match on their input values, the nested function `check` can be written more
compactly like this:

```ocaml
let rec check min max = function
| Leaf -> true
| Node (x, left, right) ->
  min <= x && x <= max && 
  check min x left &&
  check x max right
```

### Polymorphic ADTs

ADT definitions can also be polymorphic. For instance, suppose we want
to use a binary search tree data structure to implement maps of
integer keys to values, but we want the data structure to be
parametric in the type of values stored in the map. This can be
done as follows:

```ocaml
type 'a tree =
  | Leaf
  | Node int * 'a * 'a tree * 'a tree
```

```ocaml
# let ti = Node (1, 2, Leaf, Leaf) ;;
val ti : int tree = Node (1, 2, Leaf, Leaf)

# let ts = Node (1, "banana", Leaf, Leaf) ;;
val ts : string tree = Node (1, "string", Leaf, Leaf)
```

Note that an ADT can also take multiple type parameters. For instance,
if we want to abstract from both the type of values as well as the
type of keys in our definition of the type `tree`, this can be done as
follows:

```ocaml
type ('a, 'b) tree =
  | Leaf
  | Node 'a * 'b * ('a, 'b) tree * ('a, 'b) tree
```

The type `('a, 'b) tree` now stands for a binary tree where each inner
node stores two values, one of type `'a` and one of type `'b`. Example:

```ocaml
# Node ("banana", 2, Leaf, Leaf) ;;
- : (string, int) tree = Node ("banana", 2, Leaf, Leaf)
```

### The `option` Type

One of the predefined ADTs of OCaml is the `option` type:

```ocaml
type 'a option =
  | None
  | Some of 'a
```

This type is useful for defining *partial* functions that may not
always have a defined return value. For instance, here is how we can
implement a function `find` that finds the value associated with a
given key `k` in a map implemented as a binary search tree, if such an
association exists:

```ocaml
let rec find k = function
  | Node (k1, v, left, right) ->
    if k1 = k then Some v
    else if k1 > k then find k left
    else find k right
  | Leaf -> None
```

A client of `find` can now pattern-match on the result value of `find`
to determine whether the key `k` was present in the tree and what the
associated value `v` was in that case. The advantage of this
implementation is that the static type checker will check for us that
the client code will also consider the possibility that the key was
not found by `find`.

Contrast this with the use of `null` as an indicator of an undefined
return value in many other languages. The use of `null` values often
leads to run-time errors because the `null` case is not handled by the
client code and the compiler is unable to detect this at compile-time.

#### Tuples and Lists as ADTs

Note that both tuples and lists are just special cases of algebraic
data types. Having tuples and lists as types that are built into the
language is just a matter of convenience. One could just as well
implement lists and tuples as user-defined types in a library. Here,
is a user-defined version of the type `'a list`:

```ocaml
type 'a mylist = 
  | Nil
  | Cons of 'a * 'a mylist
```

```ocaml
# let reverse xs = 
  let rec reverse_helper rev_xs = function
  | Cons (hd, tl) -> reverse_helper (Cons (hd, rev_xs)) tl
  | Nil -> rev_xs
  in reverse_helper Nil xs 
;;
val reverse: 'a mylist -> 'a mylist

# reverse (Cons (1, Cons (2, Cons (3, Nil)))) ;;
- : int mylist = Cons (3, Cons (2, Cons (1, Nil)))
```

And here is an ADT that implements pairs of values:

```ocaml
type ('a, 'b) pair =
  | Pair of 'a * 'b

let fst = function
  | Pair (a, _) -> a
  
let snd = function
  | Pair (_, b) -> b
```


## Higher-Order Functions

In functional programming languages, functions are typically treated
as "*first-class citizens*". That is, functions may take other
functions as arguments and may again produce functions as return
values. A function that takes another function as argument is called a
*higher-order function*.

Higher-order functions provide a powerful mechanism for abstracting
over common computation patterns in programs. This mechanism is
particularly useful for designing libraries with rich interfaces
that support callbacks to client code. We will study these mechanisms
using the example of Scheme's list data type.

As a warm-up, suppose that we want to write a function `sum_ints` that
takes the bounds `a` and `b` of a (half-open) interval `[a,b)` of
integer numbers and computes the sum of the values in that
interval. For example, `sum_ints 1 4` should yield `6`. The following
recursive implementation does what we want:

```ocaml
let rec sum_ints a b =
  if a < b 
  then a + sum_ints (a + 1) b
  else 0
```

Now, consider the following function `sum_squrs` that computes
the sum of the squares of the numbers in an interval `[a,b)`:

```ocaml
let rec sum_squrs a b =
  if a < b 
  then a * a + sum_squrs (a + 1) b
  else 0
```

The functions `sum_ints` and `sum_squrs` are almost identical. They only
differ in the summand that is added in each recursive call. In the
case of `sum_ints` it is `a`, and in the case of `sum_squrs`, it is
`a * a` (i.e. `a` squared). We can write a higher-order function `sum`
that abstracts from these differences. The function `sum` takes
another function `f` as additional parameter. The function `f`
captures the computation that is performed in the summand:

```ocaml
let rec sum f a b =
  if a < b 
  then f a + sum (a + 1) b
  else 0
```

```ocaml
let square a = a * a
let sum_squrs a b = sum square a b
```

Instead of defining the function `square` explicitly, we can also
provide it to `sum` as an anonymous function given by a lambda
abstraction:

```ocaml
let sum_ints a b = sum (fun a -> a) a b
let sum_squrs a b = sum (fun a -> a * a) a b
```

### Curried Functions

Reconsider our definition of `sum_ints` and `sum_squrs` in
terms of `sum`:

```ocaml
let sum_ints a b = sum (fun a -> a) a b
let sum_squrs a b = sum (fun a -> a * a) a b
```

One annoyance with these definitions is that we have to redeclare the
parameters `a` and `b` which are simply passed to `sum`. To simplify
the definition further, we can take advantage of the fact that `sum`
is a curried function. That is, instead of providing all parameters to
`sum` at once, we simply partially apply `sum` and provide only the
function `f` at the time when we define `sum_ints` and
`sum_squrs`. The remaining parameters `a` and `b` are then provided
when `sum_ints` respectively `sum_squrs` are called.

```ocaml
let sum_ints = sum (fun a -> a)
let sum_squrs = sum (fun a -> a * a)
```

### Higher-Order Functions on Lists

A common use case of higher-order functions is to realize
callbacks to client code from within library functions. We discuss
this scenario using list-manipulating functions.


In the earlier examples we saw that functions operating on lists
follow a common pattern: they traverse the list, decomposing it into
its elements, and then apply some operation to each of the
elements. We can extract these common patterns and implement them in
more general higher-order functions that abstract from the specific
operations being performed on the elements.

A particularly common operation on lists is to traverse a list and
apply some function to each element, obtaining a new list. For
example, suppose we have a list of numbers that we want to scale by a
given factor to obtain a list of scaled values. The following function
implements this operation:

```ocaml
let rec scale factor = function
  | [] -> []
  | x :: xs -> factor * x :: scale factor xs
```

A similar operation is implemented by the following function, which
takes a list of numbers and increments each element to obtain a new
list:

```ocaml
let rec incr xs = function
  | [] -> []
  | x :: xs -> 1 + x :: incr xs
```

The type of operation that is performed by `scale` and
`incr` is called a `map`. We can implement the map
operation as a higher-order function that abstracts from the concrete
operation that is applied to each element in the list:

```ocaml
let rec map op = function
  | [] -> []
  | x :: xs -> op x :: map op xs
```

The `map` function transforms the input list `xs` by applying an
operation `op` to each element in `xs`. Note that the order of the
elements in the input list is preserved.

We can now redefine `scale` and `incr` as instances of `map`:

```ocaml
let scale factor = map (fun x -> factor * x)
let incr = map (fun x -> 1 + x)
```

Note that for any binary infix operator `op`, OCaml provides the
special syntax `(op)` to turn the operator `op` into a curried
function. That is, `(op)` expands to the function `fun x y -> x op y`. 
We can use this notation to simplify the definitions of `scale` and
`incr` even further:

```ocaml
let scale factor = map (( *) factor)
let incr = map ((+) 1)
```

Please note the space in `( *)` between the opening parenthesis and
the multiplication operator. This is needed because in OCaml the
sequence `(*` opens a (multi-line) comment.

The `List` module of OCaml standard library has a predefined function
`List.map` that behaves exactly like our `map` function:

```ocaml
> List.map ((+) 1) [1; 2; 3]
- : int list = [2; 3; 4]
```

#### Folding Lists

We have seen that we can often identify common patterns in functions
on data structures and implement them in generic higher-order
functions. We can then conveniently reuse these generic functions,
reducing the amount of code we have to write. In this section, we will
look at the most general patterns for performing operations on
lists, namely *fold operations*.

As a motivating example, consider the following function, which
computes the sum of the values stored in a list of integers

```ocaml
let sum_list = function
  | [] -> 0
  | x :: xs -> x + sum_list xs
```

Consider a list `xs` of `n` integer values `x1` to `xn`:

```ocaml
[x1; ...; xn]
```

Then unrolling the recursion of `sum` on `xs` yields the
following computation

```ocaml
x1 + (x2 + (... (xn + 0)...))
```
That is, in the `i`-th recursive call, we add the current head `xi` to
the sum of the values in the current tail. Here, we consider the sum
of an empty list `[]` to be `0`. If we represent this
computation as a tree, this tree looks as follows:

```ocaml
      +
     / \
    x1  +
       / \
      x2 ... 
           \
            +
           / \
          xn  0
```

With this representation, it is easy to see how to generalize from the
specific computation performed by the represented expression. That is,
in the general case, instead of adding the current head to the sum of
the current tail of the list, we apply a generic operation `op` in
each step that combines the current head with the result of the
recursive computation on the tail. Moreover, instead of starting with
the specific initial value `0` for the empty list, we are given an
initial zero value `z`.  The resulting expanded recursive computation
is then represented by the following tree:

```ocaml
      op
     / \
    x1  op
       / \
      x2 ... 
           \
            op
           / \
          xn  z
```
 
or using OCaml syntax, by the expression

```ocaml
op x1 (op x2 (... (op xn z) ...))
```

We refer to this type of computation as a *fold* of the list
because the list is traversed and recursively folded into a single
value. Note that the tree is leaning towards the right. We therefore
refer to this type of fold operation as a *fold-right*. That is,
the recursive computation is performed in right-to-left order of the
values stored in the list.

The following higher-order function implements the fold-right operation:

```ocaml
let rec fold_right op xs z = 
  match xs with
  | [] -> z
  | x :: xs -> op x (fold_right op z xs)
```

We can now redefine `sum_list` in terms of `fold_right`:

```ocaml
let sum_list xs = fold_right (+) xs 0
```

```ocaml
# sum_list [1; 2; 3] ;;
- : int = 6
```

Many of the other functions that we have seen before perform
fold-right operations on lists. In particular, we can define
`concat` using `fold_right` as follows:

```ocaml
let concat xs ys = fold_right (fun z zs -> z :: zs) xs ys
```

or more compactly

```ocaml
let concat = fold_right (fun z zs -> z :: zs)
```

Also the higher-order function `map` is actually just a special case of
a fold-right:

```ocaml
let map op xs = fold_right (fun x ys -> op x :: ys) xs []
```

All the above operations on lists have in common that they combine the
elements in the input list and the result of the recursive computation
in right-to-left order. We can also consider fold operations that
perform the computation in left-to-right order:

```ocaml
op (... (op (op z x1) x2) ...) xn
```

The corresponding computation tree then looks as follows:
```ocaml
        op
       /  \
     ...  xn
     /
    op 
   /  \
  op  x2
 /  \
z   x1
```

Note that the tree is now leaning towards the left and the elements
are combined in left-to-right order. We therefore refer to this type
of computation as a *fold-left*.

The following function implements the fold-left operation on lists:

```ocaml
let rec fold_left op z = function
  | [] -> []
  | x :: xs -> fold_left (op z x) xs
```

Since addition is associative and commutative, we can alternatively
define `sum_list` using `fold_left` instead of `fold_right`:

```ocaml
let sum_list = fold_left (+) 0
```

In fact, this definition of `sum_list` is more efficient than our
previous implementations because `fold_left` is tail-recursive,
whereas our implementation of `fold_right` is not. Usually, only one
of the two types of fold operations can be used to implement a
specific computation on lists if `op` is not commutative and
associative. For example, we can express `reverse` in terms of a
fold-left as follows:

```ocaml
let reverse xs = fold_left (fun xs x -> x :: xs) [] xs
```

If we replaced `foldl` by `foldr` in this definition:

```ocaml
fold_right (fun x xs -> x :: xs) xs []
```

we would not obtain the correct result. The output list would be equal
to the input list.

Again, since the functions `fold_left` and `fold_right` are so incredibly
useful, they are already predefined in the `List` module of the
standard library.

To get a glimpse of the expressive power of these higher-order
functions, consider the following function, which computes the dot
product of two vectors `v1` and `v2` represented as lists of floats.

```ocaml
let dot_prod v1 v2 = List.fold_left (+.) 0. (List.map2 ( *.) v1 v2)
```


```ocaml
# dot_prod [3.; 2.; 1.] [1.; 2.; 3.]
- : float = 10.
```

It is instructive to re-implement this code snippet in a language like
Java to appreciate how much more concise and comprehensive the
implementation with higher-order functions is.

The basic idea of higher-order functions such as `map`, `fold_right`, and
`fold_left` generalizes from lists to other data structures. So you will
find these kinds of functions in most implementations of common data
structures in the standard libraries of functional programming
languages.

### Practicing the use of `fold_left` and `fold_right`

If you have trouble deriving a implementation of a particular function
on lists using `fold_left`s`/`fold_right`, here is a trick of how to go about it.

If you want an implementation that uses `fold_right`, then first write a
non-tail-recursive function of this shape:

```ocaml
let rec your_function xs = 
  match xs with
  | [] -> (* xs is empty *)
  value_your_function_returns_for_empty_list
  | hd :: tl -> (* xs is non-empty *)
    (* solve the problem for tl recursively and store result in res *)
    let res = your_function tl in
    (* compute actual result for whole list from hd and res *)
    work_to_do_in_each_step_using_hd_and_res
```

Then this translates into the following equivalent implementation with
`fold_right`:

```ocaml
let your_function xs = 
  List.fold_right 
    (fun hd res -> work_to_do_in_each_step_using_hd_and_res)
    xs
    value_your_function_returns_for_empty_list
```

Example:

 
```ocaml
let rec sum_list xs =
  match xs with
  | [] -> 0
  | hd :: tl -> 
    let res = sum_list tl in
    hd + res
```

gives:

```ocaml
let sum_list xs = List.fold_right (lambda hd res -> hd + res) xs 0
```

For using fold_left, you first need a tail-recursive implementation of this shape:

```ocaml
let your_function xs = 
  let rec your_function_helper xs res =
    match xs with
    | [] -> (* list is empty *)
      res
    | hd :: tl -> (* list is non-empty *)
      let res1 = work_to_do_in_each_step_using_hd_and_res in
      (* call the helper recursively on tl with the updated accumulator value res1 *)
      your_function_helper tl res1
  in
  your_function_helper xs value_your_function_returns_for_empty_list
```
 
Then this leads to an equivalent implementation with `fold_left` like this:

 
```ocaml
let your-function xs = 
  List.fold_left (fun res hd -> work_to_do_in_each_step_using_hd_and_res) 
  value_your_function_returns_for_empty_list 
  xs
```
 
Example:

 
```ocaml
let sum_list xs =
  let rec sum_list_helper xs res =
    match xs with
    | [] -> res
    | hd :: tl -> 
      let res1 = hd + res in
      sum_list_helper tl res1
  in
  sum_list_helper xs 0
```

gives

 
```ocaml
let sum_list xs = List.fold_left (fun res hd -> hd + res) 0 xs
```
 
So if you have trouble coming up with an implementation that uses
`fold_left`/`fold_right`, try to follow this recipe. Once you have
done this on a few examples, it will become natural to write the
versions that use the fold functions directly.
