(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
type t_any : any;;

[%%expect{|
success
|}]

type t_void : void;;

[%%expect{|
failure
|}]

(***************************************)
(* Test 1: annotation on type variable *)

let x : (int : value) = 5
let x : (int : immediate) = 5
let x : (int : any) = 5;;

[%%expect{|
success
|}]

(****************************************)
(* Test 2: Annotation on type parameter *)

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any) -> 'a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : ('a : any). 'a -> 'a = fun x -> x
;;
[%%expect {|
Line 1, characters 8-28:
1 | let f : ('a : any). 'a -> 'a = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout any, but was inferred to have layout value.
|}]
(* CR layouts (v2.5): This error message should change to complain
   about the [fun x], not the arrow type. *)

(********************************************)
(* Test 4: Annotation on record field types *)

type r = { field : ('a : immediate). 'a -> 'a }
let f { field } = field 5
;;
[%%expect {|
type r = { field : ('a : immediate). 'a -> 'a; }
val f : r -> int = <fun>
|}]

let f { field } = field "hello"
;;
[%%expect {|
Line 1, characters 24-31:
1 | let f { field } = field "hello"
                            ^^^^^^^
Error: This expression has type string but an expression was expected of type
         'a
       string has layout value, which is not a sublayout of immediate.
|}]

let r = { field = fun x -> x }
let r = { field = Fun.id }
;;
[%%expect {|
val r : r = {field = <fun>}
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : immediate)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
|}]

let r = { field = fun (type (a : value)) (x : a) -> x }
;;
[%%expect {|
val r : r = {field = <fun>}
|}]

(********************)
(* Test 5: newtypes *)

let f = fun (type (a : value)) (x : a) -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f = fun (type (a : immediate)) (x : a) -> x
;;
[%%expect {|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
Line 1, characters 29-36:
1 | let f = fun (type (a : any)) (x : a) -> x
                                 ^^^^^^^
Error: This pattern matches values of type a
       but a pattern was expected which matches values of type 'a
       a has layout any, which is not a sublayout of value.
|}]

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value). a -> a = fun x -> x
;;
[%%expect {|
val f : 'a -> 'a = <fun>
|}]

let f : type (a : immediate). a -> a = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f : type (a : any). a -> a = fun x -> x
;;
[%%expect {|
Line 1, characters 4-43:
1 | let f : type (a : any). a -> a = fun x -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout any, but was inferred to have layout value.
|}]
(* CR layouts v2.5: This error message will change to complain
   about the fun x, not the arrow type. *)

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-46:
2 |   val f : ('a : value). 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout value, but was inferred to have layout immediate.
|}]

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm
  val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
module type S =
  sig
    val f : ('a : immediate). 'a t2_imm -> 'a t2_imm
    val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
  end
|}]

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : immediate). 'a -> 'a) = x "string"

[%%expect {|
Line 1, characters 43-51:
1 | let f (x : ('a : immediate). 'a -> 'a) = x "string"
                                               ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         'a
       string has layout value, which is not a sublayout of immediate.
|}]

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate) (x : a) = x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type a : immediate) (x : a) -> x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun (type a : value) (x : a) -> x

[%%expect{|
val f : 'a -> 'a = <fun>
|}]

let o = object
  method m : type (a : immediate). a -> a = fun x -> x
end

[%%expect{|
val o : < m : ('a : immediate). 'a -> 'a > = <obj>
|}]

let f : type (a : immediate). a -> a = fun x -> x

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f x =
  let local_ g (type a : immediate) (x : a) = x in
  g x [@nontail]

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f = fun x y (type (a : immediate)) (z : a) -> z

[%%expect{|
val f : ('a : immediate) 'c 'b. 'b -> 'c -> 'a -> 'a = <fun>
|}]

let f = fun x y (type a : immediate) (z : a) -> z

[%%expect{|
val f : ('a : immediate) 'c 'b. 'b -> 'c -> 'a -> 'a = <fun>
|}]

external f : ('a : immediate). 'a -> 'a = "%identity"

[%%expect{|
external f : ('a : immediate). 'a -> 'a = "%identity"
|}]


type (_ : any) t2_any
exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn

[%%expect{|
type (_ : any) t2_any
exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn
|}]


let f (x : ('a : immediate). 'a -> 'a) = x 3, x true

[%%expect{|
val f : (('a : immediate). 'a -> 'a) -> int * bool = <fun>
|}]

type _ a = Mk : [> ] * ('a : immediate) -> int a

[%%expect {|
type _ a = Mk : ('a : immediate). [>  ] * 'a -> int a
|}]

let f_imm : ('a : immediate). 'a -> 'a = fun x -> x

[%%expect {|
val f_imm : ('a : immediate). 'a -> 'a = <fun>
|}]

let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x

[%%expect {|
Line 1, characters 37-53:
1 | let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x
                                         ^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       'a has layout value, which is not a sublayout of immediate.
|}]

type (_ : value) g =
  | MkG : ('a : immediate). 'a g

[%%expect {|
type _ g = MkG : ('a : immediate). 'a g
|}]
