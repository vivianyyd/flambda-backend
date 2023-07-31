(* TEST
   * expect
*)

let f = fun ~(a:[%src_pos]) ~(b:[%src_pos]) () -> ()
[%%expect{|
val f : a:[%src_pos] -> b:[%src_pos] -> unit -> unit = <fun>
|}]

let _ = f ~b:Lexing.dummy_pos ~a:Lexing.dummy_pos () ;;
[%%expect{|
- : unit = ()
|}]

let g = fun ~(a:[%src_pos]) ?(c = 0) ~(b:[%src_pos]) () -> ()
[%%expect{|
val g : a:[%src_pos] -> ?c:int -> b:[%src_pos] -> unit -> unit = <fun>
|}]

let _ = g ~b:Lexing.dummy_pos ~a:Lexing.dummy_pos () ;;
[%%expect{|
- : unit = ()
|}]

let k = fun ~(a:[%src_pos]) ~(a:int) () -> ()
[%%expect{|
val k : a:[%src_pos] -> a:int -> unit -> unit = <fun>
|}]

let _ = k ~a:Lexing.dummy_pos ~a:0 ();;
[%%expect{|
- : unit = ()
|}]

let _ = k ~a:0 ~a:Lexing.dummy_pos ();;
[%%expect{|
Line 1, characters 13-14:
1 | let _ = k ~a:0 ~a:Lexing.dummy_pos ();;
                 ^
Error: This expression has type int but an expression was expected of type
         lexing_position
|}]

let m ~(x:[%src_pos]) () = ();;

let _ = m Lexing.dummy_pos ();;
[%%expect {|
val m : x:[%src_pos] -> unit -> unit = <fun>
Line 3, characters 27-29:
3 | let _ = m Lexing.dummy_pos ();;
                               ^^
Error: The function applied to this argument has type x:[%src_pos] -> unit
This argument cannot be applied without label
|}]
