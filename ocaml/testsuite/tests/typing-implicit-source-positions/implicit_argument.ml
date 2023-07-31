(* TEST
   * expect
*)

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val f : src_pos:[%src_pos] -> unit -> lexing_position = <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 151; pos_cnum = 159}
|}]

let g = fun ~(a:[%src_pos]) ?(c = 0) ~(b:[%src_pos]) () -> ()
[%%expect{|
val g : a:[%src_pos] -> ?c:int -> b:[%src_pos] -> unit -> unit = <fun>
|}]

let _ = g ~a:Lexing.dummy_pos () ;;
[%%expect{|
- : unit = ()
|}]
