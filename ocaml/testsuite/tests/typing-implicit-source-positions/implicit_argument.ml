(* TEST
   * expect
*)

let f = fun ~(src_pos:[%src_pos]) () -> src_pos
[%%expect{|
val f : src_pos:lexing_position -> unit -> lexing_position = <fun>
|}]

let _ = f ();;
[%%expect{|
- : lexing_position =
{pos_fname = ""; pos_lnum = 1; pos_bol = 156; pos_cnum = 164}
|}]
