(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary AST types used by parsetree and typedtree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type global_flag =
  | Global
  | Nothing

(* constant layouts are parsed as layout annotations, and also used
   in the type checker as already-inferred (i.e. non-variable) layouts *)
type const_layout =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate

type label = string

type arg_label =
    Nolabel
  | Labelled of string (** [label:T -> ...] *)
  | Optional of string (** [?label:T -> ...] *)
  | Position of string (** [label:[%src_pos] -> ...]*)
  (* CR src_pos: We'll have to revert this change eventually to remain compatible with
     ppx. Instead, add types for labels in the Parsetree and Typedtree *)

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}


type variance =
  | Covariant
  | Contravariant
  | NoVariance

type injectivity =
  | Injective
  | NoInjectivity
