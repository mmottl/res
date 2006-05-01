(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-2002  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* $Id: strat.mli,v 1.6 2005/11/07 20:25:28 mottl Exp $ *)

(** Interface to strategies *)
module type T = sig
  type t (** The abstract type of strategies. *)

  val default : t
  (** Default strategy of this strategy implementation. *)

  val grow : t -> (int -> unit) -> int -> unit
  (** [grow strat resizer new_len] grows some contiguous
      datastructure using strategy [strat] to a new (virtual) length
      [new_len] by calling its resizer function with its new (real)
      length. Be careful, the new (real) length {b must} be larger than
      the new (virtual) length, otherwise your program will crash! *)

  val shrink : t -> (int -> unit) -> int -> int -> unit
  (** [shrink strat resizer real_len new_len] (possibly) shrinks
      some contiguous datastructure of length [real_len] depending
      on its demanded new (virtual) length [new_len] by calling its
      resizer function with its new (real) length. Be careful, the new
      (real) length {b must} be larger than the new (virtual) length,
      otherwise your program will crash! *)
end
