(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-  Markus Mottl
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

  val grow : t -> int -> int
  (** [grow strat new_len] @return the new real length of some contiguous
      datastructure using strategy [strat] given new virtual length
      [new_len].  The user should then use this new real length to resize
      the datastructure.

      Be careful, the new (real) length {b must} be larger than the new
      virtual length, otherwise your program will crash!
  *)

  val shrink : t -> real_len : int -> new_len : int -> int
  (** [shrink strat ~real_len ~new_len] @return the new real length
      of a resizable datastructure given its current real length
      [real_len] and its required new virtual length [new_len]
      wrt. strategy [strat].  The user should then use this new real
      length to resize the datastructure.  If [-1] is returned, it is
      not necessary to resize.

      Be careful, the new (real) length {b must} be larger than the new
      (virtual) length, otherwise your program may crash!
  *)
end
