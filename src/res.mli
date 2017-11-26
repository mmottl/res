(*
   RES - Automatically Resizing Contiguous Memory for OCaml

   Copyright (C) 1999-  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** Global module for resizable datastructures and default implementations *)

(** {6 Default strategies} *)

(** Default strategy for resizable datastructures *)
module DefStrat : (Strat.T with type t = float * float * int)
(** [type t] is a triple [(waste, shrink_trig, min_size)], where
    [waste] (default: 1.5) indicates how much the array should
    grow in excess when reallocation is triggered, [shrink_trig]
    (default: 0.5) at which percentage of excess elements it should be
    shrunk and [min_size] (default: 16 elements) is the minimum size
    of the resizable array. *)


module BitDefStrat : (Strat.T with type t = float * float * int)
(** Same as [DefStrat], but the minimum size is 1024 elements (bits). *)


(** {6 Default instantiation of standard resizable datastructures} *)

(** Resizable parameterized array using the default reallocation strategy. *)
module Array : (Pres_intf.T with module Strategy = DefStrat)

(** Resizable float array using the default reallocation strategy. *)
module Floats
  : (Nopres_intf.T with module Strategy = DefStrat and type el = float)

(** Resizable bit vector using the default reallocation strategy. *)
module Bits
  : (Nopres_intf.T with module Strategy = BitDefStrat and type el = bool)

(** Resizable weak array using the default reallocation strategy. *)
module Weak : (Weak_intf.T with module Strategy = DefStrat)

(** Resizable buffer using the default reallocation strategy. *)
module Buffer
  : (Nopres_intf.Buffer with module Strategy = DefStrat and type el = char)


(** {6 Functors for creating standard resizable datastructures from
       strategies} *)

(** Functor that creates resizable parameterized arrays from reallocation
    strategies. *)
module MakeArray : functor (S : Strat.T) ->
  (Pres_intf.T with module Strategy = S)

(** Functor that creates resizable float arrays from reallocation
    strategies. *)
module MakeFloats : functor (S : Strat.T) ->
  (Nopres_intf.T with module Strategy = S and type el = float)

(** Functor that creates resizable bit vectors from reallocation
    strategies. *)
module MakeBits : functor (S : Strat.T) ->
  (Nopres_intf.T with module Strategy = S and type el = bool)

(** Functor that creates resizable weak arrays from reallocation
    strategies. *)
module MakeWeak : functor (S : Strat.T) -> (Weak_intf.T with module Strategy = S)

(** Functor that creates resizable buffers (=string arrays) from
    reallocation strategies. *)
module MakeBuffer : functor (S : Strat.T) ->
  (Nopres_intf.Buffer with module Strategy = S and type el = char)
