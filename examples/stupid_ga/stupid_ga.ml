(* You want to write a GA in less than 100 lines using bit-vectors?
   Here you go... (brain-dead implementation) *)

module type GA_SPEC = sig
  val ngenes : int                         (* Number of genes *)
  val mut_prob : int                       (* Mutation probability in % *)
  val recomb_prob : int                    (* Recombination probability *)
  val evaluate_indiv : Res.Bits.t -> float (* Evaluate Individual *)
end

module Ga (Spec : GA_SPEC) = struct
  open Spec
  module Genes = Res.Bits

  type genes = Genes.t
  type indiv = {mutable genes : genes; mutable fitness : float option}
  type population = indiv array

  let random_bit () = Random.int 2 > 0

  let create_indiv () =
    {genes = Genes.init ngenes (fun _ -> random_bit ()); fitness = None}

  let print_indiv ch indiv =
    let print_genes ch =
      Genes.iter (fun g -> output_char ch (if g then '1' else '0'))
    and fitness =
      match indiv.fitness with None -> "N/A" | Some f -> string_of_float f in
    Printf.fprintf ch "%a -> (%s)" print_genes indiv.genes fitness

  let mutate_indiv indiv =
    let mutate_gene i gene =
      if Random.int 100 < mut_prob then
        Genes.set indiv.genes i
          (if Genes.get indiv.genes i then false else true) in
    Genes.iteri mutate_gene indiv.genes; indiv.fitness <- None

  let evaluate_indiv indiv = match indiv.fitness with
    | Some x -> x
    | None -> let x = Spec.evaluate_indiv indiv.genes in
              indiv.fitness <- Some x; x

  let create_pop size = Array.init size (fun _ -> create_indiv ())
  let mutate_pop = Array.iter mutate_indiv
  let recombine_indiv i1 i2 c =
    Genes.blit i2.genes c i1.genes c (ngenes - c); i1.fitness <- None

  let evaluate_pop pop =
    Array.fold_left (fun acc indiv ->
      if evaluate_indiv indiv < evaluate_indiv acc then indiv
      else acc) pop.(0) pop

  let recombine_pop p =
    let len = Array.length p in
    let recombine i indiv =
      if i + 1 < len && Random.int 100 < recomb_prob then
        let mate = i + Random.int (len - i - 1) + 1 in
        recombine_indiv indiv p.(mate) (Random.int ngenes) in
    Array.iteri recombine p

  let select_pop p =
    let compare a b = match a.fitness, b.fitness with
      | Some af, Some bf -> af >= bf
      | _ -> failwith "select_pop: unevaluated individual!" in
    Sort.array compare p;
    for i = 0 to Array.length p / 2 do p.(i) <- create_indiv () done;
end

module MyGA_Spec = struct
  let ngenes = 20
  let mut_prob = 3
  let recomb_prob = 70

  (* Tries to evolve binary representation of 42 - cool! *)
  let evaluate_indiv genes =
    let sum = ref 0 in
    Res.Bits.iter (fun g -> sum := (!sum lsl 1) + (if g then 1 else 0)) genes;
    let res = float !sum -. float 42 in
    res *. res
end

module MyGA = Ga(MyGA_Spec)
open MyGA

let _ =
  Random.self_init ();
  let p = create_pop 100 in let best = ref p.(0) in
  while best := evaluate_pop p; !best.fitness <> Some 0.0 do
    Printf.printf "best so far: %a\n" print_indiv !best; flush stdout;
    select_pop p; recombine_pop p; mutate_pop p done;
  Printf.printf "The winner is: %a\n" print_indiv !best
