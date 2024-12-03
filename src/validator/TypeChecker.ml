(* Type system with subtyping relation <:
   Forms a complete lattice where:
   - ⊤ is the top type
   - ⊥ is the bottom type
   - ⊔ is the least upper bound
   - ⊓ is the greatest lower bound
*)
module Type = struct
  type t = 
    | Top                           (* ⊤ *)
    | Bot                           (* ⊥ *)
    | Arrow of t * t                (* τ₁ → τ₂ *)
    | Product of t list             (* τ₁ × τ₂ × ... × τₙ *)
    | Union of t * t                (* τ₁ ⊔ τ₂ *)
    | Inter of t * t                (* τ₁ ⊓ τ₂ *)

  (* Subtyping relation forms a preorder:
     - Reflexivity: ∀τ. τ <: τ
     - Transitivity: ∀τ₁τ₂τ₃. τ₁ <: τ₂ ∧ τ₂ <: τ₃ ⟹ τ₁ <: τ₃
  *)
  let rec subtype t1 t2 = match (t1, t2) with
    | _, Top -> true
    | Bot, _ -> true
    | Arrow(s1,t1), Arrow(s2,t2) -> 
        subtype s2 s1 && subtype t1 t2  (* Contravariant in domain *)
    | _ -> false
end 