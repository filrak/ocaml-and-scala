type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

(* Helper functions *)
let lhd = function
  LNil -> failwith "lhd"
  | LCons (x, _ ) -> x
;;

let ltl = function
  LNil -> failwith "ltl"
  | LCons (_, xf) -> xf()
;;

