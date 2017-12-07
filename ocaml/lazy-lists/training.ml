type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function
  LNil -> failwith "lhd"
  | LCons (x, _ ) -> x
;;

let ltl = function
  LNil -> failwith "ltl"
  | LCons (_, xf) -> xf()
;;
let llast x = 
  let rec llistIn x acc = match x with
    LNil -> acc
    | LCons (h, t) -> llistIn (ltl (LCons (h, t))) (lhd (LCons (h, t)))
  in llistIn x 0
;;

let rec toLazyList = function
  [] -> LNil
  | h::t -> LCons(h, function () -> toLazyList t)
;;

let rec sumLazy = function
 LNil -> 0
 | LCons (x, t) -> x + sumLazy (ltl (LCons (x, t)))
;;

let lazyList = LCons (1, function () -> LCons(5, function () -> LCons(10, function() -> LNil)));;

print_int (sumLazy lazyList);;
print_int (llast lazyList);;