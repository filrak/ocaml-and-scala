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

(* For test purposes *)
let rec llen = function
LNil -> 0
| LCons (x, t) -> 1 + llen (ltl (LCons (x, t)))
;;

let lrepeat lazyList x = 
  let rec lrepeatIn lazyList x counter = match lazyList with
  LNil -> LNil
  | LCons (h, tl) -> if counter < x then
      LCons (lhd lazyList, 
        function () -> lrepeatIn (lazyList) x (counter + 1) )
    else
      LCons (lhd lazyList, 
        function () -> lrepeatIn (ltl lazyList) x 1)
    in lrepeatIn lazyList x 1
;;

let lazyList = LCons (1, 
function () -> LCons(2, 
  function () -> LCons(3, 
    function () -> LCons (4, 
      function () -> LCons (5, 
        function () -> LNil)))))
;;

let lazyListRepeated = lrepeat lazyList 2;;
print_int (llen lazyListRepeated);;