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



let rec zip (lA, lB) = match (lA, lB) with 
  (LNil, _ ) -> LNil
  | (_, LNil ) -> LNil
  | (lA, lB) -> LCons ( ( lhd lA, lhd lB), function () -> zip ( ltl lA, ltl lB))
;;

let lazyListA = LCons (1, 
  function () -> LCons(2, 
    function () -> LCons(3, 
      function () -> LCons (4, 
        function () -> LCons (5, 
          function () -> LNil)))))
;;

let lazyListB = LCons ('a', 
  function () -> LCons('b', 
    function () -> LCons('c', 
      function () -> LCons ('d', 
        function () -> LCons ('e', 
          function () -> LNil)))))
;;

zip (lazyListA, lazyListB);;