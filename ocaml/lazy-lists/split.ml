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

let rec llen = function
  LNil -> 0
  | LCons (x, t) -> 1 + llen (ltl (LCons (x, t)))
;;



(* Assumption: index begins from 0 *)
let splitLazy toSplit = 
  let listLen = llen toSplit in 
  let rec splitEven toSplit counter = 
    if counter == listLen then
     LNil
    else 
      (
        if counter mod 2 == 0 then
          LCons ( lhd toSplit, function () -> splitEven (ltl toSplit) (counter + 1))
        else
          splitEven (ltl toSplit) (counter + 1)
      ) 
    in let rec splitOdd toSplit counter = 
    if counter == listLen then
     LNil
    else 
      (
        if counter mod 2 == 0 then
          splitOdd (ltl toSplit) (counter + 1)
        else
          LCons ( lhd toSplit, function () -> splitOdd (ltl toSplit) (counter + 1))        
      ) 
    in (splitOdd toSplit 0, splitEven toSplit 0)
;;


let lazyList = LCons (5, 
    function () -> LCons(6, 
      function () -> LCons(3, 
        function () -> LCons (2, 
          function () -> LCons (1, 
            function () -> LNil)))))
;;

splitLazy lazyList;;
