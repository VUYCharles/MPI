type unirtrouver =  int array ;; (* Array of int ;; *)

let init (n : int) : unirtrouver = 
        Array.init n (fun x ->x) ;; 

let trouver (u : unirtrouver) (i : int) = 
        let rec trouver_cc (parent : int) = 
                if parent <> u.(parent) then 
                        (u.(parent) <- trouver_cc(u.(parent));); 
                parent 
        in 
        trouver_cc i;
;;


let rec trouver_cc (u:unirtrouver) (i:int) = 
        if u.(i) <> i then 
                u.(i) <- trouver_cc u u.(i) ; 
        u.(i) 
;;
(*
let trouver (u:unirtrouver) (i:int) = 
        let rec t (parent:int) = 
                if parent <> u.(parent) then 
                        t u.(parent) 
                else 
                        parent 
        in t 
;;
*)
let unir (uf : unirtrouver) (i:int) (j:int) = 
        let ri = trouver_cc uf i in 
        let rj = trouver_cc uf j in 
        uf.(rj) <- ri 
;;
