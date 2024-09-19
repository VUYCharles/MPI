type 'a mot = 'a list ;; 

let rec longueur (mot : 'a mot) : int = match mot with 
  | c::r -> 1 + longueur r 
  | [] -> 0
;;

let rec est_vide (mot : 'a mot) : bool = match mot with 
  | [] -> true 
  | _ -> false 
;;

let rec miroir (mot : 'a mot) : 'a mot = match mot with 
  | c::r -> miroir r @ [c] 
  | [] -> [] 
;;

let concat (a : 'a mot) (b : 'a mot) : 'a mot = 
  a @ b 
;;

let vers_mot (s : string) : 'a mot =  
  let n = String.length s in 
  let retour = ref [] in 
  for i=0 to n-1 do 
    retour := s.[i] :: !retour 
  done;
  !retour 
;;


let rec est_prefixe (pre : 'a mot) (mot : 'a mot): bool = match pre, mot with 
  | c::r, c'::r' -> if c=c' then est_prefixe r r' else false 
  | [] , r -> true 
  | c::r, [] -> false 
;;



let est_suffixe (su : 'a mot) (mot :'a mot) : bool = 
  let n = List.length mot in 
  let bon_i = n - List.length su in 
  let a_verif = ref mot in 
  for i=0 to bon_i -1 do 
    match !a_verif with 
    | c::r -> a_verif := r 
    | [] -> failwith "jamais"
  done;
  est_prefixe su !a_verif ;; 

  

let test = (
  let su = ['b';'a'] in 
  let mot = ['a';'b';'a';'a'] in 
  est_suffixe su mot 

)


let est_facteur (fa : 'a mot) (mot : 'a mot) : bool = 
  if est_prefixe fa mot then true 
  else 
  let rec parcours mot = match mot with 
  | [] -> false 
  | c::r -> if est_prefixe fa r then true else parcours r 
  in 
  parcours mot 
;;


let t = (
  let su = ['b';'c'] in 
  let mot = ['a';'b';'b';'a'] in 
  est_facteur su mot 

)

let est_sous_mot (sm : 'a mot) (mot :'a mot) = 
  let a_verif = ref sm in 
  let rec parcours  mot = match !a_verif, mot with 
  | c::r , c'::r' when c=c' -> ( a_verif := r ;  parcours r' ) 
  | c::r, c'::r' -> parcours r'
  | c::r , [] -> false 
  | [] , _ -> true 
in 
parcours mot 
;;


let t = (
  let su = ['a';'a'] in 
  let mot = ['a';'b';'b';'a'] in 
  est_sous_mot su mot 

)
    

let coupe ( mot : 'a mot) (i : int) : 'a mot * 'a mot = 
    let u , v= ref [], ref [] in 
    let cpt= ref 0 in 
    let rec parcours mot = match mot with 
    | c::r when !cpt < i - 1-> (u := !u  @  [c]; 
                            cpt := !cpt + 1 ; 
                            parcours r )
    | c::r when !cpt = i - 1 -> (u := !u @ [c] ; 
                                v := r )
    | _ ->failwith ""
    in 
    parcours mot; 
    (!u,!v)
  ;;

let _ = (
  let mot = ['a';'b';'b';'a'] in 
  coupe mot 3


)

let prefixe (mot : 'a mot) = 
  let i = List.length mot / 2 in 
  let pref, _ = coupe mot i in 
  pref 
;;

let suffixe (mot : 'a mot) = 
  let i = List.length mot / 2 in 
  let _, suff = coupe mot i in 
  suff
;;



type 'a exp_reg = 
    Lettre of 'a 
  | Union of 'a exp_reg * 'a exp_reg 
  | Concat of 'a exp_reg * 'a exp_reg 
  | Etoile of 'a exp_reg 
;;


let e = Union( Concat ( Lettre('a'), Etoile( Lettre('b') )),Concat (Lettre('b'), Etoile(Lettre('a')) )  )  ;;

let rec test (mot : 'a mot) (e : 'a exp_reg) : bool = match e with 
| Lettre(c) -> if mot = [c] then true else false 
| Union (e', e'') -> test mot e' || test mot e'' 
| Etoile(e') -> (
  (* Premier cas : le epsi est accepté *)
   (est_vide mot || (
    let n = List.length mot in 
    let res = ref false in 
    (* Plus de premier cas : vérifier si m est dans L(e.e#)*)
    res := !res || (test mot e' ) ; 
    for i=1 to (n - 1)  do 
      let pre,suff = coupe mot i in 
      res := ( !res  || ( (test pre e') && (test suff ( Etoile (e')) ) ) )
    done;
    !res 
   )
   ) 
)
| Concat(e', e'') -> (
  let n = List.length mot in 
  let res = ref false in 
  (* cas extrèmes car sinon failwith *)
  let pre, suff = [], mot in 
  res := !res || ( (test pre e') && (test suff e'')) ; 
  let pre,suff = mot, [] in 
  res := !res || ( (test pre e') && (test suff e'')) ; 
  for i=1 to (n - 1)  do 
    let pre,suff = coupe mot i in 
    res := ( !res  || ( (test pre e') && (test suff e'') ) )
  done;
  !res 
  )
;;


let _ = (
  let e = Union( Concat ( Lettre('a'), Etoile( Lettre('b') )),Concat (Lettre('b'), Etoile(Lettre('a')) )  ) in
  let mot = ['a';'b';'b';'b'] in
  test mot e 
)



let rec peut_engendrer_vide (e : 'a exp_reg) : bool = match e with 
| Lettre _ -> false 
| Concat (e',e'') -> peut_engendrer_vide e' && peut_engendrer_vide e'' 
| Union (e',e'') -> peut_engendrer_vide e' || peut_engendrer_vide e'' 
| Etoile(e') -> true 
;;

let retirer_doublon (l :'a list) = 
  let hash = Hashtbl.create 10 in 
  let rec parcours l = match l with 
  | q::r -> ( match Hashtbl.find_opt hash q with 
            | Some _ -> parcours r 
            | None -> (Hashtbl.add hash q 1;  q :: parcours r) )
  | [] -> [] 
in 
parcours l ;; 



let premieres (e : 'a exp_reg ) : 'a list = 
  let retour = ref [] in 
  let rec parcours e = match e with 
  | Lettre c -> retour := c :: !retour 
  | Union(e',e'') -> ( parcours e' ; parcours e'' ) 
  | Concat(e',e'') -> (if peut_engendrer_vide e' then parcours e'' else parcours e'  )
  | Etoile(e') -> parcours e' 
  in
  parcours e ;
  !retour 
;;


let dernieres (e : 'a exp_reg ) : 'a list = 
  let retour = ref [] in 
  let rec parcours e = match e with 
  | Lettre(c) -> retour := c :: !retour 
  | Union(e',e'') -> ( parcours e'; parcours e'')
  | Concat(e',e'') -> ( match e'' with 
        | Etoile(e''') -> ( parcours e' ; parcours e''' )
        | _ -> ( parcours e' ) )
  | Etoile (e') -> parcours e' 
in
parcours e ;
!retour 
;;


let _ = (
  let e = Union( Concat ( Lettre('a'), Etoile( Lettre('b') )),Concat (Lettre('c'), Etoile(Lettre('d')) )  ) in
  dernieres e 


)

  
let facteurs (e : 'a exp_reg) : ('a * 'a) list = 
  let retour = ref [] in 
  let curr = ref 'z' in 
  let tcurr = ref 0 in 
  let rec parcours e = match e with 
  | Lettre(c) -> if !tcurr = 0 then  ( curr := c ; tcurr := 1) else if !tcurr = 1 then retour :=    (!curr,c) :: !retour ; 
  | Union(e',e'') -> ( parcours e'; curr := 'z' ; tcurr := 0 ; parcours e'') (*Parcourir e' puis reset avant de e''*)
  | Concat(e',e'') -> (parcours e'; parcours e'')
  | Etoile(e') -> (let d = premieres e' in let f = dernieres e' in List.iter(fun x ->  List.iter(fun x' -> retour := (x,x') :: !retour ) f  ) d ; 
  (* let l = List.combine d f in List.iter (fun (c,c') -> retour :=  (c,c') :: !retour (*; Printf.printf  "%c,%c" c c' *)) l ;  *)
  parcours ( Concat(e',e'))) 
in 
parcours e ;
let l = retirer_doublon !retour in 
l
;;

let _ = (
  let e = Union( Concat ( Lettre('a'), Etoile( Lettre('b') )),Concat (Lettre('b'), Etoile(Lettre('a')) )  ) in
  facteurs e ;
  let e' = Etoile(Union(Concat(Etoile(Lettre('a')), Lettre('b')), Lettre('c'))) in 
  facteurs e' 



)


