open Random ;; 


type image = int array array ;;


let nouvelle_image (n:int) = 
        Random.self_init();
        let r = Array.make_matrix n n 0 in
        for i=0 to n-1 do 
                for j=0 to n-1 do 
                        r.(i).(j) <- Random.int 2  ; 
                done
        done;
        r

;;


let afficher_image_nb (im : image) = 
        let n = Array.length im in 
        for i=0 to n-1 do 
                for j=0 to n-1 do 
                        if im.(i).(j) = 0 then 
                                Printf.printf(".") 
                        else 
                                Printf.printf("#")
                done ;
                Printf.printf ("\n") ;
        done
;;


let _ = 
        (
        let i = nouvelle_image 5 in 
        afficher_image_nb i 
                )


let id (n:int) (i:int) (j:int) =  
        i*n + j 
;;

type unirtrouver = int array ;; 

let init (n:int) : unirtrouver = 
        Array.init n (fun x -> x) 
;;

let trouver (u:unirtrouver) (i:int) = 
        let rec trouver_cc (parent : int) = 
                if parent <> u.(parent) then 
                        u.(parent) <- trouver_cc u.(parent) ;
                u.(parent) 
        in trouver_cc i 
;;

let unir (u:unirtrouver) (i:int) (j:int) = 
        let ri = trouver u i in 
        let rj = trouver u j in 
        u.(rj) <- ri 
;;


(*
let composantes (img : image) : unirtrouver = 
        let n = Array.length img in 
        let uf = init (n*n) in  
        for i=0 to n-1 do 
                for j=0 to n-2 do 
                        (*vers la droite*)
                        if img.(i).(j) == img.(i).(j+1) then 
                                unir uf (i*n+j) (i*n+j+1) ;
                done
        done;
        for j=0 to n-1 do 
                for i=0 to n-2 do 
                        if img.(i).(j) == img.(i+1).(j) then 
                                unir uf (i*n+1) ((i+1)*n+j) ;
                done
        done;
        uf 
;;
*)



let composantes (img : image) : unirtrouver = 
        let n = Array.length img in 
        let uf = init (n*n) in  
        for i=0 to n-2 do 
                for j=0 to n-2 do 
                        (*vers la droite*)
                        if img.(i).(j) = img.(i).(j+1) then 
                                unir uf (i*n+j) (i*n+j+1) ;
                        if img.(i+1).(j) = img.(i).(j) then 
                                unir uf ((i)*n +j) ((i+1)*n+j)
                done
        done;
        for i=0 to n-2 do (
                if img.(i).(n-1) = img.(i+1).(n-1) then unir uf (i*n+(n-1)) ((i+1)*n+(n-1));
                if img.(n-1).(i) = img.(n-1).(i+1) then unir uf ((n-1)*n + i) ((n-1)*n + i +1) ; )
        done;
        uf
;;


let afficher_composantes (u:unirtrouver) (n:int) = 
        for i = 0 to n-1 do 
                for j=0 to n-1 do 
                        Printf.printf " %d " (trouver u (i*n+j)) ;
                done;
                Printf.printf("\n") ;
        done
;;
        
let _ =(
        let ni = nouvelle_image 5 in 
        afficher_image_nb ni ; 
        let u = composantes ni in 
        afficher_composantes u 5 
)



let recolore (img : image) (u : unirtrouver) : image = 
        let n = Array.length img in 
        (*let retour = Array.make_matrix n n 0 in *)
        let ha = Hashtbl.create 4 in 
        let couleur_courante = ref 0 in 
        for i=0 to n-1 do 
                for j=0 to n-1 do 
                        match Hashtbl.find_opt ha ( trouver u (i*n+j) )  with 
                        | Some couleur -> img.(i).(j) <- couleur 
                        | None -> ( img.(i).(j) <- !couleur_courante ; Hashtbl.add ha (trouver u (i*n+j)) !couleur_courante ; couleur_courante := !couleur_courante + 1 ) 
                done
        done;
        img
;;
                       

let afficher_image (img : image) = 
        let n = Array.length img in 
        for i=0 to (n-1) do 
                for j=0 to (n-1) do 
                        Printf.printf " %c " (Char.chr (97+img.(i).(j)) );
                done;
                Printf.printf("\n");
        done
;;


let _ = (
        let ni = nouvelle_image 5 in 
        afficher_image_nb ni ; 
        let u = composantes ni in 
        afficher_composantes u 5 ;
        let img' = recolore ni u in
        afficher_image img' 
)
