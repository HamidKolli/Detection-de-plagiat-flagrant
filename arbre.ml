type noeud_lex = Lettre of char *bool * arbre_lex
and arbre_lex = noeud_lex  list;;



let ajoute s arbre_lex= 
  let n = String.length s in 
  let rec aux index arbre_lex =
    if index = n then arbre_lex else 
      match arbre_lex with
        [] -> 
          [Lettre (s.[index],false,aux (index + 1) [])]
      |x::xs -> match x with 
          Lettre(c,b,ys) -> if c = s.[index] then
              Lettre(c,b,(aux (index + 1) ys) )::(xs)
            else 
              x::aux index xs 
  in  aux 0 arbre_lex 
  
    
let ajoute2 s arbre_lex = 
  let n = String.length s in 
  let rec aux index arbre_lex =
    if index = n then arbre_lex else 
      match arbre_lex with
        [] -> 
          []
      |x::xs -> match x with 
          Lettre(c,_,ys) -> if c = s.[index]  then
              if  c = '#' then
                Lettre(c,true,[] )::(xs)
              else
                Lettre(c,true,(aux (index + 1) ys) )::(xs)
            else 
              x::aux index xs 
  in  aux 0 arbre_lex 

let rec construit ls = 
  match List.rev ls with 
    [] -> []
  |s::xs -> ajoute s (construit xs )  ;;




let  get_prefix_from s n =
  let len = String.length s in 
  if n < 0 then "" else
    let rec aux  stmp i =  
      if len <= i then
        stmp
      else
        aux  (stmp^(String.make 1 (s.[i]))) (i+1)
      
    in aux  "" n
      
      

      
let make_prefix_list s = 
  let len = String.length s in 
  let rec aux stmp i =  
    if len <= i then
      stmp
    else
      aux  ((get_prefix_from s i)::stmp) (i+1)
      
  in aux  [] 0

    

let sousChaine s ss = 
  let arbre = construit (make_prefix_list s)  in
  let len = String.length ss in 
  let rec aux index abr =
    if index >= len then true else
      match abr with
        [] -> false
      |x::xs -> 
          match x with
            Lettre(c,_,ys) -> if c = ss.[index] then aux (index + 1) ys
              else if c = '#' then false else aux index xs
  in aux 0 arbre



let sousChainesCommunes s1 s2= 
  let arbre = construit (make_prefix_list s1) in
  let prefix = make_prefix_list s2 in
  let rec aux1 list arbre=
    match list with
      [] -> arbre 
    |s::xs -> aux1 xs (ajoute2 s arbre)
  in let abr = aux1 prefix arbre in
  let rec aux arbre f ls prec= 
    match arbre with
      [] -> ls
    | x::xs -> match x with
        Lettre (c,false,ys) ->aux xs (if prec then f else (fun x -> x)) (aux ys (fun x -> x) (if prec then (f "")::ls else ls) false) prec
      | Lettre (c,true,ys) -> aux xs (if prec then f else (fun x -> x)) (aux ys (fun x -> (f x)^(String.make 1 c)) ls true) prec
                                
  in List.fold_left (fun x y ->  if (String.length x < String.length y)then y else x) "" (aux abr (fun x -> x)[] false)
    

  
type noeud_lex_compresse = Mot of string  * arbre_lex_compresse
and arbre_lex_compresse = noeud_lex_compresse  list;;


let compresse arbre_lex =
  let rec aux f abr = 
    match abr with
      [] -> []
    |x::xs -> match x with
        Lettre(c,_,ys) -> 
          let len = List.length ys in
          if len <> 1 || c = '#' then Mot ((f (String.make 1 c)), (aux (fun z -> z) ys))::(aux f xs) 
          else
            (aux (fun z -> (f (String.make 1 c)^(z))) ys)@(aux (fun z ->  z) xs)
  in aux (fun x ->  x) arbre_lex


let sousChainesCommunesCompresse s1 s2 = 
  let arbre = compresse (construit (make_prefix_list s1) ) in 
  let rec aux abr index f list_ss= 
    match abr with
      [] -> list_ss
    |x::xs -> match x with
        Mot (m,ys) ->  let rec aux2  i im f = 
                         if i >= String.length s2 then let str = (f "") in if str = "" then [] else [(f "")] else
                         if im >= String.length m then aux ys i f list_ss else
                         if m.[im] = s2.[i] then aux2  (i+1) (im + 1) (fun x -> (f "")^(String.make 1 m.[im])^x) else
                           let str = (f "") in if str = "" then aux2  (i+1) (im) (fun x -> x) else str::aux2  (i+1) (im) (fun x -> x)
          in aux xs index f ((aux2  index 0 f)@list_ss)
  in  List.fold_left (fun x y ->  if ((String.length x) < (String.length y)) then y else x) "" (aux arbre 0 (fun x -> x) []);;


let arbreSuffixesCompresse s = 
  let list_pref = make_prefix_list s in 
  let ls = String.length s in
  let rec ajoute arbre_lex s deja_ajoute=
    match arbre_lex with
      [] -> if deja_ajoute then [] else [Mot (s, [])]
    |x::xs -> match x with
        Mot(m,ys) -> 
          let lm = String.length m in
          let rec aux i f deja_ajoute= 
            if i >= ls then 
              (Mot(m,ys),true) 
            else if i >= lm then 
              if ys = [] then 
                (Mot(m^((get_prefix_from s i)),ys),true)
              else 
                (Mot(m,ajoute ys (get_prefix_from s i) false),true) 
            else if m.[i] = s.[i] then 
              aux (i+1)  (fun x -> (f "")^(String.make 1 m.[i])^x) deja_ajoute
            else if i <> 0 then 
              (Mot(f "",[Mot((get_prefix_from m i),ys);Mot((get_prefix_from s i),[])]),true)
            else 
              (Mot(m,ys),deja_ajoute) 
          in match aux 0 (fun x -> x) deja_ajoute with
            (m,dte) -> m::ajoute xs s dte 
  in List.fold_left (fun x y -> ajoute x y false) [] list_pref;;

  
  
type sens = Gauche | Haut| Diagonale| Aucun;;  
type case = Case of sens * int;;

let sousChaineCommunesDynamique s1 s2 =
  let n = String.length s1 in
  let m = String.length s2 in
  let mat = Array.make_matrix (n+1) (m+1) (Case(Aucun,(-1))) in
  for i = 0 to n do
    mat.(i).(0) <- Case(Aucun,0);
  done;
  for i = 0 to m do
    mat.(0).(i) <- Case(Aucun,0);
  done;
  
  let rec calcul_case i j =
    (if i = 0 || j = 0 then ()
     else if s1.[i-1] = s2.[j-1] then 
       mat.(i).(j) <- (match mat.(i-1).(j-1) with 
             Case(_,k) -> 
               if k <> -1 then 
                 Case(Diagonale,k + 1)
               else 
                 let k = (calcul_case (i-1) (j-1)) +1 in  Case(Diagonale,k)) 
     else 
       mat.(i).(j) <- (match mat.(i).(j-1) with 
             Case(_,k1) -> match  mat.(i-1).(j) with
               Case(_,k2) -> let k1 = (if k1 = (-1) then  (calcul_case (i) (j-1)) else k1) in 
                 let k2 = (if k2 = (-1) then  (calcul_case (i-1) (j)) else k2) in 
                 if k1 < k2 then 
                   Case (Gauche,k2)
                 else
                   Case (Haut,k1)));
    match mat.(i).(j) with 
      Case(_,k)->k
        
  in calcul_case (n) (m);

  let rec sousChainesAux i j f listSousChaine prec taille index1 index2 =
    if i = 0  || j = 0 then 
      if prec then ((f ""),index1,index2,taille)::listSousChaine else listSousChaine 
    else match mat.(i).(j) with 
        Case(s,nb) -> match s with
          Aucun -> (if prec then (((f ""),(i-1),(j-1),taille)::listSousChaine) else listSousChaine)
        |Gauche -> sousChainesAux (i-1) (j) (fun x -> x)  (if prec then (((f ""),index1,index2,taille)::listSousChaine) else listSousChaine) false 0 index1 index2
        |Haut -> sousChainesAux (i) (j-1) (fun x -> x)  (if prec then (((f ""),index1,index2,taille)::listSousChaine) else listSousChaine) false 0 index1 index2
        |Diagonale -> 
            sousChainesAux (i-1) (j-1) (fun x -> x^String.make 1 s1.[i-1]^(f "")) listSousChaine true ( if prec then taille else nb) (i-1) (j-1)
                
  
  in sousChainesAux n m (fun x -> x) [] false 0 0 0;;

      
    
sousChaineCommunesDynamique "ananas" "banane";;
sousChaineCommunesDynamique "hamid" "akamiaka";;




  
arbreSuffixesCompresse "ananas#";;
make_prefix_list "ananas#";; 
sousChainesCommunesCompresse "ananas#" "banane#";;
sousChainesCommunesCompresse "hamid#" "akakmimimi#";; 
sousChainesCommunes "hamid#" "akakmimimi#";;
compresse(construit (make_prefix_list "ababababababababa") );;


      