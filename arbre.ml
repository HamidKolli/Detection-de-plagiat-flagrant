
type noeud_lex = Lettre of char *bool * arbre_lex
and arbre_lex = noeud_lex  list;;



let ajoute s arbre_lex sec= 
  let n = String.length s in 
  let rec aux index arbre_lex =
    if index = n then arbre_lex else 
      match arbre_lex with
        [] -> 
          [Lettre (s.[index],false,aux (index + 1) [])]
      |x::xs -> match x with 
          Lettre(c,_,ys) -> if c = s.[index] then  
              Lettre(c,sec,(aux (index + 1) ys) )::(xs)
            else 
              x::aux index xs 
  in  aux 0 arbre_lex 
  
let rec construit ls sec= 
  match List.rev ls with 
    [] -> []
  |s::xs -> ajoute s (construit xs sec) sec ;;




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
  let arbre = construit (make_prefix_list s) false in
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
  let arbre = construit (make_prefix_list s1) false in
  let prefix = make_prefix_list s2 in
  let rec aux1 list arbre=
    match list with
      [] -> arbre
    |s::xs -> aux1 xs (ajoute s arbre true)
  in let abr = aux1 prefix arbre in
  let rec aux arbre f ls prec= 
    match arbre with
      [] -> ls
    | x::xs -> match x with
        Lettre (c,false,ys) ->aux xs (if prec then f else (fun x -> x)) (aux ys (fun x -> x) (if prec then (f "")::ls else ls) false) prec
      | Lettre (c,true,ys) -> aux xs (if prec then f else (fun x -> x)) (aux ys (fun x -> (f x)^(String.make 1 c)) ls true) prec
                                
  in List.fold_left (fun x y ->  if (String.length x < String.length y)then y else x) "" (aux abr (fun x -> x)[] false)
      

  
type noeud_lex_compresse = Mot of string *bool * arbre_lex_compresse
and arbre_lex_compresse = noeud_lex_compresse  list;;


let compresse arbre_lex =
  let rec aux f abr = 
    match abr with
      [] -> []
    |x::xs -> match x with
        Lettre(c,_,ys) -> 
          let len = List.length ys in
          if len <> 1 || c = '#' then Mot ((f (String.make 1 c)), false, (aux (fun z -> z) ys))::(aux f xs) 
          else
            (aux (fun z -> (f (String.make 1 c)^(z))) ys)@(aux (fun z ->  z) xs)
  in aux (fun x ->  x) arbre_lex
