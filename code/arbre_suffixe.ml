
  (*Structure de donnee de l'arbre des suffixes(c'est un arbre lexical)*)
(*char(la lettre)*boolean(si le noeud est commun entre deux chaine)*arbre_lex(les fils du noeud)*)
type noeud_lex = Lettre of char *bool * arbre_lex
and arbre_lex = noeud_lex  list;;

(*Structure de donnee de l'arbre des suffixes*)
(*string(le mot)*arbre_lex_compresse(les fils du noeud)*)  
type noeud_lex_compresse = Mot of string  * arbre_lex_compresse
and arbre_lex_compresse = noeud_lex_compresse  list;;


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
    |s::xs -> ajoute s (construit xs ) 

  let arbreSuffixes s = construit (Chaine.makeSuffixList s)
      
  let sousChainesCommunes s1 s2 = 
    let arbre = arbreSuffixes s1 in
    let prefix = Chaine.makeSuffixList s2 in
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

