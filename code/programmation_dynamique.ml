
(* Partie 1 Programmation dynamique  *)
(* La structure *)
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
      (*Fonction qui calcule les distances et mis a jour les sens (direction)
    int(indice1) -> int(indice2)-> int(taille d'une sous chaine)   
      *)
      let rec calculCase i j =
        (if i = 0 || j = 0 then ()
        else if s1.[i-1] = s2.[j-1] then 
          mat.(i).(j) <- (match mat.(i-1).(j-1) with 
                Case(_,k) -> 
                  if k <> -1 then 
                    Case(Diagonale,k + 1)
                  else 
                    let k = (calculCase (i-1) (j-1)) +1 in  Case(Diagonale,k)) 
        else 
          mat.(i).(j) <- (match mat.(i).(j-1) with 
                Case(_,k1) -> match  mat.(i-1).(j) with
                  Case(_,k2) -> let k1 = (if k1 = (-1) then  (calculCase (i) (j-1)) else k1) in 
                    let k2 = (if k2 = (-1) then  (calculCase (i-1) (j)) else k2) in 
                    if k1 < k2 then 
                      Case (Gauche,k2)
                    else
                      Case (Haut,k1)));
        match mat.(i).(j) with 
          Case(_,k)->k
      (*Warning : on a pas besion de la donne evaluer a la fin mais on a besoin d'elle pour les appels recursives*) 
      in calculCase (n) (m);
    (* Fonction qui construit les sous chaines en partant de la derniere case de la matrice et en remontant tout en suivant les sens des cases

      int(indice1) -> int(indice2)->(string->string)(une fonction qui construit la chaine)->bool(pour savoir si la case precedente fait partie de la chaine)
      ->int(son indice dans s1)->int(son indice dans s2)->(string,int,int,int) list
      *)
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
                    
      
      in List.fold_left (fun x y -> 
        match x with
          (s,_,_,_) -> match y with
            (s2,_,_,_) -> if String.length s < String.length s2 then y else x 
      )  ("",0,0,0) (sousChainesAux n m (fun x -> x) [] false 0 0 0)



  let sousChaineCommunesDynamique2 s1 s2= 
    match sousChaineCommunesDynamique s1 s2 with
      (s,_,_,_) -> s

