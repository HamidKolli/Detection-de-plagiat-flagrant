
let rec sousChainesCommunesCompresseAux s2 abr index f list_ss = 
  match abr with
    [] -> list_ss
  |x::xs -> match x with
  Arbre_suffixe.Mot (m,ys) ->  let rec aux2  i im f lists= 
                      if i >= String.length s2 then   let str = (f "") in if str = "" then lists else str::lists else
                      if im >= String.length m then  sousChainesCommunesCompresseAux s2 ys i f list_ss else
                      if m.[im] = s2.[i] then aux2  (i+1) (im + 1) (fun x -> (f (String.make 1 m.[im]))^x) lists else
                        let str = (f "") in if str = "" then aux2  (i+1) (im) (fun x -> x) lists else aux2  (i+1) (im) (fun x -> x) (str::lists)
        in sousChainesCommunesCompresseAux s2 xs index f (aux2  index 0 f  list_ss)


let sousChainesCommunesCompresse s1 s2 = 
  let arbre = Arbre_suffixe.compresse (Arbre_suffixe.arbreSuffixes s1)  
  in  List.fold_left (fun x y ->  if ((String.length x) < (String.length y)) then y else x) "" (sousChainesCommunesCompresseAux s2 arbre 0 (fun x -> x) [])


let arbreSuffixesCompresse s = 
  let list_pref = Chaine.makeSuffixList s in 
    let ls = String.length s in
      let rec ajoute arbre_lex s deja_ajoute=
        match arbre_lex with
          [] -> if deja_ajoute then [] else [Arbre_suffixe.Mot (s, [])]
        |x::xs -> match x with
        Arbre_suffixe.Mot(m,ys) -> 
              let lm = String.length m in
              let rec aux i f deja_ajoute = 
                if i >= ls then 
                  (Arbre_suffixe.Mot(m,ys),true) 
                else if i >= lm then 
                  if ys = [] then 
                    (Arbre_suffixe.Mot(m^((Chaine.getSuffixFrom s i)),ys),true)
                  else 
                    (Arbre_suffixe.Mot(m,ajoute ys (Chaine.getSuffixFrom s i) false),true) 
                else if m.[i] = s.[i] then 
                  aux (i+1)  (fun x -> (f "")^(String.make 1 m.[i])^x) deja_ajoute
                else if i <> 0 then 
                  (Arbre_suffixe.Mot(f "",[Arbre_suffixe.Mot((Chaine.getSuffixFrom m i),ys);Arbre_suffixe.Mot((Chaine.getSuffixFrom s i),[])]),true)
                else 
                  (Arbre_suffixe.Mot(m,ys),deja_ajoute) 
        in match aux 0 (fun x -> x) deja_ajoute with
          (m,dte) -> m::ajoute xs s dte 
  in List.fold_left (fun x y -> ajoute x y false) [] list_pref

let sousChainesCommunesCompresse2 s1 s2 = 
  let arbre = arbreSuffixesCompresse s1  
  in  List.fold_left (fun x y ->  if ((String.length x) < (String.length y)) then y else x) "" (sousChainesCommunesCompresseAux s2 arbre 0 (fun x -> x) [])


