let dot func arbre file_name= 
  let file = open_out ("../dot/"^file_name^".dot") in
    Printf.fprintf file "graph\n";
    Printf.fprintf file "{\n";
    Printf.fprintf file "0[label=\"\"];\n";
    func arbre 1 file 0;
    Printf.fprintf file "}\n";
    close_out file;;


let rec dot_arbre_suffixe_compresse arbre_compresse id file racine = 
  match arbre_compresse with
    [] -> id
    |x :: xs -> 
      match x with 
      Arbre_suffixe.Mot (m,ys) -> 
              Printf.fprintf file "%d[label=\"%s\"];\n" id m;
              Printf.fprintf file "%d -- %d\n" racine id;
              let idfils = dot_arbre_suffixe_compresse xs (id+1) file racine in
                dot_arbre_suffixe_compresse ys idfils file id;;

let rec dot_arbre_suffixe arbre id file racine = 
  match arbre with
    [] -> id
    |x :: xs -> 
      match x with 
      Arbre_suffixe.Lettre (m,_,ys) -> 
              Printf.fprintf file "%d[label=\"%c\"];\n" id m;
              Printf.fprintf file "%d -- %d\n" racine id;
              let idfils = dot_arbre_suffixe xs (id+1) file racine in
                dot_arbre_suffixe ys idfils file id;;





