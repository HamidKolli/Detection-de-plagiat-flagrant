Printf.printf "\nGenerer fichier .dot";

Printf.printf "\n\n\n 1 - arbre des suffixes ";
Printf.printf "\n 2 - arbre des suffixes compresse ";
Printf.printf "\nChoisissez le type d'arbre : ";
let choix = read_int () in 
Printf.printf "\nEntrer la premiere chaine: ";
let chaine = read_line () in 
  if choix = 1 then 
    (Dot.dot Dot.dot_arbre_suffixe (Arbre_suffixe.arbreSuffixes (chaine^"#")) chaine ;
    Printf.printf "\nle fichier %s.dot est dans le repertoire ../dot/ \n" chaine)
  else
    Dot.dot Dot.dot_arbre_suffixe_compresse (Arbre_suffixe_compresse.arbreSuffixesCompresse (chaine^"#")) chaine ; 
    Printf.printf "\nle fichier %s.dot est dans le repertoire ../dot/ \n" chaine
    


