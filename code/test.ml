
exception Test_fail of string

let equals a b = b = a

let test f fun_name =
  Printf.printf "\nEntrer la premiere chaine: ";
  let s1 = read_line () in
  Printf.printf "\nEntrer la deuxieme chaine: ";
  let s2 = read_line () in
  Printf.printf "\nEntrer le resultat: ";
  let result = read_line ()
  in 
    if equals (f s1 s2) result then Printf.printf "fonction %s est correcte\n" fun_name  else raise (Test_fail(fun_name))