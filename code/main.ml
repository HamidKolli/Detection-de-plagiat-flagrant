let read_base name_file =
  let channel = open_in name_file in
  let rec read_file () = 
    try 
      let data =  (input_line channel ) in
        data :: (read_file ())
    with End_of_file ->  close_in channel ; []
  in
    List.rev (read_file ()) ;;


let lireDonnee ()= 
  
    let file1 = Sys.argv.(1) in let file2 = Sys.argv.(2) in
      let sl1 = read_base file1  in
        let sl2 = read_base file2  in
        let s1 = List.fold_left (^) "" sl1 in
          let s2 = List.fold_left (^) "" sl2 in
            let uS1 = String.uppercase_ascii s1 in
              let uS2 = String.uppercase_ascii s2 in
                  (uS1,uS2)
  

let rec getNombreCaractere s i n ss=
  if i = n || i = String.length s then ss else  getNombreCaractere s (i+1) n (ss^(if s.[i] = ' ' || (Char.code s.[i]) =  Char.code '\'' then "" else (String.make 1 s.[i])));;



let calculeTemps func nombreCaractere file methode b= 
  let donnee = match lireDonnee () with
    (s1,s2) -> (String.trim ((getNombreCaractere s1 0 nombreCaractere "")^"#"),String.trim ((getNombreCaractere s2 0 nombreCaractere "")^"#"))
  in 
    let pt1 = Unix.times () in 
      match donnee with
        (s1,s2) -> Printf.printf "\nsous chaine des fichiers %s %s, methode  %s = %s\n" Sys.argv.(1) Sys.argv.(2)  methode (String.trim (func s1 s2));
      let pt2 = Unix.times () in 
        let ut = pt2.tms_utime -. pt1.tms_utime in
        if b then Printf.fprintf file  "%d %f " nombreCaractere ut  else Printf.fprintf file "%f\n"  ut  
            
;;

if Array.length Sys.argv >= 4 then
  let file_out = open_out Sys.argv.(3) in
  let lists = [|10; 20; 50; 75; 100; 125; 150; 200; 350; 400; 500; 1000|] in
    for i = 0 to (Array.length lists) - 1  do

      calculeTemps Programmation_dynamique.sousChaineCommunesDynamique2 lists.(i) file_out "programmation dynamique" true ;
      calculeTemps Arbre_suffixe_compresse.sousChainesCommunesCompresse2 lists.(i) file_out "arbre des suffixes compresse" false ;
      
    done; close_out file_out;
else failwith "No filename";;