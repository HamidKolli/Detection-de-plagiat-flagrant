
test.cmi:
	ocamlc -c test.mli 

test.cmo: test.cmi
	ocamlc -c test.ml
chaine.cmi:
	ocamlc -c chaine.mli 

chaine.cmo: chaine.cmi
	ocamlc -c chaine.ml
 
arbre_suffixe.cmi:
	ocamlc -c arbre_suffixe.mli 

arbre_suffixe.cmo: chaine.cmo arbre_suffixe.cmi
	ocamlc chaine.cmo -c arbre_suffixe.ml


arbre_suffixe_test:  chaine.cmo arbre_suffixe.cmo  test.cmo
	ocamlc  test.cmo chaine.cmo arbre_suffixe.cmo  -o arbre_suffixe_test.exe arbre_suffixe_test.ml
 
arbre_suffixe_compresse.cmi:
	ocamlc -c arbre_suffixe_compresse.mli 

arbre_suffixe_compresse.cmo: chaine.cmo arbre_suffixe.cmi arbre_suffixe_compresse.cmi
	ocamlc chaine.cmo arbre_suffixe.cmo -c arbre_suffixe_compresse.ml

dot.cmi : arbre_suffixe.cmo
	ocamlc arbre_suffixe.cmo -c dot.mli

dot.cmo : dot.cmi arbre_suffixe.cmo
	ocamlc  arbre_suffixe.cmo -c dot.ml

dot_test : dot.cmo chaine.cmo arbre_suffixe.cmo arbre_suffixe_compresse.cmo
	ocamlc dot.cmo chaine.cmo arbre_suffixe.cmo  arbre_suffixe_compresse.cmo -o dot_test.exe dot_test.ml 

arbre_suffixe_compresse_test:  test.cmo chaine.cmo arbre_suffixe.cmo arbre_suffixe_compresse.cmo
	ocamlc  test.cmo chaine.cmo arbre_suffixe.cmo arbre_suffixe_compresse.cmo  -o arbre_suffixe_compresse_test.exe arbre_suffixe_compresse_test.ml

programmation_dynamique.cmi:
	ocamlc -c programmation_dynamique.mli 

programmation_dynamique.cmo: programmation_dynamique.cmi
	ocamlc  -c programmation_dynamique.ml

programmation_dynamique_test: test.cmo programmation_dynamique.cmo
	ocamlc test.cmo programmation_dynamique.cmo -o programmation_dynamique_test.exe programmation_dynamique_test.ml

main:  programmation_dynamique.cmo chaine.cmo arbre_suffixe.cmo arbre_suffixe_compresse.cmo
	ocamlc  unix.cma chaine.cmo arbre_suffixe.cmo arbre_suffixe_compresse.cmo programmation_dynamique.cmo -o main.exe main.ml

define execute = 
	for i in 1 2 3 4 5 6
	do
		./test.exe ../Donnees_test/donnee0.txt ../Donnees_test/donnee$i.txt ../analyses/donnee$i.txt
	done;
endef

exec_all: main ; $(value execute)

.ONESHELL:
	
define gnuplot =
	for i in 1 2 3 4 5 6
	do
		gnuplot -e "filename='../analyses/donnee$i.txt'" -e "data='donnee0 et donnee$i'" -e "file_out='../diagramme_gnuplot/donnee$i.png'" ../gnuplot_file/plot 
	done;
endef

plot: exec_all ;$(value gnuplot)
	
.ONESHELL:

clean:
	rm -rf *.cmi *.cmo *.exe "../diagramme_gnuplot/donnee_arbre.png" "../diagramme_gnuplot/donnee_progDynamique.png" ../analyses/*.txt ../dot/*

clean_all: clean
	rm -rf ../diagramme_gnuplot/*