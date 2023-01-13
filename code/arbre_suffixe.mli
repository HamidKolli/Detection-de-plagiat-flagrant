
(*Structure de donnee de l'arbre des suffixes(c'est un arbre lexical)*)
(*char(la lettre)*boolean(si le noeud est commun entre deux chaine)*arbre_lex(les fils du noeud)*)
type noeud_lex = Lettre of char *bool * arbre_lex
and arbre_lex = noeud_lex  list;;

(*Structure de donnee de l'arbre des suffixes*)
(*string(le mot)*arbre_lex_compresse(les fils du noeud)*)  
type noeud_lex_compresse = Mot of string  * arbre_lex_compresse
and arbre_lex_compresse = noeud_lex_compresse  list;;


  

    (*Fonction qui permet d'ajouter une chaine entiere dans un arbre des suffixes*)
    (*string(la chaine a ajouter)->arbre_lex(l'arbre des suffixes)->arbre_lex(l'arbre d'entre + la chaine)*)
  val ajoute : string -> arbre_lex -> arbre_lex  

    (*Fonction qui permet de modifier que les noeuds communs entre une chaine et un arbre des suffixes en mettant le boolean a true*)
    (*string(la chaine a ajouter)->arbre_lex(l'arbre des suffixes)->arbre_lex(l'arbre d'entre avec les noeuds communs a true)*)
  val ajoute2 : string -> arbre_lex -> arbre_lex  

    (*Fonction qui construit un arbre apartir d'une liste de mot*)
    (*string list->arbre_lex*)
  val construit : string list -> arbre_lex

    (*Fonction qui renvoie une des plus longues sous chaines communes entre s1 et s2 *)
    (*string->string->string*)
  val sousChainesCommunes : string -> string -> string

    (*Fonction qui construit un arbre des suffixe a partir d'une chaine s*)
    (*string-> arbre_lex*)
  val arbreSuffixes : string -> arbre_lex   

    (*Fonction qui renvoie un arbre compresse a partir d'un arbre des suffixes *)
    (*arbre_lex -> arbre_lex_compresse*)
  val compresse : arbre_lex -> arbre_lex_compresse
