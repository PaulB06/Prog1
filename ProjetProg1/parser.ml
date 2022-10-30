(* On utilise le module Lexer pour avoir accès à la liste de lexèmes *)
open Lexer

(* On définit une fonction très simple renvoyant la longueur d'une liste *)
let rec long_lst l = match l with [] -> 0 | t :: q -> 1 + long_lst q

(* On créé une fonction renvoyant la zone comprise entre la première parenthèse ouvrante de la chaîne 
   et la parenthèse fermante correspondante, en éliminant ces deux dernières
   On nottera cependant que cette fonction suppose que la liste commence par une parenthèse ouvrante,
   mais cela ne doit pas poser de problème au vu des conditions d'appel de cette fonction  *)
let zone_par lst =
  let rec aux l c =
    match (l, c) with
    | Par_ferm :: q, 1 -> []
    | Par_ouv :: q, _ -> Par_ouv :: aux q (c + 1)
    | Par_ferm :: q, _ -> Par_ferm :: aux q (c - 1)
    | lex :: q, _ -> lex :: aux q c
    | [], _ ->
        failwith "Apprends à bien parenthéser une expression par pitié!!!!"
  in
  aux (List.tl lst) 1

(* Cette fonction sert à détecter la première sous-expression d'une liste, qui est renvoyée sous la forme d'une sous-liste de la liste principale
   ou d'une liste contenant seulement un opérateur, en fontion de la valeur de op, mais cette fonction rajoute aussi un entier couplé à cette 
   liste de manière à avoir de l'information sur le contenu de la liste, cet entier valant:
    +2 si la liste et vide, mais la fonction n'est pas censée être appelée dans ce cas (ça doit pas arriver)
    +1 si la liste est un opérateur arithmétique nécessitant deux arguments (ce cas doit ariver ssi op = true)
    0  si la liste contient seulement un réel (entier ou flottant) et que c'était le premier élément de la liste originale
    -1 si la liste commençait par une parenthèse ouvrante, auquel cas la liste renvoyée contient toute la zone entre les parnethèses (sans elles)
    -2 si la liste commençait par un des trois opérateurs unaires (float, int ou -) devant des parenthèses, auquel cas on renvoit cet opérateur 
    suivi de ce qu'il y avait entre les parenthèses après l'opérateur (mais en conservant les parenthèses)
    -3 si la liste commençait par un signe suivi d'un réel (entier ou flottant), auquel cas le signe était associé au réel
    -4 si la liste commençait par une parenthèse précédée d'un plus, auquel le plus est ignoré contrairement aux autres opérateurs unaires
*)
let rec detect_exp lst op =
  match lst with
  | Par_ouv :: _ when not op -> (zone_par lst, -1)
  | Par_ferm :: _ -> failwith "Ratio par les parenthèses!"
  | N_int n :: _ when not op -> ([ N_int n ], 0)
  | N_float x :: _ when not op -> ([ N_float x ], 0)
  | Plus_i :: N_int n :: _ when not op -> ([ N_int n ], -3)
  | Plus_i :: N_float x :: _ when not op -> ([ N_float x ], -3)
  | Plus_i :: Par_ouv :: q when not op -> (zone_par (Par_ouv :: q), -4)
  | Plus_i :: _ when op -> ([ Plus_i ], 1)
  | Moins_i :: N_int n :: _ when not op -> ([ N_int (-n) ], -3)
  | Moins_i :: N_float x :: _ when not op -> ([ N_float (-.x) ], -3)
  | Moins_i :: Par_ouv :: q when not op ->
      ((Moins_sup :: Par_ouv :: zone_par (Par_ouv :: q)) @ [ Par_ferm ], -2)
  | Moins_i :: _ when op -> ([ Moins_i ], 1)
  | Int_a :: Par_ouv :: q when not op ->
      ((Int_a :: Par_ouv :: zone_par (Par_ouv :: q)) @ [ Par_ferm ], -2)
  | Float_a :: Par_ouv :: q when not op ->
      ((Float_a :: Par_ouv :: zone_par (Par_ouv :: q)) @ [ Par_ferm ], -2)
  | Plus_f :: _ when op -> ([ Plus_f ], 1)
  | Moins_f :: _ when op -> ([ Moins_f ], 1)
  | Div_euc :: _ when op -> ([ Div_euc ], 1)
  | Modulo :: _ when op -> ([ Modulo ], 1)
  | Mul_i :: _ when op -> ([ Mul_i ], 1)
  | Mul_f :: _ when op -> ([ Mul_f ], 1)
  | Moins_sup :: Par_ouv :: q when not op ->
      ((Moins_sup :: Par_ouv :: zone_par (Par_ouv :: q)) @ [ Par_ferm ], -2)
  | [] -> ([], 2)
  | _ -> failwith "Ton expression est mal écrite... Dommage, essaye encore!!!"

(* Cette fonction utilise la fonction précédente pour dissocier une liste de lexèmes en plusieurs expressions, avec leurs entiers associés,
   tout en considérant qu'une fois sur deux l'on doive trouver un opérateur, l'autre fois sur deux, une expression, de plus, pour complètement 
   ignorer le plus dans +(exp), on rechange le -4 en -1 *)
let sep_exp lst =
  let rec aux l_l op c =
    match l_l with
    | t :: q when c <> 0 -> aux q op (c - 1)
    | t :: q ->
        let l, o = detect_exp l_l op in
        let n =
          long_lst l
          +
          if o = -1 then 2
          else 0 + if o = -3 then 1 else 0 + if o = -4 then 3 else 0
        in
        (l, if o = -4 then -1 else o) :: aux l_l (not op) n
    | [] -> []
  in
  aux lst false 0

(* Cette fonction récapitulative applique la fonction précédente à la chaîne de caractères de départ, en passant par toutes les fonctions utiles jusqu'ici *)
let separation str = sep_exp (lexemisation str)

(* Cette fonction permet tout simplement de transformer une liste en tableau, ce que l'on va utiliser pour manipuler plus facilement notre liste de lexèmes *)
let lst_to_tab lst =
  let n = long_lst lst in
  if n = 0 then failwith "Ah!Ah! Tu te crois malin à ne rien écrire?";
  let tab = Array.make n (List.hd lst) in
  let rec aux l i =
    match l with
    | t :: q ->
        tab.(i) <- t;
        aux q (i + 1)
    | [] -> ()
  in
  aux lst 0;
  tab

(* Cette fonction sert simplement à tester la présence d'un élément dans un tableau, en renvoyant true s'il est trouvé, false sinon *)
let test_in el tab =
  let r = ref false in
  let n = Array.length tab in
  for i = 0 to n - 1 do
    if el = tab.(i) then r := true
  done;
  !r

(* Cette fonction a pour objectif de définir l'ordre dans lequel les opérations (dont l'entier associé est 1) devront être appelées
   selon les priorités opératoires, en utilisant le fait que la liste d'un opérateur soit un singleton
   De plus, on est passé d'une liste à un tableau *)
let num_tab lst =
  let tab = lst_to_tab lst in
  let n = Array.length tab in
  let prio =
    [|
      [| Modulo; Div_euc; Mul_i; Mul_f |];
      [| Plus_i; Plus_f; Moins_i; Moins_f |];
    |]
  in
  let c = ref 1 in
  for i = 0 to 1 do
    for j = 0 to n - 1 do
      let a, b = tab.(j) in
      if a = [] then failwith "C'est quoi cette expression vide?????";
      if test_in (List.hd a) prio.(i) && b = 1 then (
        tab.(j) <- (a, !c);
        c := !c + 1)
    done
  done;
  tab

(* Encore une fonction récapitulative pour la fonction précédente *)
let numerotation str = num_tab (separation str)

(* Cette fonction va rechercher le maximum (entier) ou 0 d'une liste de couples selon la deuxième coordonnée, ce qui est notammment 
   utile pour savoir combien il y a d'opérateurs dans notre liste puisqu'ils ont été numérotés *)
let max_dc tab =
  let n = Array.length tab in
  let m = ref 0 in
  for i = 0 to n - 1 do
    let _, a = tab.(i) in
    if a > !m then m := a
  done;
  !m

(* Cette fonction réalise la projection selon la deuxième coordonnée d'un tableau de couples, ce qui va nous permettre de gagner en efficacité
   lorsque nous chercherons à accéder à la numérotation des opérateurs *)
let proj_dc tab =
  let n = Array.length tab in
  let a, b = tab.(0) in
  let res = Array.make n b in
  for i = 0 to n - 1 do
    let _, b = tab.(i) in
    res.(i) <- b
  done;
  res

(* Cette fonction renvoie la position de la première occurence d'un élément dans un tableau, ou la longuer du tableau si l'élément n'y est pas 
   (ce cas n'arrivera pas pour nous), ce qui est utile pour déterminer l'odre des positions dans lesquelles s'effectuent les opérations *)
let rech tab i =
  let n = Array.length tab in
  let j = ref 0 in
  while !j < n && tab.(!j) <> i do
    j := !j + 1
  done;
  !j

(* Cette fonction nous permet justement d'obtenir l'ordre des positions dans lesquelles s'effectuent les opérations en utilisant la fonction précédente *)
let ordre_tab tab m =
  let rec aux i =
    match i with _ when i > m -> [] | _ -> rech tab i :: aux (i + 1)
  in
  aux 1

(* On définit ici notre type arbre qui nous permettra de produire l'arbre syntaxique final, dans les feuilles de nos arbres seront stockées les entiers
   et les flottants, dans les nodes on aura les opérateurs binaires et dans les branches les opérateurs unaires, à noter qu'il est important de définir les
   branches, sans quoi il nous serait nécessaire de définir les arbres comme étant potentielllement vides, ce qu'il faut éviter tant que possible *)
type arbre_s =
  | Feuille of lexeme_am
  | Node of lexeme_am * arbre_s * arbre_s
  | Branche of lexeme_am * arbre_s

(* Cette fonction prenant un entier n en argument renvoie tout simplement le tableau [|0;1;...;n-1|] *)
let tab_id n =
  let tab = Array.make n 0 in
  for i = 1 to n - 1 do
    tab.(i) <- i
  done;
  tab

(* Cette fonction permet de fusionner deux arbres à l'aide d'un opérateur binaire et de renvoyer l'arbre qui fait la fusion des deux, à noter que
   les arguments en entrée et en sorti sont des couples (arbre,entier) où l'entier représente le type de l'arbre afin de détecter les erreurs de typage *)
let fusion_arbres c1 c2 op =
  let a1, t1 = c1 in
  let a2, t2 = c2 in
  match op with
  | (Plus_f | Moins_f | Mul_f) when t1 = 1 && t2 = 1 -> (Node (op, a1, a2), 1)
  | (Plus_i | Moins_i | Modulo | Mul_i | Div_euc) when t1 = 0 && t2 = 0 ->
      (Node (op, a1, a2), 0)
  | _ -> failwith "Ne mélange pas tes entiers avec tes flottants!!"

(* Une fonction récapitulative qui renvoie à la fois num et ord *)
let num_ord str =
  let num = numerotation str in
  let ord = ordre_tab (proj_dc num) (max_dc num) in
  (num, ord)

(* Cette fonction est la fonction qui renvoie finalement l'arbre syntaxique pour une liste de lexemes en entrée, elle est plutôt complexe et 
   utilise plusieurs fonctions définies précédemment, je vais donc tenter de l'expliquer pas à pas dans le code *)
let lst_to_arbre lst =
  let rec ab_type l =                                                         (* On utilise une fonction récursive auxiliaire bien que ce ne soit finalement pas nécessaire *)
    let tab = num_tab (sep_exp l) in                                          (* On sépare les différentes expressions de la liste tout en numérotant les opérateurs *)
    let n = Array.length tab in                                               (* Avoir la longueur du tableau sous la main ça ne fait jamais de mal *)
    let m = max_dc tab in                                                     (* On prend max_dc pour connaître le nombre d'opérateurs dans le tableau *)
    if m = 0 then (                                                           (* C'est le cas "facile" où l'on n'a pas d'opérations à faire, juste à différencier quelques cas *)
      let e, p = tab.(0) in
      if e = [] then failwith "Tu pensais vraiment m'avoir comme ça?...";     (* Je sais pas trop quand ça arrive ni même si ça peut arriver mais bon... *)
      match (List.hd e, p) with
      | _, -1 -> ab_type e                                                    (* Le cas où l'on avait initialemnt des parenthèses autour de l'expression, on peut donc refaire un appel récursif sur cette nouvelle expression *)
      | N_int n, _ -> (Feuille (N_int n), 0)                                  (* Si l'on a juste un entier ou un flottant, il suffit de le renvoyer en l'incluant dans une Feuille *)
      | N_float x, _ -> (Feuille (N_float x), 1)
      | Int_a, _ -> (                                                         (* Dans le cas des opérateurs unaires, on renvoie une branche où l'opérateur est au sommmet de la branche *)
          let a, t = ab_type (List.tl e) in                                   (* On peut aussi vérifier le bon typage des opérateurs float et int *)
          match t with                                                        (* et typer le moins losque l'on se trouve dans le cas -(exp) *)
          | 0 -> failwith "ça alors! Ton expression est mal typée..."
          | _ -> (Branche (Int_a, a), 0))
      | Float_a, _ -> (
          let a, t = ab_type (List.tl e) in
          match t with
          | 1 -> failwith "ça alors! Ton expression est mal typée..."
          | _ -> (Branche (Float_a, a), 1))
      | Moins_sup, _ ->
          let a, t = ab_type (List.tl e) in
          (Branche ((if t = 0 then Moins_sup_i else Moins_sup_f), a), t)
      | _, _ ->
          failwith "Cet individu n'est pas autorisé à se promener tout seul.")
    else                                                                      (* Cas compliqué, où il va falloir fusionner successivement plusieurs arbres par rapport aux différentes opérations *)
      let num = proj_dc tab in                                                (* On projette selon la deuxième coordonnée pour des soucis de simplicité *)
      let ordre = ref (ordre_tab num m) in                                    (* On prend l'odre des opérations à effectuer successivement, l'objectif va être de vider cette liste (plus aucune opération à faire) *)
      let valeurs = tab_id n in                                               (* Ce tableau devra représenter les valeurs associés à chaque arbre, deux arbres ayant la même valeur devant toujours être identiques *)
      let arbres = Array.make n (Feuille (N_int 0), 0) in                     (* On initialise un tableau de n arbres, sans que la valeur d'initialisation n'ait d'importance *)
      for i = 0 to n - 1 do                                                   (* On commence par initialiser dans le tableau arbre en y mettant tous les arbres *)
        if num.(i) <= 0 then                                                  (* qui ne correspondent pas à des opérateurs, on s'occupera ensuite de les fusionner *)
          let a, b = tab.(i) in                                               (* avec les opérateurs *)
          arbres.(i) <- ab_type a
      done;
      while !ordre <> [] do                                                   (* Correspond à un tant que il y a des opérations à faire *)
        let op = List.hd !ordre in                                            (* L'on extrait de la liste la première opération à faire, il faut donc calculer en priorité l'arbre qui *)
        ordre := List.tl !ordre;                                              (* lui est associé, avnt de le fusionner avec les autres *)
        let l, _ = tab.(op) in                                                (* On prend en l la liste (qui n'a en fait qu'un élément) contenant l'opérateur en question *)
        let cpl = fusion_arbres arbres.(op - 1) arbres.(op + 1) (List.hd l) in(* Puis on réalise la fusion des deux arbres autour de l'opérateur, avec l'opérateur associé *)
        let j = ref 1 in                                                      (* Ce nouvel arbre doit donc prendre la place de là où est l'opérateur, et de ses voisins, *)
        let v_mu = valeurs.(op - 1) in                                        (* puisqu'ils ont fusionnés pour le créer, mais aussi de toutes les cases du tableau où la *)
        while !j <= op && valeurs.(op - !j) = v_mu do                         (* valeur est la même que la valeur d'un des deux voisins, d'après le critère que l'on s'est fixé *)
          arbres.(op - !j) <- cpl;                                            (* pour le tableau valeurs, ce qui permettra de fusionner ce nouvel arbre avec des arbres *)
          valeurs.(op - !j) <- op;                                            (* éloignés dans le tableau, lorsque toutes les opérations entre les deux ont déjà été éffectuées *)
          j := !j + 1                                                         (* De plus, il ne faut pas oublier de modifier valeurs de manière à ce que toute les cases *)
        done;                                                                 (* contenant désormais ce nouvel arbre aient la même valeur *)
        arbres.(op) <- cpl;
        j := 1;
        let v_pu = valeurs.(op + 1) in
        while !j <= n - 1 - op && valeurs.(op + !j) = v_pu do                 (* Ainsi, à la fin du processus, toutes les cases ont la même valeur *)
          arbres.(op + !j) <- cpl;                                            (* Par conséquent, toutes les cases de arbres contiennent le même arbre que l'on peut renvoyer *)
          valeurs.(op + !j) <- op;                                            (* Comme résultat, en précisant au passage son type *)
          j := !j + 1
        done
      done;
      arbres.(0)
  in
  let a, t = ab_type lst in
  (a, t)

(* La fonction qui peut désormais nous créerss de magnifiques arbres (<3), à partir de la chaîne de caractère initiale,
   et par conséquence directe, la fonction que l'on utilisera dans le fichier en .mli ainsi que dans le compilateur *)
let arbrification str = lst_to_arbre (lexemisation str)