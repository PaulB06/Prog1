(* On définit un premier type de lexème plutôt simple et naïf, de manière à identifier chacun des caractères *)
type lexeme =
  | P_ouv
  | P_ferm
  | Point
  | Plus
  | Moins
  | Mod
  | Div
  | Mul
  | Chiffre of int
  | Int
  | Float

(* On utilise ce type lexeme pour transformer la chaîne de caractères initiale en une liste de lexèmes compréhensible pour le code
   Cette fonction présente tout de même l'inconvénient de faire disparaître tout les espaces de la chaîne de caractères, ce qui 
   conduira le code à interpréter par exemple "2    3. 6   98   1" comme le flottant 23.6981 au lieu de renvoyer une erreur *)
let rec string_to_lxmlst str =
  if String.length str = 0 then []
  else
    match str.[0] with
    | ' ' -> string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '+' -> Plus :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '(' ->
        P_ouv :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | ')' ->
        P_ferm :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '.' ->
        Point :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '-' ->
        Moins :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '%' -> Mod :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '/' -> Div :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | '*' -> Mul :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | c when Char.code c < 58 && Char.code c >= 48 ->
        Chiffre (Char.code c - 48)
        :: string_to_lxmlst (String.sub str 1 (String.length str - 1))
    | 'i' when (String.sub str 0 3) = "int" -> 
      Int :: string_to_lxmlst (String.sub str 3 (String.length str - 3))
    | 'f' when (String.sub str 0 5) = "float" ->
        Float :: string_to_lxmlst (String.sub str 5 (String.length str - 5))
    | _ -> failwith "caractère inattendu"

(* Notre premier type de lexème étant trop naïf, il ne permet pas de réelllemnt identifier les entiers et les flottants,
   ni de différencier les opérateurs ainsi que la fonction qu'ils occupent, on définit donc un nouveau type de lexème,
   les lexèmes améliorés (lexeme_am) qui seront plus polyvalents que le type précédent *)
type lexeme_am =
  | Par_ouv
  | Par_ferm
  | N_int of int
  | N_float of float
  | Plus_i
  | Plus_f
  | Moins_i
  | Moins_f
  | Modulo
  | Div_euc
  | Mul_i
  | Mul_f
  | Int_a
  | Float_a
  | Moins_sup
  | Moins_sup_i
  | Moins_sup_f

(* Les deux fonctions suivantes servent à définir les puissances de 10 à valeur dans les entiers et deans les flottants,
   on aura besoin de ces fonctions pour convertir nos listes de Chiffres en entiers ou en flottants *)
let rec p_10_i n = match n with 0 -> 1 | _ -> 10 * p_10_i (n - 1)

let p_10_f n =
  if n >= 0 then Float.of_int (p_10_i n) else 1. /. Float.of_int (p_10_i (-n))

(* On définit une fonction permettant de convertir une liste de Chiffres en un entier en fonction de la puissance de 10 associée à la liste *)
let lst_to_int l d =
  let rec aux l d =
    match l with Chiffre n :: q -> (n * p_10_i d) + aux q (d - 1) | _ -> 0
  in
  aux l (d - 1)

(* De même que précédemmment, on créé une fonction permettant de transformer une liste de Chiffres en un flottant *)
let lst_to_float l d =
  let rec aux l d =
    match l with
    | Chiffre n :: q -> (Float.of_int n *. p_10_f d) +. aux q (d - 1)
    | _ -> 0.
  in
  aux l (d - 1)

(* On obtient finalement une fonction nopus permettant de passer de notre ancien type de lexeme à notre nouveau,
   en différenciant chaque cas, pour connaître par exemple le type de chaque opérateur, ou la valeur de chaque réel *)
let am_lex lst =
  let rec reel l res n d c =
    match (l, n) with
    | Point :: q, _ -> reel q res (n + 1) d (c + 1)
    | Chiffre m :: q, 0 -> reel q (Chiffre m :: res) n (d + 1) (c + 1)
    | Chiffre m :: q, 1 -> reel q (Chiffre m :: res) n d (c + 1)
    | _, 0 -> (N_int (lst_to_int (List.rev res) d), c)
    | _, 1 -> (N_float (lst_to_float (List.rev res) d), c)
    | _, _ -> failwith "écris bien tes flottants stp!!!!"
  in
  let rec aux c l =
    if c <> 0 then match l with [] -> [] | t :: q -> aux (c - 1) q
    else
      match l with
      | [] -> []
      | Plus :: Point :: q -> Plus_f :: aux 0 q
      | Moins :: Point :: q -> Moins_f :: aux 0 q
      | Mul :: Point :: q -> Mul_f :: aux 0 q
      | Plus :: q -> Plus_i :: aux 0 q
      | Moins :: q -> Moins_i :: aux 0 q
      | Mul :: q -> Mul_i :: aux 0 q
      | Mod :: q -> Modulo :: aux 0 q
      | Div :: q -> Div_euc :: aux 0 q
      | P_ouv :: q -> Par_ouv :: aux 0 q
      | P_ferm :: q -> Par_ferm :: aux 0 q
      | Int :: q -> Int_a :: aux 0 q
      | Float :: q -> Float_a :: aux 0 q
      | Chiffre n :: q ->
          let lx, c = reel l [] 0 0 0 in
          lx :: aux c l
      | _ -> failwith "ceci était plutôt inattendu!!!"
  in
  aux 0 lst

(* On créé finalement une fonction "bilan", permettant de passer directement de la chaîne de caractères initiale 
   à notre type lexeme amélioré, ce sera la fonction utilisée dans le fichier en .mli, et donc dans le parseur s*)
  let lexemisation str = am_lex (string_to_lxmlst str)