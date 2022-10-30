(* On a besoin du parseur car il nous faut nos magnifiques arbres!!! *)
open Parser

(* Comme flotant en assembleur => Pas cool, on abesoin de savoir à l'avance combien il faudra définir
   de flottants dans notre code, à noter que si le cas -(exp) où exp est de type float est match avec
   1 + ... , c'est parceque l'on va faire 0. -. exp pour réaliser cette opération en assembleur, et 
   donc on fait apparaître un nouveau flottant: 0. . Alors, on aurait pu s'arranger pour qu'il apparaisse
   au plus une fois étant donné que l'on peut réutilisé plusieurs fois le même zéro, mais honnêtement, 
   je n'ai pas grand chose à faire de ces problèmes d'optimisation *)
let rec compte_float a =
  match a with
  | Node (_, a1, a2) -> compte_float a1 + compte_float a2
  | Branche (Moins_sup_f, ap) -> 1 + compte_float ap
  | Branche (_, ap) -> compte_float ap
  | Feuille (N_int _) -> 0
  | Feuille (N_float _) -> 1
  | _ -> failwith "Ho là là! Cet arbre est très mal construit..."

(* Bon là on produit le code Assembleur, ah ah c'est marrant l'Assembleur! NON!!!! Pas du tout! C'est infernal!...
   et dire que mon pauvre petit arbre a été produit dans l'unique but d'être transformé en code Assembleur, 
   c'est là que je retombe en dépression que je pleure, que je remet ma vie en question: "POURQUOI? mon arbre était si beau 
   et si gentil..." Mais bon la vie est si cruelle,... il faut s'y faire... Toujours devoir se relever des erreurs du passé 
   pour aller de l'avant, ça me fait penser à la mort de mon père quand j'avais 7 ans: C'était un beau jour d'autom... ATTENDS!!! 
   QUOI??? Tu es vraiment en train de lire ce truc, je pensais que ça servais juste à décorer mon code. Bon, reprenons de manière
   un petit peu plus sérieuse, où en était on déjà... Ah oui! Le code Assembleur! Comme je le disais tout le monde adore ce truc merveilleux, 
   et je ne te ferais donc pas l'affront de te présenter en détail chacune des opérations que j'ai pu réaliser en Assembleur, ainsi que les
   instructions associées. Mais ce qu'il faut retenir c'est qu'on a une pile où l'on met tous nos nombres (entiers comme flotants), qu'avec les
   entiers ça marche plutôt bien, alors qu'avec les flottants, on doit changer nous-même le pointeur de la pile. Pour faire une opération,
   on prend ce qu'il y a sur la pile, on fait ce qu'il y a à faire et on rempile, en fonction du type du résultat on print soit un entier, 
   soit un flottant en écrivant les instructions qui vont bien avec ça, globalement il faut aussi compter les flottants pour
   les définir car, contrairement aux entiers, on ne peut pas juste les faire apparaître dans un registre, d'où l'importance de 
   compte_float et l'idée de tous les définir après la fonction main. Sinon j'ai peut-être éventuellement un petit peu fraudé dans le 
   cas où l'on a -(exp), où j'ai préféré dire qu'on faisait commme l'arbre 0(.) -(.) (exp) plutôt que de réécrire les étapes 
   d'Assembleur qui produisent ça, mais bon... Who cares? Voilà c'est cool on compile et PAF! on a de l'Asembleur, qui n'en a jamais révé??? *)
let assemblage arbre typ =
  let i = ref (-1) in
  let floats = Array.make (compte_float arbre) 0. in
  let rec assempile a =
    match a with
    | Feuille (N_int n) -> "\t pushq $" ^ string_of_int n ^ "\n"
    | Feuille (N_float x) ->
        i := !i + 1;
        floats.(!i) <- x;
        "\t movsd .LC" ^ string_of_int !i ^ "(%rip), %xmm0 \n"
        ^ "\t movsd %xmm0, -8(%rsp) \n" ^ "\t subq $8, %rsp \n"
    | Branche (Moins_sup_i, ap) ->
        assempile (Node (Moins_i, Feuille (N_int 0), ap))
    | Branche (Moins_sup_f, ap) ->
        assempile (Node (Moins_f, Feuille (N_float 0.), ap))
    | Branche (Float_a, ap) ->
        assempile ap ^ "\t popq %rax \n" ^ "\t cvtsi2sdq %rax, %xmm0 \n"
        ^ "\t movsd %xmm0, -8(%rsp) \n" ^ "\t subq $8, %rsp \n"
    | Branche (Int_a, ap) ->
        assempile ap ^ "\t movsd (%rsp), %xmm0 \n" ^ "\t addq $8, %rsp \n"
        ^ "\t cvttsd2siq %xmm0, %rax \n" ^ "\t pushq %rax \n"
    | Node (Plus_i, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t popq %rax \n" ^ "\t popq %rbx \n"
        ^ "\t addq %rax, %rbx \n" ^ "\t pushq %rbx \n"
    | Node (Moins_i, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t popq %rax \n" ^ "\t popq %rbx \n"
        ^ "\t subq %rax, %rbx \n" ^ "\t pushq %rbx \n"
    | Node (Mul_i, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t popq %rax \n" ^ "\t popq %rbx \n"
        ^ "\t imulq %rax, %rbx \n" ^ "\t pushq %rbx \n"
    | Node (Plus_f, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t movsd (%rsp), %xmm0 \n"
        ^ "\t movsd 8(%rsp), %xmm1 \n" ^ "\t addq $8, %rsp \n"
        ^ "\t addsd %xmm0, %xmm1 \n" ^ "\t movsd %xmm1, (%rsp) \n"
    | Node (Moins_f, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t movsd (%rsp), %xmm0 \n"
        ^ "\t movsd 8(%rsp), %xmm1 \n" ^ "\t addq $8, %rsp \n"
        ^ "\t subsd %xmm0, %xmm1 \n" ^ "\t movsd %xmm1, (%rsp) \n"
    | Node (Mul_f, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t movsd (%rsp), %xmm0 \n"
        ^ "\t movsd 8(%rsp), %xmm1 \n" ^ "\t addq $8, %rsp \n"
        ^ "\t mulsd %xmm0, %xmm1 \n" ^ "\t movsd %xmm1, (%rsp) \n"
    | Node (Modulo, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t popq %rbx \n" ^ "\t popq %rax \n"
        ^ "\t movq $0, %rdx \n" ^ "\t cqto \n" ^ "\t idivq %rbx \n"
        ^ "\t pushq %rdx \n"
    | Node (Div_euc, a1, a2) ->
        assempile a1 ^ assempile a2 ^ "\t popq %rbx \n" ^ "\t popq %rax \n"
        ^ "\t movq $0, %rdx \n" ^ "\t cqto \n" ^ "\t idivq %rbx \n"
        ^ "\t pushq %rax \n"
    | _ ->
        failwith
          "Maintenant j'ai tout fait donc cette erreur n'intervient pas..."
  in
  let rec aux tab n =
    match n with
    | -1 -> ""
    | n ->
        ".LC" ^ string_of_int n ^ ": \n" ^ "\t .double "
        ^ string_of_float tab.(n)
        ^ "\n"
        ^ aux tab (n - 1)
        ^ if n = 0 then "\n" else ""
  in
  let string_principale = assempile arbre in
  "\t .text \n" ^ "\t .globl main \n" ^ "main: \n" ^ string_principale
  ^
  if typ = 0 then
    "\t popq %rax \n" ^ "\t movq %rax, %rdi\n" ^ "\t call print_int \n"
    ^ "\t ret \n" ^ "\n" ^ aux floats !i ^ "print_int: \n"
    ^ "\t movq %rdi, %rsi \n" ^ "\t movq $aff_int, %rdi \n"
    ^ "\t movq $0, %rax \n" ^ "\t call printf \n" ^ "\t ret \n" ^ "\n"
    ^ "aff_int: \n" ^ "\t .string \"Le résultat (entier) vaut: %d   .\" \n"
  else
    "\t movsd (%rsp), %xmm0 \n" ^ "\t addq $8, %rsp \n"
    ^ "\t movq %xmm0, %rdi \n" ^ "\t call print_float \n" ^ "\t ret \n" ^ "\n"
    ^ aux floats !i ^ "print_float: \n" ^ "\t movq %rsp, %rbp \n"
    ^ "\t movq %rdi, %xmm0 \n" ^ "\t movl $aff_float, %edi \n"
    ^ "\t movl $1, %eax \n" ^ "\t call printf \n" ^ "\t nop \n" ^ "\t leave \n"
    ^ "\t ret \n" ^ "\t .data \n" ^ "\n" ^ "aff_float: \n"
    ^ "\t .string \"Le résultat (flottant) vaut: %g   .\" \n"

(* Fonction qui permet de compiler de l'Assembleur directement à partir de la chaîne initiale *)
let compilation str =
  let a, t = arbrification str in
  assemblage a t

(* Fonction qui a servi à vérifier rapidement quelques trucs avant d'avoir fini le makefile el le main.ml *)
let p str = print_string (compilation str)