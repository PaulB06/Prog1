(* On prend ce module pour pouvoir compiler un code Assembleur à partir d'une chaîne *)
open Compiler

(* Ce code permet d'extraire la première ligne d'un fichier, je crois qu'il plante si jamais on n'a pas de première 
   ligne et il ne s'intéresse pas aux autres lignes mais c'est pas très important au vu de ce que l'on veut faire *)
let extraction file = 
  let ic = open_in file in
input_line ic

(* Cette fonction créé ou remplace un fichier nommé file pour y écrire str dedans *)
let importation file str = 
  let oc = open_out file in 
  Printf.fprintf oc "%s" str;
  close_out oc ;;

(* J'ai vu qu'Hugo avait fait ça, et Hugo, c'est un crack de l'info pratique, du coup, je copie! *)
let usage = "Usage : ./aritha expression.exp"

(* Finalemnt, on arrive enfin à la fonction finale qui fait tout, et paradoxalement, c'est aussi la seule qui n'a
   pas de nom (ça fait réfléchir!), mais c'est normal puisqu'elle doit être éxécutée directement à l'appel de make,
   du coup, elle extrait la bonne ligne du bon fichier, elle compile un code Assembleur avec ça et elle le réécrit
   dans un fichier où .exp a été changé en .s, fichier que l'on peut finalement compilé puis éxécuter puisque c'est
   un code Assembleur. *)
let () =
  if Array.length Sys.argv <= 1 then print_endline usage
  else let file = Sys.argv.(1) in
  let str = extraction file in
  importation ((String.sub file 0 (String.length file - 3)) ^ "s") (compilation(str)) 
