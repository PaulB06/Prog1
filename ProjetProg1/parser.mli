open Lexer

type arbre_s =
  | Feuille of lexeme_am
  | Node of lexeme_am * arbre_s * arbre_s
  | Branche of lexeme_am * arbre_s

val arbrification: string -> arbre_s * int