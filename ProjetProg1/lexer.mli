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

val lexemisation: string -> lexeme_am list