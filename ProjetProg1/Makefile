all: rapport aritha

aritha: compiler
	ocamlc lexer.ml parser.ml compiler.ml main.ml -o aritha
	
rapport: 
	pdflatex rapport.tex

lexer: 
	ocamlc -c lexer.mli
	ocamlc -c lexer.ml

parser: lexer
	ocamlc -c parser.mli
	ocamlc -c parser.ml

compiler: parser
	  ocamlc -c compiler.mli
	  ocamlc -c compiler.ml

test:	aritha
	./aritha exemples/exemple1.exp
	gcc -no-pie exemples/exemple1.s -o exp1
	./aritha exemples/exemple2.exp
	gcc -no-pie exemples/exemple2.s -o exp2
	./aritha exemples/exemple3.exp
	gcc -no-pie exemples/exemple3.s -o exp3
	./aritha exemples/exemple4.exp
	gcc -no-pie exemples/exemple4.s -o exp4
	./aritha exemples/exemple5.exp
	gcc -no-pie exemples/exemple5.s -o exp5
	./aritha exemples/exemple6.exp
	gcc -no-pie exemples/exemple6.s -o exp6
	./aritha exemples/exemple7.exp
	gcc -no-pie exemples/exemple7.s -o exp7

clean:
	rm -f *.log *.aux *.cmi *.cmo *.s aritha exemples/*.s exp*
