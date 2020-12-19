all:
	yacc -d semantic.y
	lex scanner.l
	
	gcc -g lex.yy.c y.tab.c -o lava

	./lava