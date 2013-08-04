default:
	alex Lexer.x
	ghc Lexer.hs
	happy Parser.y -i
	ghc Parser.hs

clean:
	rm -f *.hi *.o Lexer.hs Parser.hs *.info
