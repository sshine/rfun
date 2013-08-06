default:
	alex Lexer.x
	ghc Lexer
	happy Parser.y -i
	ghc Parser
	ghc Main

clean:
	rm -f *.hi *.o Lexer.hs Parser.hs *.info Main
