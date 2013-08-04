default:
	alex RFUN/Lexer.x
	ghc RFUN/Lexer.hs
	happy RFUN/Parser.y -i
	ghc RFUN/Parser.hs

clean:
	cd RFUN && rm -f *.hi *.o Lexer.hs Parser.hs
