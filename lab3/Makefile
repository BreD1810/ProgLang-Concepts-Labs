main: LambdaToyTokens.x LambdaToyGrammar.y
	alex LambdaToyTokens.x
	happy LambdaToyGrammar.y
	ghc -no-keep-hi-files -no-keep-o-files Main.hs

clean: LambdaToyGrammar.hs LambdaToyTokens.hs Main
	rm LambdaToyGrammar.hs LambdaToyTokens.hs Main
