default:
	alex LambdaToyTokens.x
	happy LambdaToyGrammar.y
	ghc -no-keep-hi-files -no-keep-o-files Main.hs

clean:
	rm LambdaToyGrammar.hs LambdaToyTokens.hs Main
