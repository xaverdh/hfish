all:
	ghc -H16m -O2 -j5 -threaded -o hfish Main.hs

dyn:
	ghc -H16m -O2 -j5 -threaded -dynamic -o hfish Main.hs

prof:
	ghc -H16m -O2 -j5 -threaded -dynamic -o hfish Main.hs
	ghc -H16m -O2 -j5 -threaded -prof -o hfish-prof -prof -osuf p.o -hisuf p.hi Main.hs

clean:
	find . -name '*.dyn_hi' -delete
	find . -name '*.dyn_o' -delete
	find . -name '*.hi' -delete
	find . -name '*.o' -delete


