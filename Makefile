all:
	cabal exec ghc -- -H16m -O2 -j -threaded -o hfish Main.hs

dyn:
	cabal exec ghc -- -H16m -O2 -j -threaded -dynamic -o hfish Main.hs

prof:
	cabal exec ghc -- -H16m -O2 -j -threaded -dynamic -o hfish Main.hs
	cabal exec ghc -- -H16m -O2 -j -threaded -prof -o hfish-prof -prof -osuf p.o -hisuf p.hi Main.hs

clean:
	find . -name '*.dyn_hi' -delete
	find . -name '*.dyn_o' -delete
	find . -name '*.hi' -delete
	find . -name '*.o' -delete


