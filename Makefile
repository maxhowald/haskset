

setserver: Setserve.hs set.hs SetAssets.hs
	ghc Setserve.hs

clean: 
	rm -f *.db3* *# *~ *.o *.hi *.dyn* *.aes Setserve
