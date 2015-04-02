

setserver: setserver.hs set.hs SetAssets.hs
	ghc setserver.hs

clean: 
	rm -f *.db3 *# *~ *.o *.hi *.dyn* *.aes setserver
