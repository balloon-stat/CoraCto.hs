PROG = CoraCto

$(PROG).exe: $(PROG).hs
	ghc  -XMultiWayIf $(PROG).hs

run: $(PROG).exe
	./$(PROG).exe
