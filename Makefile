GHC = ghc

default: clean run

bfhs: bfhs.hs
	$(GHC) --make -o $@ $+

run: bfhs
	./bfhs a.bf

clean:
	rm -f bfhs bfhs.o bfhs.hi
