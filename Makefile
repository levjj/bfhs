GHC = ghc

default: clean run

bfhs: bfhs.hs
	$(GHC) --make -o $@ $+

run: bfhs
	./bfhs hello.bf

clean:
	rm -f bfhs
