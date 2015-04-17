CC=ghc
LX=alex

default: main

main:
	$(MAKE) -C src/
	mv src/mpp .

clean:
	$(MAKE) -C src/ clean
	-rm mpp

