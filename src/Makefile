CC=ghc
LX=alex

default: main

main: MppLexer.hs MppParser.hs
	ghc Main.hs -o mpp

MppLexer.hs: MppLexer.x
	alex MppLexer.x

MppParser.hs: MppParser.y
	happy MppParser.y

clean:
	-rm *.o *.hi mpp MppLexer.hs MppParser.hs

