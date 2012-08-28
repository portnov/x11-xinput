GHC=ghc
C2HS=c2hs
LIBS=-lXi

all: xitest

xitest: xitest.hs XInput.hs
	$(GHC) --make xitest.hs $(LIBS)

XInput.hs: XInput.chs
	$(C2HS) $<

clean:
	rm -f *.chs.h
	rm -f *.hi *.o
	rm -f *.chi
	rm -f xitest
