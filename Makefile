GHC=ghc
C2HS=c2hs
LIBS=-lXi

all: xitest

xitest: xitest.hs Graphics/X11/XInput/Types.hs
	$(GHC) --make xitest.hs $(LIBS)

%.hs: %.chs
	$(C2HS) $<

clean:
	find . -name \*.chs.h -delete
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*.chi -delete
	rm -f xitest
