GHC=ghc
C2HS=c2hs
LIBS=-lXi

MODULES=Graphics/X11/XInput/Types.hs \
				Graphics/X11/XInput/Foreign.hs \
				Graphics/X11/XInput/Functions.hs \
				Graphics/X11/XInput/Parser.hs

all: xitest

xitest: xitest.hs $(MODULES)
	$(GHC) --make xitest.hs $(LIBS)

%.hs: %.chs
	$(C2HS) $<

clean:
	find . -name \*.chs.h -delete
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*.chi -delete
	rm -f xitest
