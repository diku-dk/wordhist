MLTON=mlton
FUTHARK?=futhark
FUTHARK_BACKEND?=multicore
CC?=cc
CFLAGS?=-O3
MLTONFLAGS = \
	-default-ann 'allowFFI true' \
	-codegen c \
	-default-type int64

ifeq ($(FUTHARK_BACKEND),cuda)
	MLTONFLAGS+=-link-opt -lcuda -link-opt -lcudart -link-opt -lnvrtc
endif

wordhist: wordhist.mlb main.sml wordhist.c wordhist.smlfut.c
	$(MLTON) $(MLTONFLAGS) wordhist.mlb wordhist.smlfut.c wordhist.c

libwordhist.a: wordhist.o wordhist.smlfut.o
	ar rcs $@ $^

%.o: %.c
	$(CC) -c $^ $(CFLAGS)

wordhist.smlfut.c: wordhist.c
	smlfut --target=mlton-mono wordhist.json --structure-name=Wordhist

wordhist.c: wordhist.fut lib
	$(FUTHARK) $(FUTHARK_BACKEND) --library wordhist.fut

lib: futhark.pkg
	futhark pkg sync

clean:
	rm -rf *.smlfut.* wordhist *.o *.a wordhist.c wordhist.h wordhist.json wordhist.sig wordhist.sml MLB
