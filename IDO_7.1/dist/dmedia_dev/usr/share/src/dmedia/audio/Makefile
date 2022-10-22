#!smake
SHELL = /bin/sh
CFLAGS = -O 
ALL= motifexample clipit passthru record volume concert_a aselect\
audio_button audiostate recordexample
CC =cc

all: $(ALL)

motifexample:	motifexample.c
	$(CC) -O motifexample.c -lXm -lXt -lX11 -laudiofile -laudio -o motifexample

audiostate: audiostate.o
	$(CC) -o $@ audiostate.o -laudio

clipit: clipit.o
	$(CC) -o $@ clipit.o -laudio -lgl

passthru:       passthru.o
	$(CC) -o $@ passthru.o -laudio

record:       record.o
	$(CC) -o $@ record.o -laudio

aselect:       aselect.o
	$(CC) -o $@ aselect.o -laudio

concert_a:       concert_a.o
	$(CC) -o $@ concert_a.o -laudio -lm

volume:       volume.o
	$(CC) -o $@ volume.o -laudio

audio_button:       audio_button.o
	$(CC) -o $@ audio_button.o -lXm -lXt -lX11 -laudio

recordexample.o: recordexample.c
	$(CC) $(CFLAGS) -c recordexample.c

recordexample: recordexample.o
	$(CC) recordexample.o -o recordexample -laudio -laudiofile -laudioutil -lm -lmalloc

.c.o:
	$(CC) $(CFLAGS) -c $<
clean:
	rm -f *.o

clobber:clean
	rm -f $(ALL)
