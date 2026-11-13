# music-ref: Chocolate Doom's OPL music path, headless (see main.c).
#   make              build ./music-ref
#   make check        render and compare the JS port for the test songs (compare.sh)
CC ?= cc
# Chocolate Doom's music depends on x86-64 here: a tempo change after the music
# was paused converts a negative float to uint64_t (opl_queue.c's
# OPL_Queue_AdjustCallbacks), which wraps on x86-64 and saturates to 0 on arm64.
# The reference is the x86-64 behaviour, so on an Apple Silicon Mac the renderer
# is built for x86-64 (it runs under Rosetta).
ifeq ($(shell uname -sm),Darwin arm64)
ARCH = -arch x86_64
endif
CFLAGS ?= -O2 -std=gnu99 -fwrapv -fno-strict-aliasing -ffp-contract=off -w
SRC = main.c shim/shim.c \
      chocolate/i_oplmusic.c chocolate/midifile.c chocolate/mus2mid.c chocolate/memio.c \
      chocolate/opl.c chocolate/opl_queue.c chocolate/opl3.c
# opl_sdl.c's calls into the emulator go through shim.c's tracing wrappers
TRACED = -DOPL3_GenerateStream=REF_GenerateStream -DOPL3_WriteRegBuffered=REF_WriteRegBuffered

music-ref: $(SRC) chocolate/opl_sdl.c $(wildcard chocolate/*.h shim/*.h)
	$(CC) $(ARCH) $(CFLAGS) -Ishim -Ichocolate $(TRACED) -c chocolate/opl_sdl.c -o opl_sdl.o
	$(CC) $(ARCH) $(CFLAGS) -Ishim -Ichocolate -o $@ $(SRC) opl_sdl.o
	rm -f opl_sdl.o

check: music-ref
	./compare.sh

clean:
	rm -f music-ref

.PHONY: check clean
