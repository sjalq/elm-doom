# screen-check: the original intermission and finale, run on their own, their
# frames dumped after every tic (harness.c), next to the Elm port's
# (src/ScreenCheck.elm, built with eco), compared pixel for pixel (check.py).
#
#   make                 build ./harness and ./screen-check
#   make check WAD=..    run every scenario in check.py on both, compare
#
# The game sources are doom/oracle/src (linuxdoom-1.10 with the oracle's
# platform changes), minus its driver and s_sound.c, which the harness replaces
# with stubs that log the sounds and music asked for.

CC     ?= clang
ECO    ?= eco
ORACLE  = ../../oracle
SRC     = $(ORACLE)/src
DFLAGS  = -std=gnu99 -fsigned-char -fwrapv -fno-strict-aliasing -ffp-contract=off \
          -DNORMALUNIX -w -O1 -Wno-implicit-int -Wno-incompatible-function-pointer-types \
          -Wno-int-conversion -Wno-incompatible-pointer-types -Wno-return-type
SRCS    = $(filter-out $(SRC)/oracle_main.c $(SRC)/s_sound.c,$(wildcard $(SRC)/*.c))
OBJS    = $(patsubst $(SRC)/%.c,build/%.o,$(SRCS)) build/harness.o
WAD    ?= ../../headless/freedoom1.wad
WAD2   ?= freedoom2.wad

all: harness screen-check

harness: $(OBJS)
	$(CC) $(DFLAGS) -o $@ $(OBJS) -lm

build/%.o: $(SRC)/%.c | build
	$(CC) $(DFLAGS) -c $< -o $@

build/harness.o: harness.c | build
	$(CC) $(DFLAGS) -I$(SRC) -c $< -o $@

build out:
	mkdir -p $@

screen-check: src/ScreenCheck.elm $(wildcard ../../src/Doom/*.elm ../../src/Doom/*/*.elm)
	$(ECO) make src/ScreenCheck.elm --output=screen-check

check: all | out
	python3 check.py --wad $(WAD) --wad2 $(WAD2)

clean:
	rm -rf build out harness screen-check

.PHONY: all check clean
