# doom-oracle: headless linuxdoom-1.10 printing one state line per level tic
# (line format: doom/src/Doom/StateHash.elm).
#
#   make                              build ./doom-oracle
#   make demos WAD=<freedoom1.wad>    run DEMO1..DEMO4 -> out/demoN.txt
#   make check WAD=<freedoom1.wad>    demos again, under AddressSanitizer +
#                                     UBSan with a malloc-backed zone, and a
#                                     second plain run: all must be identical
#
# Built from src/, a copy of linuxdoom-1.10 (GPL-2.0). Every change to an id
# file is marked with a "DOOM-ORACLE" comment saying what changed and why it
# cannot alter play; the git history of doom/oracle/src starts with the
# pristine sources, so `git diff <first commit> -- doom/oracle/src` is the
# whole patch set. Dropped from the original: i_main.c, i_system.c,
# i_video.c, i_sound.c, i_net.c, s_sound.c (replaced by i_headless.c and
# oracle_main.c).
#
# Changes, by kind (details at each DOOM-ORACLE comment):
#  - platform: i_headless.c stubs (no video/sound/net, I_Quit exits 0),
#    oracle_main.c driver, -iwad and game mode from the WAD's maps (d_main.c),
#    headless D_DoomLoop = the original -singletics loop minus display/sound
#    (d_main.c), defaults without a config file (m_misc.c), header fixes for
#    macOS (m_bbox.h, w_wad.c, r_data.c).
#  - demos: G_DoPlayDemo accepts version 109 and in-memory demos (g_game.c);
#    the state line is printed from G_Ticker right after P_Ticker (g_game.c).
#  - 64-bit: arrays of pointers allocated as n*4 bytes (r_data.c, p_setup.c);
#    pointer<->int casts (d_net.c, d_main.c, r_data.c, r_draw.c, z_zone.c,
#    p_saveg.c, m_misc.c); the on-disk maptexture_t had a pointer member
#    (r_data.c); zone blocks rounded to 8 bytes (z_zone.c).
#  - memory safety: sprnames gets its NULL terminator (info.c); spechit,
#    intercepts and linespeciallist get room past their original limits and
#    report any overflow on stderr instead of corrupting memory (p_map.c,
#    p_maputl.c, p_local.h, p_spec.c, p_enemy.c).
#
# Flags that pin down C semantics the game relies on and that could differ
# between 1997 x86 gcc and arm64 clang:
#   -fsigned-char         plain char is signed on x86
#   -fwrapv               signed overflow wraps (two's complement) as on x86
#   -fno-strict-aliasing  the code type-puns freely (zone, WAD structs)
#   -ffp-contract=off     no fused multiply-add anywhere
#   -std=gnu99            implicit function declarations are hard errors (a
#                         pointer-returning function declared implicitly
#                         would be truncated to int on 64-bit)
# Evaluation order: expressions like (P_Random()-P_Random()) have unspecified
# operand order in C; clang evaluates left to right, as vanilla did.

CC      ?= clang
CFLAGS  ?= -O2
DFLAGS   = -std=gnu99 -fsigned-char -fwrapv -fno-strict-aliasing \
           -ffp-contract=off -DNORMALUNIX \
           -Werror=implicit-function-declaration \
           -Werror=int-conversion -Werror=pointer-to-int-cast \
           -Werror=int-to-pointer-cast \
           -Wno-incompatible-function-pointer-types \
           -Wno-deprecated-non-prototype -Wno-implicit-int \
           -Wno-dangling-else -Wno-parentheses -Wno-unused-value \
           -Wno-pointer-sign -Wno-shift-negative-value \
           -Wno-unknown-pragmas -Wno-tautological-pointer-compare \
           -Wno-unsequenced -Wno-format -Wno-strict-prototypes \
           -Wno-switch -Wno-return-type -Wno-constant-conversion \
           -Wno-incompatible-pointer-types -Wno-missing-declarations \
           -Wno-enum-compare -Wno-sizeof-pointer-memaccess \
           -Wno-deprecated-declarations -Wno-absolute-value

SANFLAGS = -O1 -g -fno-omit-frame-pointer -DORACLE_MALLOC_ZONE \
           -fsanitize=address,undefined \
           -fno-sanitize=shift-base,function,alignment

O    ?= build
BIN  ?= doom-oracle
SRCS  = $(wildcard src/*.c)
OBJS  = $(patsubst src/%.c,$(O)/%.o,$(SRCS))
WAD  ?= freedoom1.wad
DEMOS = 1 2 3 4

all: $(BIN)

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) $(DFLAGS) -o $@ $(OBJS) -lm

$(O)/%.o: src/%.c $(wildcard src/*.h) | $(O)
	$(CC) $(CFLAGS) $(DFLAGS) -c $< -o $@

$(O) out:
	mkdir -p $@

demos: $(BIN) | out
	@for n in $(DEMOS); do \
	  ./$(BIN) -iwad $(WAD) -playdemo DEMO$$n > out/demo$$n.txt \
	    2> out/demo$$n.err || { echo "DEMO$$n failed, see out/demo$$n.err"; exit 1; }; \
	  echo "DEMO$$n: $$(wc -l < out/demo$$n.txt | tr -d ' ') level tics"; \
	done

san:
	$(MAKE) O=build-san BIN=doom-oracle-san CFLAGS="$(SANFLAGS)"

check: demos san
	@for n in $(DEMOS); do \
	  ./$(BIN) -iwad $(WAD) -playdemo DEMO$$n > out/demo$$n.again 2>/dev/null; \
	  cmp -s out/demo$$n.txt out/demo$$n.again \
	    || { echo "DEMO$$n: second run differs"; exit 1; }; \
	  ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=halt_on_error=1 \
	    ./doom-oracle-san -iwad $(WAD) -playdemo DEMO$$n > out/demo$$n.san \
	    2> out/demo$$n.san.err || { echo "DEMO$$n: sanitizer run failed, see out/demo$$n.san.err"; exit 1; }; \
	  if grep -q "runtime error" out/demo$$n.san.err; then \
	    echo "DEMO$$n: UBSan reports, see out/demo$$n.san.err"; exit 1; fi; \
	  cmp -s out/demo$$n.txt out/demo$$n.san \
	    || { echo "DEMO$$n: sanitizer build differs"; exit 1; }; \
	  echo "DEMO$$n: deterministic, sanitizer-clean, identical across builds"; \
	done

clean:
	rm -rf build build-san doom-oracle doom-oracle-san out

.PHONY: all demos check clean san
