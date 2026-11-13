/* music-ref: render a Doom music lump with Chocolate Doom's own OPL music code
 * (i_oplmusic.c, midifile.c, mus2mid.c, opl.c, opl_sdl.c, opl_queue.c and the
 * Nuked OPL3 emulator, compiled unchanged from chocolate/) to raw PCM.
 *
 *   music-ref <wad> <lump or file> <seconds> <out.raw> [volume] [looping]
 *
 * The output is interleaved 16-bit little-endian stereo at 49716 Hz, everything
 * the "audio device" plays from the moment the music module is initialised
 * (including the buffers OPL_Detect's delays play). volume is the
 * SetMusicVolume value, 0..127 (Chocolate passes the menu's 0..15 times 8;
 * default 64, the default setting 8); it is set after 127, as linuxdoom's
 * S_SetMusicVolume does. looping defaults to 1.
 *
 * A seventh argument is a script of what the game does later, applied before the
 * device buffer it names (buffers count from 0, 1024 frames each, the ones
 * OPL_Detect plays included): "buffer:action[:arg[:arg]]" items separated by
 * commas, the actions being pause, resume, stop, volume:V, and play:LUMP:LOOP
 * (S_ChangeMusic: stop, unregister, register, play).
 *
 * Part of the Doom port in Elm-OS, GPL-2.0-or-later. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doomtype.h"
#include "i_sound.h"
#include "ref.h"

int snd_samplerate = REF_RATE;
int snd_musicdevice = 0;

static void *current = NULL;

/* one script item, at the start of buffer `buffer` */
static void run_script(const char *script, long buffer)
{
    const char *p = script;
    while (p != NULL && *p)
    {
        char item[128];
        const char *end = strchr(p, ',');
        size_t n = end ? (size_t) (end - p) : strlen(p);
        char *action, *arg1, *arg2;
        long at;
        if (n >= sizeof(item)) n = sizeof(item) - 1;
        memcpy(item, p, n);
        item[n] = 0;
        p = end ? end + 1 : NULL;
        at = strtol(item, &action, 10);
        if (at != buffer || *action != ':') continue;
        action++;
        arg1 = strchr(action, ':');
        if (arg1) *arg1++ = 0;
        arg2 = arg1 ? strchr(arg1, ':') : NULL;
        if (arg2) *arg2++ = 0;
        if (!strcmp(action, "pause")) music_opl_module.PauseMusic();
        else if (!strcmp(action, "resume")) music_opl_module.ResumeMusic();
        else if (!strcmp(action, "stop")) music_opl_module.StopSong();
        else if (!strcmp(action, "volume")) music_opl_module.SetMusicVolume(atoi(arg1));
        else if (!strcmp(action, "play"))
        {
            int len;
            void *lump = ref_lump(arg1, &len);
            music_opl_module.StopSong();
            if (current) music_opl_module.UnRegisterSong(current);
            current = lump ? music_opl_module.RegisterSong(lump, len) : NULL;
            music_opl_module.PlaySong(current, arg2 ? atoi(arg2) : 1);
        }
    }
}

int main(int argc, char **argv)
{
    void *song;
    void *data;
    int length;
    double seconds;
    size_t want;
    int volume = 64;
    int looping = 1;
    FILE *out;

    if (argc < 5)
    {
        fprintf(stderr, "usage: music-ref <wad> <lump> <seconds> <out.raw> [volume] [looping]\n");
        return 2;
    }
    if (!ref_load_wad(argv[1]))
    {
        fprintf(stderr, "music-ref: can't read %s\n", argv[1]);
        return 1;
    }
    /* a lump name, or a file (a lump saved on its own) */
    data = ref_lump(argv[2], &length);
    if (data == NULL)
    {
        FILE *lf = fopen(argv[2], "rb");
        if (lf != NULL)
        {
            fseek(lf, 0, SEEK_END);
            length = (int) ftell(lf);
            fseek(lf, 0, SEEK_SET);
            data = malloc(length);
            if (fread(data, 1, length, lf) != (size_t) length) data = NULL;
            fclose(lf);
        }
    }
    if (data == NULL)
    {
        fprintf(stderr, "music-ref: no lump %s\n", argv[2]);
        return 1;
    }
    seconds = atof(argv[3]);
    if (argc > 5) volume = atoi(argv[5]);
    if (argc > 6) looping = atoi(argv[6]);

    if (!music_opl_module.Init())
    {
        fprintf(stderr, "music-ref: OPL init failed\n");
        return 1;
    }
    music_opl_module.SetMusicVolume(127);
    music_opl_module.SetMusicVolume(volume);
    song = music_opl_module.RegisterSong(data, length);
    if (song == NULL)
    {
        fprintf(stderr, "music-ref: couldn't register %s\n", argv[2]);
        return 1;
    }
    music_opl_module.PlaySong(song, looping);
    current = song;

    want = (size_t) (seconds * REF_RATE);
    while (ref_output_frames < want)
    {
        if (argc > 7) run_script(argv[7], (long) (ref_output_frames / REF_BUFFER));
        ref_device_buffer();
    }

    out = fopen(argv[4], "wb");
    if (out == NULL)
    {
        fprintf(stderr, "music-ref: can't write %s\n", argv[4]);
        return 1;
    }
    fwrite(ref_output, sizeof(int16_t) * 2, want, out);
    fclose(out);
    ref_close_trace();
    return 0;
}
