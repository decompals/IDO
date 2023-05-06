/*****************************************************************************
 * Audio File Library example code: aifcinfo
 *   parse an AIFF-C or AIFF file and print out a description of
 *   the contents
 *
 *   scott porter, april 92
 *****************************************************************************/
#ident "$Revision: 1.20 $"

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <malloc.h>
#include <fcntl.h>
#include <string.h>
#include <dmedia/audioutil.h>
#include <dmedia/audiofile.h>

#ifdef AWARE
/*
 * Aware compression info
 */
typedef struct {
    long   layer;
    long   bitratepolicy;
    long   bitratetarget;
} aware_desc_type;

#endif /*AWARE*/

/*
 * this structure describes an audio track
 */
typedef struct {
    int nmarks;                      /* number of markers */
    long *markid;                    /* list of marker ids */
    long *markpos;                   /* list of marker positions */
    char **markname;                 /* list of marker names */
    int  haveaesdata;                /* was there AES data for the track? */
    unsigned char aesdata[24];       /* contains the AES data */
    long sampwidth;                  /* sample width */
    long sampfmt;                   /* sample type */
    long nframes;                     /* total number of samples */
    long nchannels;                  /* number of audio channels */
    double rate;                     /* sample rate */
    long compressiontype;            /* compression type */
    char *compressionname;           /* compression name */
#ifdef AWARE
    aware_desc_type aware_desc;
#endif /*AWARE*/
} track_desc_type;


/*
 * this structure contains sampler configuration parameters
 */
typedef struct {
    long midi_basenote;             /* base note for sample playback */
    long midi_lonote;               /* lowest note for sample playback */
    long midi_hinote;               /* highest note for sample playback */
    long midi_lovelocity;           /* lowest velocity for sample playback */
    long midi_hivelocity;           /* highest velocity for sample playback */
    long numdbs_gain;               /* number of DB's of gain suggested */
    long numcents_detune;           /* number of cents of detune suggested */
    long sustain_loop_id;                 /* sustain loop id */
    long sustain_loop_mode;               /* sustain loop mode */
    long sustain_loop_start_mark_id;      /* sustain loop start mark */
    long sustain_loop_end_mark_id;        /* sustain loop end mark */
    long sustain_loop_start_frame;
    long sustain_loop_end_frame;
    long release_loop_id;                 /* release loop id */
    long release_loop_mode;               /* release loop mode */
    long release_loop_start_mark_id;      /* release loop start mark */
    long release_loop_end_mark_id;        /* release loop end mark */
    long release_loop_start_frame;
    long release_loop_end_frame;
} instrument_desc_type;


/*
 * this structure describes a "miscellaneous data chunk"
 */
typedef struct {
    long miscid;                   /* id for the miscellaneous chunk */
    long miscsize;                 /* size of the miscellaneous chunk */
    long misctype;                 /* type of miscellaneous data */
    char *misctext;                /* text, if applicable */
} misc_desc_type;

/*
 * this structure describes an audio file 
 */
typedef struct {
    int fd;                                /* unix file descriptor */
    char *filename;                        /* file name */
    AFfilehandle file;                     /* AF file handle */
    long filefmt;                          /* file format */
    long filefmtvers;                      /* fmt version, if applicable */
    track_desc_type track_desc;            /* audio track description */
    long have_sampler_data;                /* was there instrument data */
    instrument_desc_type instrument_desc;  /* instrument data description */
    long num_misc;                         /* number of misc data chunks */
    misc_desc_type  *misc_desc_list;       /* misc data chunks descriptions */
} file_desc_type;

static char spaces[] = "   ";

static void getmyname(char *, char **);
static char *myname;

static void read_file_desc(file_desc_type *);
static void read_track_desc(file_desc_type *);
static void read_instrument_desc(file_desc_type *);
static void read_misc_desc(file_desc_type *);
static void print_file_desc(file_desc_type *);
static void print_track_desc(file_desc_type *);
static void print_instrument_desc(file_desc_type *);
static void print_misc_desc(file_desc_type *);


main(int argc, char **argv)
{
    file_desc_type file_desc;


    getmyname(argv[0], &myname);

    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s filename\n", myname);
        exit(1);
    }

    file_desc.filename = argv[1];
    file_desc.fd       = open(file_desc.filename, O_RDONLY);
 
    if (!file_desc.fd) {
       fprintf(stderr, "%s: failed to open the file %s\n", 
          myname, file_desc.filename);
       exit(1);
    }

    /*
     * test whether the audio file library can identify the file
     * as a supported audio format
     */
    file_desc.filefmt = AFidentifyfd(file_desc.fd);
    switch(file_desc.filefmt)
    {
        case AF_FILE_AIFFC:
        case AF_FILE_AIFF:
           break;
        default:
           fprintf(stderr, 
           "%s: failed to recognize %s as an AIFF-C or AIFF file\n",
                myname, file_desc.filename); 
           exit(1);
    }

    /*
     * attach an audio file handle to the file descriptor
     */
    file_desc.file = AFopenfd(file_desc.fd, "r", AF_NULL_FILESETUP);

    if (file_desc.file == AF_NULL_FILEHANDLE ) {
      fprintf(stderr, "%s: failed to open %s as an AIFF-C or AIFF file\n", 
                       myname, file_desc.filename);
      exit(1);
    }

    /* 
     * get general file description
     */
    read_file_desc(&file_desc);

    /*
     * get audio track description
     */
    read_track_desc(&file_desc);

    /*
     * get sampler info
     */
    read_instrument_desc(&file_desc);

    /*
     * get descriptions of miscellaneous data 
     */
    read_misc_desc(&file_desc);

    /*
     * print file description
     */
    print_file_desc(&file_desc);

    /*
     * print audio track description
     */
    print_track_desc(&file_desc);

    /*
     * print instrument map description
     */
    print_instrument_desc(&file_desc);

    /* 
     * print miscellaneous data chunk descriptions
     */
    print_misc_desc(&file_desc);

    /*
     * close the filehandle structure
     */
    AFclosefile(file_desc.file);

    exit(0);
}


static void
getmyname(char *argv0, char **myname)
{  
    char *p;

    /*printf("argv0 = %s\n", argv0);*/
    p = strrchr(argv0, '/');
    if (p) {
        p++; 
    } 
    else {
        p = argv0;
    }
    
    *myname = (char *)malloc(strlen(p) + 1);
    strcpy(*myname, p);

    /*printf("myname = %s\n", *myname);*/
} 

static void
read_file_desc(file_desc_type *file_desc)
{
    AFfilehandle file;

    file = file_desc->file;
    file_desc->filefmt = AFgetfilefmt(file, &file_desc->filefmtvers);
    file_desc->num_misc = AFgetmiscids(file, (long *)0);
    file_desc->misc_desc_list
       = (misc_desc_type *)calloc(file_desc->num_misc, sizeof(misc_desc_type));
    file_desc->have_sampler_data = AFgetinstids(file, (long *)0);
}

static void 
read_track_desc(file_desc_type *file_desc)
{
    track_desc_type *track_desc;
    AFfilehandle file;
    long sampfmt, sampwidth;
    char *s;
    int i;
#ifdef AWARE
    AUpvlist pvlist;
#endif /*AWARE*/

    track_desc = &(file_desc->track_desc);
    file = file_desc->file;

    track_desc->nframes = AFgetframecnt(file, AF_DEFAULT_TRACK);
    track_desc->nchannels = AFgetchannels(file, AF_DEFAULT_TRACK);

    AFgetsampfmt(file, AF_DEFAULT_TRACK, 
        &track_desc->sampfmt, &track_desc->sampwidth);

    track_desc->haveaesdata 
       = AFgetaeschanneldata(file, AF_DEFAULT_TRACK, track_desc->aesdata);

    track_desc->rate = AFgetrate(file, AF_DEFAULT_TRACK);
    track_desc->compressiontype = AFgetcompression(file, AF_DEFAULT_TRACK);
    s = AFgetcompressionname(file, AF_DEFAULT_TRACK);

#ifdef AWARE
    if ((track_desc->compressiontype == AF_COMPRESSION_AWARE_MULTIRATE)
          || (track_desc->compressiontype == AF_COMPRESSION_AWARE_MPEG))
    {
        pvlist = AUpvnew(3);

        AUpvsetparam(pvlist, 0, AF_AWARE_PARAM_LAYER);
        AUpvsetvaltype(pvlist,  0, AU_PVTYPE_LONG);

        AUpvsetparam(pvlist, 1, AF_AWARE_PARAM_BITRATE_POLICY);
        AUpvsetvaltype(pvlist,  1, AU_PVTYPE_LONG);

        AUpvsetparam(pvlist, 2, AF_AWARE_PARAM_BITRATE_TARGET);
        AUpvsetvaltype(pvlist,  2, AU_PVTYPE_LONG);
  
        AFgetcompressionparams(file, AF_DEFAULT_TRACK, 
            &track_desc->compressiontype, pvlist, 3);

        AUpvgetval(pvlist, 0, &track_desc->aware_desc.layer);
        AUpvgetval(pvlist, 1, &track_desc->aware_desc.bitratepolicy);
        AUpvgetval(pvlist, 2, &track_desc->aware_desc.bitratetarget);

        AUpvfree(pvlist);
    }
#endif /*AWARE*/


    if (s == 0) { /* strlen dies on null strings, it seems */
        track_desc->compressionname = (char *)malloc(1);
        track_desc->compressionname[0] = '\0';
    }
    else {
       track_desc->compressionname = (char *)malloc(strlen(s)+1); 
       strcpy(track_desc->compressionname, s); 
    }

    track_desc->nmarks = AFgetmarkids(file, AF_DEFAULT_TRACK, (long *)0);

    track_desc->markid  = (long *)calloc(track_desc->nmarks, sizeof(long)); 
    track_desc->markpos = (long *)calloc(track_desc->nmarks, sizeof(long));
    track_desc->markname = (char **)calloc(track_desc->nmarks, sizeof(char *));

    AFgetmarkids(file, AF_DEFAULT_TRACK, track_desc->markid);

    for (i=0; i<track_desc->nmarks; i++)
    { 
       track_desc->markpos[i] 
          = AFgetmarkpos(file, AF_DEFAULT_TRACK, track_desc->markid[i]);
       s = AFgetmarkname(file, AF_DEFAULT_TRACK, track_desc->markid[i]);

       if (s == (char *)0) { /* strlen dies on null strings, it seems */
           track_desc->markname[i] = (char *)malloc(1);
           track_desc->markname[i][0] = '\0';
       }
       else {
           track_desc->markname[i] = (char *)malloc(strlen(s)+1);
           strcpy(track_desc->markname[i], s);
       }
    }
}

static void 
read_instrument_desc(file_desc_type *file_desc)
{
     instrument_desc_type *inst_desc;
     AFfilehandle file;

     if (file_desc->have_sampler_data)
     {
         inst_desc = &(file_desc->instrument_desc);
         file = file_desc->file;
    
         /*
          * get instrument parameters
          */
         inst_desc->midi_basenote =
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_MIDI_BASENOTE);
         inst_desc->midi_lonote = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_MIDI_LONOTE);
         inst_desc->midi_hinote = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_MIDI_HINOTE);
         inst_desc->midi_lovelocity = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_MIDI_LOVELOCITY);
         inst_desc->midi_hivelocity = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_MIDI_HIVELOCITY);
         inst_desc->numdbs_gain =
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_NUMDBS_GAIN);
         inst_desc->numcents_detune = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_NUMCENTS_DETUNE);
         inst_desc->sustain_loop_id =
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_SUSLOOPID);
         inst_desc->release_loop_id = 
             AFgetinstparamlong(file, AF_DEFAULT_INST, AF_INST_RELLOOPID);
   
         inst_desc->sustain_loop_start_mark_id =
             AFgetloopstart(file, AF_DEFAULT_INST, inst_desc->sustain_loop_id);
         inst_desc->sustain_loop_end_mark_id =
             AFgetloopend(file, AF_DEFAULT_INST, inst_desc->sustain_loop_id);
         inst_desc->sustain_loop_mode =
             AFgetloopmode(file, AF_DEFAULT_INST, inst_desc->sustain_loop_id);
   
         if (inst_desc->sustain_loop_start_mark_id > 0)
         {     
             inst_desc->sustain_loop_start_frame =
                  AFgetmarkpos(file, AF_DEFAULT_TRACK,
                    inst_desc->sustain_loop_start_mark_id);
             inst_desc->sustain_loop_end_frame =
                  AFgetmarkpos(file, AF_DEFAULT_TRACK,
                    inst_desc->sustain_loop_end_mark_id);
         }
         else
         {
             inst_desc->sustain_loop_start_frame = 0;
             inst_desc->sustain_loop_end_frame = 0;
         }
    
         inst_desc->release_loop_start_mark_id =
             AFgetloopstart(file, AF_DEFAULT_INST, inst_desc->release_loop_id);
         inst_desc->release_loop_end_mark_id =
             AFgetloopend(file, AF_DEFAULT_INST, inst_desc->release_loop_id);
         inst_desc->release_loop_mode =
             AFgetloopmode(file, AF_DEFAULT_INST, inst_desc->release_loop_id);
    
         if (inst_desc->release_loop_start_mark_id > 0)
         {     
         inst_desc->release_loop_start_frame =
             AFgetmarkpos(file, AF_DEFAULT_TRACK,
                    inst_desc->release_loop_start_mark_id);
         inst_desc->release_loop_end_frame =
             AFgetmarkpos(file, AF_DEFAULT_TRACK,
                    inst_desc->release_loop_end_mark_id);
         }
         else
         {
             inst_desc->release_loop_start_frame = 0;
             inst_desc->release_loop_end_frame = 0;
         }
    }     
}

static void 
read_misc_desc(file_desc_type *file_desc)
{
    int i;
    misc_desc_type *misc_desc;
    long *miscids;
    AFfilehandle file;
    int nread;
 
    file = file_desc->file;
    miscids = (long *)calloc(file_desc->num_misc, sizeof(long));
    AFgetmiscids(file, miscids);

    for (i=0; i<file_desc->num_misc; i++) 
    {
        misc_desc = &file_desc->misc_desc_list[i];

        misc_desc->miscid   = miscids[i];
        misc_desc->miscsize = AFgetmiscsize(file, misc_desc->miscid);
        misc_desc->misctype = AFgetmisctype(file, misc_desc->miscid); 

        switch (misc_desc->misctype)
        {
            case AF_MISC_AIFF_COPY:
            case AF_MISC_AIFF_AUTH:
            case AF_MISC_AIFF_NAME:
            case AF_MISC_AIFF_ANNO:
                misc_desc->misctext = (char *)malloc(misc_desc->miscsize + 1);
                nread = AFreadmisc(file, misc_desc->miscid, 
                           misc_desc->misctext, misc_desc->miscsize);
                misc_desc->misctext[misc_desc->miscsize] = '\0';
                break;

            default:
                break; 
        }
    }
}

static void
print_file_desc(file_desc_type *file_desc)
{
    printf("\n");
    switch(file_desc->filefmt)
    {
        case AF_FILE_AIFF: 
           printf("%s: %s [AIFF format]\n", myname, file_desc->filename);
           break;
        default:
        case AF_FILE_AIFFC: 
	   printf("%s: %s [AIFF-C format version %u]\n",
                   myname, file_desc->filename, file_desc->filefmtvers);
#ifdef NOTDEF
	   printf("%s note: I can't decipher" 
                  " AIFF-C comment chunks yet\n", spaces, myname); 
#endif /*NOTDEF*/
           break;
        
    }
}

static void
print_track_desc(file_desc_type *file_desc)
{
    track_desc_type *track_desc;
    long nframes;
    long minutes;
    float seconds;
    char *compressionstring;
    int i;
    static char g722string[] = "G.722";
    static char alawstring[] = "G.711 A-law";
    static char ulawstring[] = "G.711 u-law";
    static char nonestring[] = "none";
    static char unknownstring[] = "unknown";
#ifdef AWARE
    static char awarestring[256];
    char *algstring;
    char *layerstring;
    char bitratetargetstring[80];
    char *bitratepolicystring;
    static char nullstring[]             = "";
    static char awareconstqualstring[]   = "constant quality ";
    static char awarefixedratestring[]   = "fixed rate ";
    static char awarelosslessstring[]    = "lossless ";
    static char awarelayer1string[]      = "Layer I " ;
    static char awarelayer2string[]      = "Layer II ";
    static char awarempegstring[]        = "MPEG ";
    static char awaremultistring[]       = "MultiRate ";
#endif /*AWARE*/

    track_desc = &file_desc->track_desc;

    nframes = track_desc->nframes;
    seconds = (float)nframes / track_desc->rate;
    minutes = (long)(seconds / 60.0);
    seconds -= minutes * 60.0;
  
    printf("\n"); 
    printf("audio track information:\n");

    printf("%s number of interleaved channels = %d\n",
                spaces, track_desc->nchannels);
    printf("%s total number of sample frames  = %d\n", spaces, nframes);
    printf("%s sampling rate                  = %g sample frames/sec\n", 
                 spaces, track_desc->rate);
    if (minutes > 0) {
        printf("%s playing time                   = %d min : %g sec\n", 
                 spaces, minutes, seconds);
    }
    else {
        printf("%s playing time                   = %g sec\n", 
                 spaces, seconds);
    }

    switch (track_desc->sampfmt)
    {
        case AF_SAMPFMT_TWOSCOMP:
        printf("%s sample format                  = %d-bit signed linear PCM\n",
               spaces, track_desc->sampwidth);
            break;

        default:
        printf("%s sample format                  = UNKNOWN\n");
            break; 
    }

    if (track_desc->haveaesdata > 0) {
      printf("%s AES channel status data        = [%02x %02x %02x %02x]\n",
        spaces, 
        track_desc->aesdata[0],
        track_desc->aesdata[1],
        track_desc->aesdata[2],
        track_desc->aesdata[3]);
    }  
    else {
      printf("%s AES channel status data        = none\n", spaces);
    }

    if (file_desc->filefmt == AF_FILE_AIFFC) {
        switch (track_desc->compressiontype)
        {
            case AF_COMPRESSION_NONE:
               compressionstring = nonestring;
               break;
            case AF_COMPRESSION_G722:
               compressionstring = g722string;
               break;
            case AF_COMPRESSION_G711_ALAW:
               compressionstring = alawstring;
               break;
            case AF_COMPRESSION_G711_ULAW:
               compressionstring = ulawstring;
               break;
#ifdef AWARE
            case AF_COMPRESSION_AWARE_MPEG:      /* gotta clean this up */
            case AF_COMPRESSION_AWARE_MULTIRATE:
               if (track_desc->compressiontype == AF_COMPRESSION_AWARE_MPEG)
               {
                   algstring = awarempegstring;
                   if (track_desc->aware_desc.layer == AF_AWARE_LAYER_I) {
                       layerstring = awarelayer1string;
                   }
                   else if (track_desc->aware_desc.layer == AF_AWARE_LAYER_II) {
                       layerstring = awarelayer2string;
                   }
                   else /* unknown layer */ {
                       layerstring = nullstring; 
                   }
               }
               else /* AF_AWARE_COMPRESSION_MULTIRATE */
               {
                   algstring = awaremultistring;
                   layerstring = nullstring;
               }
               
               if (track_desc->aware_desc.bitratepolicy
                                                     ==AF_AWARE_CONST_QUAL) {
                   bitratepolicystring = awareconstqualstring;
               }
               else if(track_desc->aware_desc.bitratepolicy
                                                     ==AF_AWARE_FIXED_RATE) {
                   bitratepolicystring = awarefixedratestring;
               } 
               else if (track_desc->aware_desc.bitratepolicy
                                                     ==AF_AWARE_LOSSLESS) {
                   bitratepolicystring = awarelosslessstring;
               } 
               else {
                   bitratepolicystring = nullstring;
               }

               if ((track_desc->compressiontype == AF_COMPRESSION_AWARE_MPEG)
                &&(track_desc->aware_desc.bitratepolicy==AF_AWARE_FIXED_RATE)){
                   sprintf(bitratetargetstring, "%d kbps", 
                       track_desc->aware_desc.bitratetarget/1000);
               }
               else {
                   bitratetargetstring[0] = '\0';
               }

               sprintf(awarestring, "Aware %s%s%s%s", algstring, layerstring,
                                bitratepolicystring, bitratetargetstring);
               compressionstring = awarestring;
               break;
#endif /*AWARE*/
            default:
               compressionstring = unknownstring;
               break;
        } /* switch */
        printf("%s compression type               = %s\n", 
                                 spaces, compressionstring);
        printf("%s compression name               = \"%s\"\n", 
                                 spaces, track_desc->compressionname);
    }
    else
    {
        printf("%s compression type               = none\n", spaces);
    }

    if (track_desc->nmarks > 0)
    {
        printf("\n");
        printf("%s audio track markers            = %d \n",
                 spaces, track_desc->nmarks);
   
        for (i=0; i<track_desc->nmarks; i++)
        {
            printf("%s %s id =%4d  pos =%10d  name =\"%s\"\n",
             spaces, spaces,
             track_desc->markid[i], 
             track_desc->markpos[i],
             track_desc->markname[i]);
        }
    }
    else
    {
        printf("%s audio track markers            = NONE \n", spaces);
    }
}

static void
print_instrument_desc(file_desc_type *file_desc)
{
    static char noloopin[] = "no loop";
    static char forwmode[] = "forward";
    static char fwbwmode[] = "forward/backward";
    char *relmode;
    char *susmode;
    instrument_desc_type *instrument_desc;
    instrument_desc = &file_desc->instrument_desc;

    switch (instrument_desc->sustain_loop_mode) {
       default:
       case 0: susmode = noloopin; break;
       case 1: susmode = forwmode; break;
       case 2: susmode = fwbwmode; break;
    }
    switch (instrument_desc->release_loop_mode)
    {
       default:
       case 0: relmode = noloopin; break;
       case 1: relmode = forwmode; break;
       case 2: relmode = fwbwmode; break;
    }
     

    printf("\n");

    if (file_desc->have_sampler_data)
    {
        printf("sampler configuration:\n");
        printf("%s %s base note                    = MIDI note %d\n", 
                     spaces, spaces, instrument_desc->midi_basenote);
        printf("%s %s low note                     = MIDI note %d\n", 
                     spaces, spaces, instrument_desc->midi_lonote);
        printf("%s %s high note                    = MIDI note %d\n", 
                     spaces, spaces, instrument_desc->midi_hinote);
        printf("%s %s high vel                     = MIDI vel %d\n",
                     spaces, spaces, instrument_desc->midi_hivelocity);
        printf("%s %s low  vel                     = MIDI vel %d\n",
                     spaces, spaces, instrument_desc->midi_lovelocity);
        printf("%s %s gain                         = %d dB\n",
                     spaces, spaces, instrument_desc->numdbs_gain);
        printf("%s %s detune                       = %d cent\n",
                     spaces, spaces, instrument_desc->numcents_detune);
        printf("%s %s sustain loop mode mode       = %d [%s]\n",
                   spaces,spaces,instrument_desc->sustain_loop_mode,susmode);
        printf("%s %s sustain loop start marker id = %d [frame %d]\n",
                   spaces,spaces,instrument_desc->sustain_loop_start_mark_id,
                   instrument_desc->sustain_loop_start_frame);
        printf("%s %s sustain loop end marker id   = %d [frame %d]\n",
                   spaces,spaces,instrument_desc->sustain_loop_end_mark_id,
                   instrument_desc->sustain_loop_end_frame);
        printf("%s %s release loop mode mode       = %d [%s]\n",
                   spaces,spaces,instrument_desc->release_loop_mode,relmode);
        printf("%s %s release loop start marker id = %d [frame %d]\n",
                   spaces,spaces,instrument_desc->release_loop_start_mark_id,
                   instrument_desc->release_loop_start_frame);
        printf("%s %s release loop end marker id   = %d [frame %d]\n",
                   spaces,spaces,instrument_desc->release_loop_end_mark_id,
                   instrument_desc->release_loop_end_frame);
 
    }
    else
    {
        printf("no sampler data or loop points\n");
    } 
}

static void 
print_misc_desc(file_desc_type *file_desc)
{
    int i;


    if (file_desc->num_misc == 0)
    {
       printf("\n");
       printf("no text or other miscellaneous data\n");
       printf("\n");
       return;
    }

    printf("\n");
    printf("miscellaneous information:\n");

    for (i=0; i<file_desc->num_misc; i++)
    {
        switch (file_desc->misc_desc_list[i].misctype)
        { 
            case AF_MISC_AIFF_COPY:
               printf("%s copyright  = \"%s\"\n",
                         spaces, file_desc->misc_desc_list[i].misctext);
               break;
            case AF_MISC_AIFF_AUTH:
               printf("%s author     = \"%s\"\n",
                         spaces, file_desc->misc_desc_list[i].misctext);
               break;
            case AF_MISC_AIFF_ANNO:
               printf("%s annotation = \"%s\"\n",
                         spaces, file_desc->misc_desc_list[i].misctext);
               break;
            case AF_MISC_AIFF_NAME:
               printf("%s name       = \"%s\"\n",
                         spaces, file_desc->misc_desc_list[i].misctext);
               break;
            case AF_MISC_AIFF_MIDI:
               printf("%s %d bytes of MIDI exclusive data\n",
                         spaces, file_desc->misc_desc_list[i].miscsize);
               break;
            case AF_MISC_AIFF_APPL:
               printf("%s %d bytes of application specific data\n",
                         spaces, file_desc->misc_desc_list[i].miscsize);
               break;

            case AF_MISC_AIFF_UNRECOGNIZED:
               printf("%s %d bytes of unrecognized data\n",
                         spaces, file_desc->misc_desc_list[i].miscsize);
               break;
        }
    }
    printf("\n");
}
