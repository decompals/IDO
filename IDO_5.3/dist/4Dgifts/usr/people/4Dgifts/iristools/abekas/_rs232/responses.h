/* responses.h    1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* This is the header file for a60_rep.c.  It contains      */
/* definitions required for parsing the responses from an   */
/* A60 or A64.                                              */
/*                                                          */

        /* current modes */

#define AT      0x08    /* current field number */
#define OPMODES 0x0c    /* see MODE BITS */
#define PLSP_ST 0x0f    /* current play speed and status.  See STATUS BITS */

        /* segment list info */

#define CSEGDAT 0x14    /* current segment data (see note 1) */
#define NSEGDAT 0x18    /* next segment data (see note 1) */
#define SEGCNT  0x1c    /* segment count */

        /* current parameters */

#define AUTORT  0x21    /* autotrans rate */
#define BGHUE   0x22    /* background: hue */
#define BGLUM   0x23    /* background: luminance */
#define BGSAT   0x24    /* background: saturation */
#define	BKPINFO	0x25	/* backup length & status (see BACKUP COMMAND/STATUS) */
#define	BKUPBAS	0x26	/* base for streamer backup */
#define	DECINFO	0x27	/* decoder: type & status (see DECODER TYPE/STATUS) */
#define	DOCRPH	0x28	/* decoder: output course phase */
#define	DOFNPH	0x29	/* decoder: output fine phase */
#define	DOSCPH	0x2a	/* decoder: output sc phase */
#define DRVSTAT 0x2b    /* drive status (see DRIVE STATUS VALUES) */
#define ENGSUP  0x2c    /* engineering setup (SEE ENGINEERING SETUP BITS) */
#define FADEVAL 0x2d    /* fade value */
#define FGHUE   0x2e    /* foreground: hue */
#define FGLUM   0x2f    /* foreground: luminance */
#define FGSAT   0x30    /* foreground: saturation */
#define FSSUP   0x31    /* frame store setup (see FRAME STORE SETUP BITS) */
#define GPI1ASN 0x32    /* GPI #1 (in) assignment */
#define GPI2ASN 0x33    /* GPI #2 (in) assignment */
#define GPI3ASN 0x34    /* GPI #3 (in) assignment */
#define GPI4ASN 0x35    /* GPI #4 (in) assignment */
#define INPCGN  0x37    /* input: chroma gain */
#define INPHKP  0x38    /* input: horizontal key position */
#define INPHPS  0x39    /* input: horizontal position */
#define INPVBLK 0x3a    /* input: video black */
#define INPVGN  0x3b    /* input: video gain */
#define INPVKP  0x3c    /* input: vetical key position */
#define KEYCLP  0x3e    /* key: clip */
#define KEYDIS  0x3f    /* key: discrimination */
#define KEYGN   0x40    /* key: gain */
#define KEYPH   0x41    /* key: phase */
#define KEYSUP  0x42    /* key: setup (see KEY SETUP BITS) */
#define KEYTYP  0x43    /* key: type */
#define MIXTRN  0x45    /* mixer trans type (0 => dissolve, 1 => key) */
#define OFSET   0x47    /* time offset for kbd in frame display mode */
#define MSKGN   0x48    /* mask: gain */
#define MSKSUP  0x49    /* mask: setup (SEE MASK SETUP BITS) */
#define OCRSPH  0x4b    /* output: course phase */
#define OFNPH   0x4c    /* output: fine phase */
#define	OUTSUP	0x4d	/* output: setup (see OUTPUT SETUP BITS) */
#define PLA_BAS 0x4f    /* play base for simultaneous record/play */
#define REC_BAS 0x50    /* record base for simultaneous record/play */
#define RECINFO 0x51    /* record length and setup (see RECORD SETUP bits) */
#define VIDDAT 	0x56    /* video data (see VIDEO DATA BITS) */

        /* current definitions */

#define	CLOKDAT	0x5c	/* current record-lock segment data (see note 12) */
#define GENEND  0x5e    /* generic end # (see note 2) */
#define GENIN   0x5f    /* generic in-point (see note 3) */
#define GENOUT  0x60    /* generic out-point (see note 3) */
#define GENSTRT 0x61    /* generic start # (see note 2) */
#define LOOPDAT 0x63    /* disk loop data (see note 1) */
#define	NLOKDAT	0x65	/* next record-lock segment data (see note 12) */
#define TCPDAT  0x66  

        /* misc. info */

#define CMACCMD 0x68    /* pending macro cmd (low byte) and step# (high byte) */
#define CMACNUM 0x69    /* current macro number (see MACRO STATUS BITS) */
#define CMACPRM 0x6a    /* currently pending macro parameter */
#define EDFLDSW 0x6c    /* edit field switches - see note #10 */
#define ERRLST  0x6d    /* error list entry - see note #4 */
#define ERRSUM  0x6e    /* error list summary - see note # 5 */
#define TTRIG   0x6f    /* time for time code triggered play of machine */
#define FLPYACC 0x70    /* data key access - see note # 6 */
#define	GRPHPRM	0x72	/* current graphics parameter set (see note #11) */
#define REM_SUP 0x74    /* remote port setup (see SETREMT) */
#define SW_VNUM 0x76    /* software version numbers - see note #7 */
#define	VITCLN	0x78	/* current VITC line number - see note #8 */

