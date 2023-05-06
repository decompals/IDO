/* a60_rep.c      1991 Abekas Video Systems  - Simon Carter */
/*                                                          */
/* A60_rep.c is one of two files which form the basis of an */
/* RS-232 serial control interface for the A60 or A64 from  */
/* an SGI 4D.  The other routine is serial.c.               */
/*                                                          */
/* This routine will parse a reponse packet from an A60 or  */
/* A64 most respones are skipped.  The normal length is a   */
/* response is four bytes, execptions are noted.  The A60   */
/* generally supports a subset of the A64 commands and      */
/* responses.  Conditional compilation is used to resolve   */
/* irreconcilable differences.                              */

#define DEBUG 1
#define TRACE 1
#define A60

#include "responses.h"

/* extra odds and ends (probably not even implemented */
#define FLPYCHNK 40           /* floppy chunk size from comdef6.h */
#define GRPHSUP 0x20          /* graphics processor setup bits grph64.h */
#define CURSVAL 0x18          /* cursor value grph64.h */
#define SHDEPTH 0x56          /* shadow depth hl4-key.h */
#define SHSTAT  0x57          /* shadow status hl4-key.h */
#define SHTHRES 0x58          /* shadow threshold hl4-key.h */

struct state 
    {
    int position;	/* add to this as you need more info */
    int mode;
    } state;

decodeReply(rxptr)
char *rxptr;
{
char *msgptr;
char *packetEnd;
int type, i, *iptr;
int temp;
int value;
int len;

msgptr = rxptr;
len = *msgptr++;			/* character count */

packetEnd = msgptr + len - 2; 		/* should point at \r */

while (msgptr < packetEnd)
    {
    type = *msgptr++;
    value = *(msgptr + 2) & 0x80 ? 0xff : 0;      /* sign-extend */
    value = (value << 8) | *(msgptr + 2);
    value = (value << 8) | *(msgptr + 1);
    value = (value << 8) | *msgptr;	  /* first byte == LSB */
    msgptr += 3;

    if(TRACE) printf("%02X %X\n", type, value);

    switch ( type )
	{
	case AT:		/* Current field number */
	    state.position = value;
	    if(DEBUG) printf("at:%lX\n", state.position);
	    break;

	case AUTORT: 		/* Auto Transition Rate (A64) */
	case BKPINFO:		/* Backup Length and Status (A64) */
	case BKUPBAS:		/* base for backup (A64) */
	    break;
	case CLOKDAT:		/* Current record lock segment data */
	    msgptr += 3;
	    break;

	case CMACCMD:		/* Current macro command */
	case CMACNUM:		/* Current macro number */
	case CMACPRM:		/* Current macro parameter */
	    break;

	case CSEGDAT:  		/* Current Segment Data */
#ifdef A64
	    msgptr += 6;
#endif
#ifdef A60
	    msgptr += 7;	/* Different from manual !! */
	    /* 2 bytes - seg # (bit 14==pause) */
	    /* 3 bytes in point */
	    /* 3 bytes out point */
	    /* 2 bytes speed */
	    /* this also applies to NSEGDAT and LOOPDAT */
#endif
	    break;

	case DRVSTAT:   	/* Drive Status (A64) */
	case EDFLDSW:		/* Edit field switches (A64) */
	case ENGSUP:		/* Framestore setup and Misc bits */
	    break;

	case ERRLST:  		/* Error list - just skip */
	    msgptr += 3;
	    break;

	case ERRSUM:     	/* Error summary - just skip */
	    msgptr += 10;
	    break;          

	case FADEVAL: 		/* Fade Value (A64) */
	case FGHUE: 		/* Foreground Hue (A64) */
	case FGLUM:  		/* Foreground Luminance (A64) */
	case FGSAT:  		/* Foreground Saturation (A64) */
	    break;

	case FLPYACC:		/* Macro <-> Floppy disk Transfer (A64) */
	    /* skip 40 bytes of data if a write floppy response */
	    if ((value & 0xFF) == 3)
		msgptr += FLPYCHNK;
	    break;

	case FSSUP:   		/* Framestore Setup (A64) */
	case GENEND:		/* Generic last Segment */
	case GENIN:    		/* Generic In Point */
	case GENOUT:  		/* Generic Out Point */
	case GENSTRT: 		/* Generic Start segment */
	case GPI1ASN:
	case GPI2ASN:
	case GPI3ASN:
	case GPI4ASN:
	    break;

	case GRPHPRM: 		/* Graphics processor parameters (A64) */
	    switch(value & 0xFF) 
		{
		case CURSVAL:	/* cursor values */
		    msgptr += 6;
		    break;

		case GRPHSUP:
		    break;
		default:
		    printf("Unknown GRPH param %X\n", value & 0xFF);
		    msgptr = packetEnd; /* punt */
		}
	    break;

	case INPCGN:		/* Input Croma gain (A64) */
	case INPHKP:		/* Input horizontal key phase (A64) */
	case INPHPS:		/* Input horizontal phase (A64) */
	case INPVKP:		/* input vertical key position (A64) */
	case INPVBLK:   	/* Video blanking (A64) */
	case INPVGN:
	case KEYCLP:
	case KEYDIS:
	case KEYGN:
	case KEYPH:
	case KEYSUP:
	case KEYTYP:
	    break;

	case LOOPDAT:
#ifdef A64
	    msgptr += 6;
#endif
#ifdef A60
	    msgptr += 7;	/* */
#endif
	    break;

	case MSKGN:		/* Mask Gain (A64) */
	case MSKSUP:		/* Mask Setup (A64) */
	    break;

	case NLOKDAT: 		/* next record lock segment data */
	    msgptr += 3;
	    break;

	case NSEGDAT:           /* Next segment data */
#ifdef A64
	    msgptr += 6;
#endif
#ifdef A60
	    msgptr += 7;	/* */
#endif
	    break;

	case OCRSPH:
	case OFNPH:
	case OFSET:
	case OUTSUP:
	    break;

	case OPMODES:
	    state.mode = value;
	    if(DEBUG) printf("mode:%lX\n", state.mode);
	    break;

	case PLA_BAS:
	case PLSP_ST:
	case REC_BAS:
	case RECINFO:
	case REM_SUP:
	case SEGCNT:
	case SHDEPTH:
	case SHSTAT:
	case SHTHRES:
	case SW_VNUM:
	case TTRIG:
	    break;
#ifdef A64

	case TPOINTS:		/* A64 */
	    msgptr += 12;
	    break;
#endif
#ifdef A60

	case TCPDAT:		/* A60 */
	    msgptr += 3;
	    break;
#endif
	case VITCLN:
	case DECINFO:
	case DOCRPH:
	case DOFNPH:
	case DOSCPH:
	    break;

	default:          /* force abort until next field  */
	    if(DEBUG) printf("unknown response :%02X\n", type);
	    printf("%X %X %X\n", rxptr, msgptr, packetEnd);
	    msgptr = packetEnd;
	    break;
	}
    }
if(*msgptr != '\r')
    printf("** framing error\n");
}

