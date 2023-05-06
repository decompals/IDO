#ifdef RASTERIMAGE	/* entire file */
/*
|| get image data from rasterfiles.
||
|| Copyright 1988, 1989, by
||   Gene Dronek (Vulcan Laboratory) and
||   Rich Morin  (Canta Forda Computer Laboratory).
|| Freely redistributable as long as this notice is preserved.
||
|| Help from Hugh Daniel, Gene Dronek, and John Gilmore.
*/
#ident "rasterimg.c: $Revision: 1.1 $"

#define ESCAPE (0x80)		  /* Escape char for byte-encoded image	*/

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include "rasterfile.h"

extern int bytes, iheight, iwidth;
extern int dsdebug;
#define DSDBG(x) {if (dsdebug) {x;}}

struct rasterfile r_hdr;

/* just a stub for rasterfiles */
endimg()
{
}

/* get the header of a rasterfile image (such as
	/usr/NeWS/smi/globes/*.im1)  */
get_hdr()
{
	int n_read;
	unsigned char *colormap;

	DSDBG( fprintf(stderr,">>> get_hdr\n") )

	n_read = fread(&r_hdr, 1, sizeof r_hdr, stdin);

	if (n_read <  0) {
		perror("iisc");
		ds_zot("iisc: error reading header");
	}
	if (n_read == 0)			return(0);
	if (n_read != sizeof r_hdr)		ds_zot("iisc: header too small");

	DSDBG( fprintf(stderr, "ras_magic:	%x\n", r_hdr.ras_magic);
	    fprintf(stderr, "ras_height:	%d\n", r_hdr.ras_height);
	    fprintf(stderr, "ras_width:	%d\n", r_hdr.ras_width);
	    fprintf(stderr, "ras_depth:	%d\n", r_hdr.ras_depth);
	    fprintf(stderr, "ras_type:		%d\n", r_hdr.ras_type);
	    fprintf(stderr, "ras_maptype:	%d\n", r_hdr.ras_maptype); )

	    if (r_hdr.ras_magic   != RAS_MAGIC)	ds_zot("iisc: invalid magic number");

	switch (r_hdr.ras_type) {
		case RT_STANDARD:
		case RT_OLD:
		case RT_BYTE_ENCODED:
			break;	/* these 3 are supported */
		default:
			ds_zot("iisc: unsupported rasterfile type");
			break;
	}

	if (r_hdr.ras_depth   != 1)		ds_zot("iisc: invalid image depth");
	iheight = r_hdr.ras_height;
	iwidth = r_hdr.ras_width;

	/* Handle color maps -- currently, read and skip and ignore it.  */
	colormap = NULL;
	if (r_hdr.ras_maplength > 0) {
		colormap = (unsigned char *)malloc((unsigned)r_hdr.ras_maplength);
		if (colormap == NULL)
			ds_zot("iisc: No memory for colormap, really!");
		n_read = fread(colormap, 1, r_hdr.ras_maplength, stdin);
		if (n_read != r_hdr.ras_maplength)
			ds_zot("iisc: Couldn't read whole colormap");
	}

	switch (r_hdr.ras_maptype) {		/* handle map types... */
	case RMT_EQUAL_RGB:
	case RMT_NONE:
	case RMT_RAW:
		break;

	default:
		ds_zot("iisc: invalid map type");
	}

	bytes = (((r_hdr.ras_width*r_hdr.ras_depth)+15)/16)*2;
	return(1);
}


get_ber(in_buf, bcnt, input)
  unsigned char in_buf[];
  int bcnt;
  FILE *input;
{
  register unsigned char
    *end, *out;

  register int
    abyte, count, duped;

#ifdef DEBUG
  static int cnt=0;
  if (++cnt == 1)
    fprintf(stderr,">>> get_ber (1st call)\n");
#endif DEBUG
	switch (r_hdr.ras_type) {
	case RT_STANDARD:
	case RT_OLD:
		return count = fread(in_buf, 1, bcnt, input);
		break;

	case RT_BYTE_ENCODED:
		break;	/* handle below */

	default:
		ds_zot("unsupported rasterfile type");
		break;
	}

  out = in_buf;
  end = out + bcnt;
  while (out < end) {
    switch (abyte = getc(input)) {
      case EOF:
        ds_zot("get_ber: EOF (1) on input data");
        break;

      case ESCAPE:
        switch (count = getc(input)) {

          case EOF:
            ds_zot("get_ber: EOF (2) on input data");
	    break;

          case 0:
            *out++ = ESCAPE;
            break;

          default:
            switch (duped = getc(input)) {

              case EOF:
                ds_zot("get_ber: EOF (3) on input data");

              default:
                count++;
                while (--count >= 0 && out < end)
                  *out++ = duped;
	          break;
            }
	    break;
        }
	break;

      default:
        *out++ = abyte;
	break;
    }					     /* End switch on each char	*/
  }				           /* End loop around all chars	*/
  return(bcnt);
}
#endif RASTERIMAGE	/* entire file */
