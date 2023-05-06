#include <dmedia/audiofile.h>

#define FORWARD_SYNC_CODE 0xbffc
#define FORWARD_SYNC_MASK 0xffff
#define REVERSE_SYNC_CODE 0x3ffd
#define REVERSE_SYNC_MASK 0xffff

int ac;
char **av;
long totalframes;
short *samps;

unsigned long bitpat[3] = {0,0,0};

main(argc, argv)
int	argc;
char	**argv;
{
    int i, j, t;

    ac = argc;
    av = argv;
    getdata();
    if(ac==3)
        for(i=0;i<totalframes/2;i++)
	    {j=totalframes-i-1;t=samps[i];samps[i]=samps[j];samps[j]=t;}
    process_data();
}

process_data()
{
    int state, i, bit, spantype, c21, c10;
    int cellsize=1000, gotshort=0, oldstate=1, spanwidth=0;

    for(i=0;i<totalframes;i++) {

	/* Quantitize (with hysteresis) */
        if(state == 1)
	    state = (samps[i]+1000)>0;
	else
	    state = (samps[i]-1000)>0;

	/* Count the span width */
	if(state==oldstate){
	    spanwidth++;
	    continue;
        }

	/* Categorize spantype as either a long span or a short span */
        if (4 * spanwidth > 3 * cellsize) {
	    cellsize = spanwidth;   /* Input was a long span */
	    spantype=1;
        } else {
	    cellsize = 2 * spanwidth; /* Input was a short span */
	    spantype=0;
        }

	/* Reset the span length counter. */
	spanwidth=1;
	oldstate=state;

	/* Decode a span into a bit */
        if(spantype==1){
	    gotshort=0;
	    bit=0;
        } else {
	    if(gotshort) {
	        bit=1;
	        gotshort=0;
	    } else {
	        gotshort=1;
		continue;
	    }
	}

	/* Put the bit into the shift register */
        c21=bitpat[2]&1; /* shift carry from word 2 to word 1 */
        c10=bitpat[1]&1; /* shift carry from word 1 to word 0 */
        bitpat[2] >>= 1; bitpat[2] |= (bit<<15);
        bitpat[1] >>= 1; bitpat[1] |= (c21<<31);
        bitpat[0] >>= 1; bitpat[0] |= (c10<<31);

	/* Test the shift register for the span sync codes */
        if( (bitpat[2] & FORWARD_SYNC_MASK) == FORWARD_SYNC_CODE){
            printforwardcode();
	} else if( (bitpat[0] & REVERSE_SYNC_MASK) == REVERSE_SYNC_CODE){
            printreversecode();
	}
    }
}

printforwardcode()
{
    int ht,hu,mt,mu,st,su,ft,fu,
        ug1, ug2, ug3, ug4, ug5, ug6, ug7, ug8,
        df, cf,
        ua1, ua2, ua3, ua4,
        syn;
    fu =(bitpat[0]>>0)&0xf;
    ug1=(bitpat[0]>>4)&0xf;
    ft =(bitpat[0]>>8)&0x3;
    df =(bitpat[0]>>10)&0x1;
    cf =(bitpat[0]>>11)&0x1;
    ug2=(bitpat[0]>>12)&0xf;
    su =(bitpat[0]>>16)&0xf;
    ug3=(bitpat[0]>>20)&0xf;
    st =(bitpat[0]>>24)&0x7;
    ua1=(bitpat[0]>>27)&0x1;
    ug4=(bitpat[0]>>28)&0xf;

    mu =(bitpat[1]>>0)&0xf;
    ug5=(bitpat[1]>>4)&0xf;
    mt =(bitpat[1]>>8)&0x7;
    ua2=(bitpat[1]>>11)&0x1;
    ug6=(bitpat[1]>>12)&0xf;
    hu =(bitpat[1]>>16)&0xf;
    ug7=(bitpat[1]>>20)&0xf;
    ht =(bitpat[1]>>24)&0x3;
    ua3=(bitpat[1]>>26)&0x1;
    ua4=(bitpat[1]>>27)&0x1;
    ug8=(bitpat[1]>>28)&0xf;

    syn=(bitpat[2]>>0)&0xffff;

    printf("forward %d%d:%d%d:%d%d:%d%d ug:%x%x%x%x%x%x%x%x df:%x cf:%x ua:%x%x%x%x syn:%x\n",
	ht,hu,mt,mu,st,su,ft,fu,
	ug1, ug2, ug3, ug4, ug5, ug6, ug7, ug8,
	df, cf,
	ua1, ua2, ua3, ua4,
	syn
    );
}

printreversecode()
{
    unsigned long mypat[3];
    unsigned long lmask, rmask, *lword, *rword, lbit, rbit;
    int i;
    int ht,hu,mt,mu,st,su,ft,fu,
        ug1, ug2, ug3, ug4, ug5, ug6, ug7, ug8,
        df, cf,
        ua1, ua2, ua3, ua4,
        syn;
    
    /* Make a copy of the shift register */

    mypat[0]=bitpat[0];
    mypat[1]=bitpat[1];
    mypat[2]=bitpat[2];

    /* Bit-reverse the copy of the shift register */
   
    lmask=0x8000;
    rmask=1;
    lword=&mypat[2];
    rword=&mypat[0];
    for(i=0;i<40;i++) {
	lbit = (*lword&lmask) != 0;
	rbit = (*rword&rmask) != 0;
	*lword &= ~lmask;
	*rword &= ~rmask;
	if(lbit)*rword |= rmask;
	if(rbit)*lword |= lmask;
	lmask>>=1;
	if(lmask==0) {
	    lmask=0x80000000;
	    lword= &mypat[1];
	}
	rmask<<=1;
	if(rmask==0){
	    rmask=1;
	    rword= &mypat[1];
	}
    }
    fu =(mypat[0]>>0)&0xf;
    ug1=(mypat[0]>>4)&0xf;
    ft =(mypat[0]>>8)&0x3;
    df =(mypat[0]>>10)&0x1;
    cf =(mypat[0]>>11)&0x1;
    ug2=(mypat[0]>>12)&0xf;
    su =(mypat[0]>>16)&0xf;
    ug3=(mypat[0]>>20)&0xf;
    st =(mypat[0]>>24)&0x7;
    ua1=(mypat[0]>>27)&0x1;
    ug4=(mypat[0]>>28)&0xf;

    mu =(mypat[1]>>0)&0xf;
    ug5=(mypat[1]>>4)&0xf;
    mt =(mypat[1]>>8)&0x7;
    ua2=(mypat[1]>>11)&0x1;
    ug6=(mypat[1]>>12)&0xf;
    hu =(mypat[1]>>16)&0xf;
    ug7=(mypat[1]>>20)&0xf;
    ht =(mypat[1]>>24)&0x3;
    ua3=(mypat[1]>>26)&0x1;
    ua4=(mypat[1]>>27)&0x1;
    ug8=(mypat[1]>>28)&0xf;

    syn=(mypat[2]>>0)&0xffff;

    printf("reverse %d%d:%d%d:%d%d:%d%d ug:%x%x%x%x%x%x%x%x df:%x cf:%x ua:%x%x%x%x syn:%x\n",
	ht,hu,mt,mu,st,su,ft,fu,
	ug1, ug2, ug3, ug4, ug5, ug6, ug7, ug8,
	df, cf,
	ua1, ua2, ua3, ua4,
	syn
    );
}

printbits(pat)
unsigned long pat[3];
{
    unsigned int msk,i;
    msk=0x8000;
    for(i=0;i<16;i++){printf("%d",(pat[2]&msk) != 0); msk/=2; };
    printf(" ");
    msk=0x80000000;
    for(i=0;i<32;i++){printf("%d",(pat[1]&msk) != 0); msk/=2; };
    printf(" ");
    msk=0x80000000;
    for(i=0;i<32;i++){printf("%d",(pat[0]&msk) != 0); msk/=2; };
    printf("\n");
}

getdata()
{
    char	*audiofilename;
    long	vers, filefmt, channels, sampwidth, samptype, n;
    double	audiorate;
    int nread;
    AFfilehandle audiofile;
    audiofilename=av[1];
    audiofile = AFopenfile(audiofilename, "r", AF_NULL_FILESETUP);
    if (audiofile == AF_NULL_FILEHANDLE) {
	printf("%s: couldn't open %s\n", av[0], audiofilename);
	exit(1);
    }
    filefmt = AFgetfilefmt(audiofile, &vers);
    totalframes = AFgetframecnt(audiofile, AF_DEFAULT_TRACK);
    audiorate = AFgetrate(audiofile, AF_DEFAULT_TRACK);
    channels = AFgetchannels(audiofile, AF_DEFAULT_TRACK);
    AFgetsampfmt(audiofile, AF_DEFAULT_TRACK, &samptype, &sampwidth);
    printf("name %s\n", audiofilename);
    printf("format %d vers %d\n",filefmt,vers);
    printf("frames %d rate %f channels %d samptype %d sampwidth %d\n",totalframes,
	    audiorate, channels, samptype, sampwidth);
    samps=(short *)malloc(2*totalframes);
    nread = AFreadframes(audiofile, AF_DEFAULT_TRACK, samps, totalframes); 
}
