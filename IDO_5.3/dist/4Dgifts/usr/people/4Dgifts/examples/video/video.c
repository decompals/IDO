/* video.c
 * Scott Carr - '87
 *
 * Modified by: Sammy Wilborn - July, '88
 *
 * diag program for testing the CG2 and DE3 registers (video overlay)
 * in various Standalone video modes as well as Genlock vido modes.
 */

#include <stdio.h>
#include <fcntl.h>
#include <get.h>
#include <gl/gl.h>
#include <gl/addrs.h>
#include <gl/cg2vme.h>
#include <gl/glerror.h>

#define DEFINE_ERRS 1

main()
{
	int fd;
	int reg;
	int val;
	int items;
	char c;
	char line[80];
	char *montype();


	foreground();	/* keep input focus on current terminal */
	noport();
	winopen("video");

	printf("current getmonitor() is %s, getothermonitor() is %s\n",
			montype(getmonitor()),montype(getothermonitor()));
	printf("enter r/w/g register number [value] (0-4,0-DE r1 1-mode 2-io 3-phase 4-phase)\n");
	while ( read(0, line, 80) > 0 ) {
		items = sscanf(line,"%c %d %x",&c, &reg, &val);
		switch ( c ) {
		case 'r':
			if ( items < 2 ) {
				printf("need to specify register on read\n");
				break;
			}
			printf("value of %d is  0x%x\n",reg,getvideo(reg));
			break;
		case 'w':
			if ( items < 3 ) {
				printf("need to specify register and value on write\n");
				break;
			}
			printf("write a 0x%x to reg %d\n",val,reg);
			setvideo(reg,val);
			break;
		case 'q':
			exit(0);
			break;
		case 'g':
			printf("current getmonitor() is %s\n",
				montype(getmonitor()));
			break;
		default:
			printf("<%c> unknown command\n",c);
		}
	}
}

char *montype(monitor)
{

	switch ( monitor ) {

	case HZ30:
		return("HZ30");
		break;
	case HZ60:
		return("HZ60");
		break;
	case NTSC:
		return("NTSC");
		break;
	case HZ50:
		return("HZ50");
		break;
	case MONA:
		return("MONA");
		break;
	case MONB:
		return("MONB");
		break;
	case MONC:
		return("MONC");
		break;
	case MOND:
		return("MOND");
		break;
	case PAL:
		return("PAL");
		break;
	case HZ30_SG:
		return("HZ30_SG");
		break;
	case MON_ALL:
		return("MON_ALL");
		break;
	case MON_GEN_ALL:
		return("MON_GEN_ALL");
		break;
	case MONSPECIAL:
		return("MONSPECIAL");
		break;
	default:
		return("unknown");
		break;
	}
}
