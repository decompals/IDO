#ifndef __REGEXP_H__
#define __REGEXP_H__
#ifdef __cplusplus
extern "C" {
#endif
/*
*
* Copyright 1992, Silicon Graphics, Inc.
* All Rights Reserved.
*
* This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
* the contents of this file may not be disclosed to third parties, copied or
* duplicated in any form, in whole or in part, without the prior written
* permission of Silicon Graphics, Inc.
*
* RESTRICTED RIGHTS LEGEND:
* Use, duplication or disclosure by the Government is subject to restrictions
* as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
* and Computer Software clause at DFARS 252.227-7013, and/or in similar or
* successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
* rights reserved under the Copyright Laws of the United States.
*/
/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc.	*/
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF     	*/
/*	UNIX System Laboratories, Inc.                     	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/
#ident  "$Revision: 1.15 $"


#include <string.h>

#ifdef _LANGUAGE_C_PLUS_PLUS
#define _SET(a,b) a = b
#define _STATIC static
#else
#define _SET(a,b)
#define _STATIC
#endif

#define	CBRA	2
#define	CCHR	4
#define	CDOT	8
#define	CCL	12
#define	CXCL	16
#define	CDOL	20
#define	CCEOF	22
#define	CKET	24
#define	CBACK	36
#define NCCL	40

#define	STAR	01
#define RNGE	03

#define	NBRA	9

#define PLACE(c)	ep[c >> 3] |= bittab[c & 07]
#define ISTHERE(c)	(ep[c >> 3] & bittab[c & 07])
#define ecmp(s1, s2, n)	(!strncmp(s1, s2, n))

_STATIC char *compile(char *instring, char *ep,char *endbuf, int seof);
_STATIC int step(char *p1, char *p2);
_STATIC int advance(char *lp,char *ep);
static void getrnge(char *str);

static char	*braslist[NBRA];
static char	*braelist[NBRA];
_STATIC int	sed, nbra;
_STATIC char	*loc1, *loc2, *locs;
static int	nodelim;

_STATIC int	circf;
static int	low;
static int	size;

static unsigned char	bittab[] = { 1, 2, 4, 8, 16, 32, 64, 128 };

_STATIC char *
compile(char *instring, char *ep,char *endbuf, int seof)
{
	INIT	/* Dependent declarations and initializations */
	register c;
	register eof = seof;
	char *lastep = instring;
	int cclcnt;
	char bracket[NBRA], *bracketp;
	int closed;
	int neg;
	int lc;
	int i, cflg;
	int iflag; /* used for non-ascii characters in brackets */

	lastep = 0;
	if((c = GETC()) == eof || c == '\n') {
		if(c == '\n') {
			UNGETC(c);
			nodelim = 1;
		}
		if(*ep == 0 && !sed)
			ERROR(41);
		RETURN(ep);
	}
	bracketp = bracket;
	circf = closed = nbra = 0;
	if(c == '^')
		circf++;
	else
		UNGETC(c);
	while(1) {
		if(ep >= endbuf)
			ERROR(50);
		c = GETC();
		if(c != '*' && ((c != '\\') || (PEEKC() != '{')))
			lastep = ep;
		if(c == eof) {
			*ep++ = CCEOF;
			if (bracketp != bracket)
				ERROR(42);
			RETURN(ep);
		}
		switch(c) {

		case '.':
			*ep++ = CDOT;
			continue;

		case '\n':
			if(!sed) {
				UNGETC(c);
				*ep++ = CCEOF;
				nodelim = 1;
				if(bracketp != bracket)
					ERROR(42);
				RETURN(ep);
			}
			else ERROR(36);
		case '*':
			if(lastep == 0 || *lastep == CBRA || *lastep == CKET)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if(PEEKC() != eof && PEEKC() != '\n')
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			if(&ep[17] >= endbuf)
				ERROR(50);

			*ep++ = CCL;
			lc = 0;
			for(i = 0; i < 16; i++)
				ep[i] = 0;

			neg = 0;
			if((c = GETC()) == '^') {
				neg = 1;
				c = GETC();
			}
			iflag = 1;
			do {
				c &= 0377;
				if(c == '\0' || c == '\n')
					ERROR(49);
				if((c & 0200) && iflag) {
					iflag = 0;
					if(&ep[32] >= endbuf)
						ERROR(50);
					ep[-1] = CXCL;
					for(i = 16; i < 32; i++)
						ep[i] = 0;
				}
				if(c == '-' && lc != 0) {
					if((c = GETC()) == ']') {
						PLACE('-');
						break;
					}
					if((c & 0200) && iflag) {
						iflag = 0;
						if(&ep[32] >= endbuf)
							ERROR(50);
						ep[-1] = CXCL;
						for(i = 16; i < 32; i++)
							ep[i] = 0;
					}
					while(lc < c ) {
						PLACE(lc);
						lc++;
					}
				}
				lc = c;
				PLACE(c);
			} while((c = GETC()) != ']');
			
			if(iflag)
				iflag = 16;
			else
				iflag = 32;
			
			if(neg) {
				if(iflag == 32) {
					for(cclcnt = 0; cclcnt < iflag; cclcnt++)
						ep[cclcnt] ^= 0377;
					ep[0] &= 0376;
				} else {
					ep[-1] = NCCL;
					/* make nulls match so test fails */
					ep[0] |= 01;
				}
			}

			ep += iflag;

			continue;

		case '\\':
			switch(c = GETC()) {

			case '(':
				if(nbra >= NBRA)
					ERROR(43);
				*bracketp++ = nbra;
				*ep++ = CBRA;
				*ep++ = nbra++;
				continue;

			case ')':
				if(bracketp <= bracket) 
					ERROR(42);
				*ep++ = CKET;
				*ep++ = *--bracketp;
				closed++;
				continue;

			case '{':
				if(lastep == (char *) 0)
					goto defchar;
				*lastep |= RNGE;
				cflg = 0;
			nlim:
				c = GETC();
				i = 0;
				do {
					if('0' <= c && c <= '9')
						i = 10 * i + c - '0';
					else
						ERROR(16);
				} while(((c = GETC()) != '\\') && (c != ','));
				if(i >= 255)
					ERROR(11);
				*ep++ = i;
				if(c == ',') {
					if(cflg++)
						ERROR(44);
					if((c = GETC()) == '\\')
						*ep++ = 255;
					else {
						UNGETC(c);
						goto nlim;
						/* get 2'nd number */
					}
				}
				if(GETC() != '}')
					ERROR(45);
				if(!cflg)	/* one number */
					*ep++ = i;
				else if((ep[-1] & 0377) < (ep[-2] & 0377))
					ERROR(46);
				continue;

			case '\n':
				ERROR(36);

			case 'n':
				c = '\n';
				goto defchar;

			default:
				if(c >= '1' && c <= '9') {
					if((c -= '1') >= closed)
						ERROR(25);
					*ep++ = CBACK;
					*ep++ = c;
					continue;
				}
			}
	/* Drop through to default to use \ to turn off special chars */

		defchar:
		default:
			lastep = ep;
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}

_STATIC int step(char *p1, char *p2)
{
	register c;


	if(circf) {
		loc1 = p1;
		return(advance(p1, p2));
	}
	/* fast check for first character */
	if(*p2 == CCHR) {
		c = p2[1];
		do {
			if(*p1 != c)
				continue;
			if(advance(p1, p2)) {
				loc1 = p1;
				return(1);
			}
		} while(*p1++);
		return(0);
	}
		/* regular algorithm */
	do {
		if(advance(p1, p2)) {
			loc1 = p1;
			return(1);
		}
	} while(*p1++);
	return(0);
}

_STATIC int advance(char *lp, char *ep)
{
	register char *curlp;
	int c;
	char *bbeg; 
	register char neg;
	int ct;

	while(1) {
		neg = 0;
		switch(*ep++) {

		case CCHR:
			_SET(loc2,lp);
			if(*ep++ == *lp++)
				continue;
			return(0);
	
		case CDOT:
			_SET(loc2,lp);
			if(*lp++)
				continue;
			return(0);
	
		case CDOL:
			_SET(loc2,lp);
			if(*lp == 0)
				continue;
			return(0);
	
		case CCEOF:
			loc2 = lp;
			return(1);
	
		case CXCL: 
			_SET(loc2,lp);
			c = (unsigned char)*lp++;
			if(ISTHERE(c)) {
				ep += 32;
				continue;
			}
			return(0);
		
		case NCCL:	
			neg = 1;

		case CCL: 
			_SET(loc2,lp);
			c = *lp++;
			if(((c & 0200) == 0 && ISTHERE(c)) ^ neg) {
				ep += 16;
				continue;
			}
			return(0);
		
		case CBRA:
			braslist[*ep++] = lp;
			continue;
	
		case CKET:
			braelist[*ep++] = lp;
			continue;
	
		case CCHR | RNGE:
			c = *ep++;
			getrnge(ep);
			while(low--) {
				_SET(loc2,lp);
				if(*lp++ != c)
					return(0);
				}
			curlp = lp;
			while(size--) {
				_SET(loc2,lp);
				if(*lp++ != c)
					break;
				}
			if(size < 0)
				lp++;
			ep += 2;
			goto star;
	
		case CDOT | RNGE:
			getrnge(ep);
			while(low--) {
				_SET(loc2,lp);
				if(*lp++ == '\0')
					return(0);
				}
			curlp = lp;
			while(size--) {
				_SET(loc2,lp);
				if(*lp++ == '\0')
					break;
				}
			if(size < 0)
				lp++;
			ep += 2;
			goto star;
	
		case CXCL | RNGE:
			getrnge(ep + 32);
			while(low--) {
				_SET(loc2,lp);
				c = (unsigned char)*lp++;
				if(!ISTHERE(c))
					return(0);
			}
			curlp = lp;
			while(size--) {
				_SET(loc2,lp);
				c = (unsigned char)*lp++;
				if(!ISTHERE(c))
					break;
			}
			if(size < 0)
				lp++;
			ep += 34;		/* 32 + 2 */
			goto star;
		
		case NCCL | RNGE:
			neg = 1;
		
		case CCL | RNGE:
			getrnge(ep + 16);
			while(low--) {
				_SET(loc2,lp);
				c = *lp++;
				if(((c & 0200) || !ISTHERE(c)) ^ neg)
					return(0);
			}
			curlp = lp;
			while(size--) {
				_SET(loc2,lp);
				c = *lp++;
				if(((c & 0200) || !ISTHERE(c)) ^ neg)
					break;
			}
			if(size < 0)
				lp++;
			ep += 18; 		/* 16 + 2 */
			goto star;
	
		case CBACK:
			bbeg = braslist[*ep];
			ct = braelist[*ep++] - bbeg;
	
			if(ecmp(bbeg, lp, ct)) {
				lp += ct;
				continue;
			}
			return(0);
	
		case CBACK | STAR:
			bbeg = braslist[*ep];
			ct = braelist[*ep++] - bbeg;
			curlp = lp;
			_SET(loc2,lp);
			while(ecmp(bbeg, lp, ct))
				lp += ct;
	
			_SET(loc2,lp);
			while(lp >= curlp) {
				if(advance(lp, ep))	return(1);
				lp -= ct;
			}
			return(0);
	
	
		case CDOT | STAR:
			curlp = lp;
			_SET(loc2,lp);
			while(*lp++);
			goto star;
	
		case CCHR | STAR:
			curlp = lp;
			_SET(loc2,lp);
			while(*lp++ == *ep);
			ep++;
			goto star;
	
		case CXCL | STAR:
			curlp = lp;
			_SET(loc2,lp);
			do {
				c = (unsigned char)*lp++;
			} while(ISTHERE(c));
			ep += 32;
			goto star;
		
		case NCCL | STAR:
			neg = 1;

		case CCL | STAR:
			curlp = lp;
			do {
				_SET(loc2,lp);
				c = *lp++;
			} while(((c & 0200) == 0 && ISTHERE(c)) ^ neg);
			ep += 16;
			goto star;
	
		star:
			do {
				if(--lp == locs)
					break;
				if(advance(lp, ep))
					return(1);
			} while(lp > curlp);
			return(0);

		}
	}
}

static void
getrnge(char *str)
{
	low = *str++ & 0377;
	size = ((*str & 0377) == 255)? 20000: (*str &0377) - low;
}

#ifdef __cplusplus
}
#endif
#endif /* !__REGEXP_H__ */
