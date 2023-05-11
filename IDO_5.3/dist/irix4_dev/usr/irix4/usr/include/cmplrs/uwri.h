/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1991, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Avenue                               |
 * |         Sunnyvale, California 94088-3650, USA             |
 * |-----------------------------------------------------------|
 */
/* $Header: /d1/cmplrs.src/v3.12/include/cmplrs/RCS/uwri.h,v 7.3 1992/01/20 13:00:51 daveb Exp $ */

procedure inituwrite(
	var ObjectName : Filename);
  external;

function idlen(
	var Id	    : Identname)
   : integer;
  external;

procedure uwrite (var U: Bcrec);
  external;

function getdtyname(
	   Dtyp	    : Datatype)
   : char;
  external;

function getmtyname(
	   Mtyp	    : Memtype)
   : char;
  external;

procedure ucoid(
	    Tag	     : Identname);
  external;

procedure ucofname(
	    Fnam     : Filename);
  external;

procedure stopucode;
  external;

procedure uputinit(
	var ObjectName : Filename);
  external;

procedure uputint(
	i: integer);
  external;

procedure uputkill;
    external;

procedure uputclose;
    external;

PROCEDURE Ubittobyte (VAR U: Bcrec);
  external;

procedure Set_u_indent(lev: integer);
  external;
