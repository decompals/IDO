#**************************************************************************
#*									  *
#* 		 Copyright (C) 1991, Silicon Graphics, Inc.		  *
#*									  *
#*  These coded instructions, statements, and computer programs  contain  *
#*  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
#*  are protected by Federal copyright law.  They  may  not be disclosed  *
#*  to  third  parties  or copied or duplicated in any form, in whole or  *
#*  in part, without the prior written consent of Silicon Graphics, Inc.  *
#*									  *
#**************************************************************************
#*
#* Makefile - Vlan  Shell Level Programs
#*
#* $Revision: 1.1 $
#*
#**************************************************************************

DEPTH = ..
include /usr/include/make/commondefs

TARGETS = vlan232alive vlan232cmd
CFILES = vlan232alive.c vlan232cmd.c

#-- Do not remove this line - sed delimiter

DEBUG = 

LCINCS = -I. -I$(DEPTH)/include
LCOPTS = $(DEBUG)

LLDOPTS = -L -L../lib
LLDLIBS = -lvlan232 

default all:
	@$(MAKE) $(TARGETS)

include $(COMMONRULES)

vlan232alive: vlan232alive.o
	$(CCF) vlan232alive.o $(LDFLAGS) -o $@

vlan232cmd: vlan232cmd.o
	$(CCF) vlan232cmd.o $(LDFLAGS) -o $@

install: all
