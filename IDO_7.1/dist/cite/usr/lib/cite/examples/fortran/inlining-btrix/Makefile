
#!smake

.SUFFIXES: .f .loc .l .s

default all: \
	btrix.l
 

.f.l:
	- /usr/bin/f77 -o $*.O2 -64 -mips4 -O2 $*.f 
	- /usr/bin/f77 -o $*.O3 -64 -mips4 -O3 -LIST:cite $*.f 
	- /usr/bin/f77 -o $*.ipa.O3 -ipa -64 -mips4 -PHASE:l=off -O3 -flist -FLIST:ftn_file=$*-after-ipa.f $*.f 
	- mv $*.ipa.O3.ipa*/$*-after-ipa.f .
	- rm -r $*.ipa.O3.ipa*
	- /usr/bin/f77 -c -64 -mips4 -O3 -LIST:cite $*-after-ipa.f 

clean clobber:
	rm -f *.l *.s *.loc *after-lno.* *after-ipa* *ipaa* core *.B *.N *.o *.O2 *.O3 *~ *.flc
