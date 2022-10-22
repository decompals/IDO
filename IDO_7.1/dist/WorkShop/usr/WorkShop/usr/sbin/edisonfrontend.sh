#!/bin/ksh
# Invokes the edison front end

INCLUDEDIRS=' -I/usr/include -I/usr/include/X11  -I/usr/include/CC  '

DEBUGFLAGS='-DNDEBUG'

COMPILEFLAGS='-D__cplusplus=1  -D_LANGUAGE_C_PLUS_PLUS=1'

EDISONFLAGS='-n -YSvcvdb -YE -w  -D__EXTENSIONS  -Dsgi  -DSVR3  -D__sgi  -D__DYNAMIC  -D__EXTENSIONS__=1 '

case $# in
0)
	echo "Usage: edisonfrontend.sh <flags> [filename]";
	exit 1;;
esac

CMDFLAGS='';

for i in $*
do
	case $i in
	
	"-compile")
		COMPILEFLAGS='';;
	"-include")
		INCLUDEDIRS='';;
	"-debug")
		DEBUGFLAGS='';;
	*)
		CMDFLAGS="${CMDFLAGS} $i";
	esac
done

/usr/lib/WorkShop/edgcpfe $INCLUDEDIRS $DEBUGFLAGS $EDISONFLAGS $COMPILEFLAGS $CMDFLAGS;
