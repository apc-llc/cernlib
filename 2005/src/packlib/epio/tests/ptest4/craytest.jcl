*
* $Id: craytest.jcl,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: craytest.jcl,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
#include "sys/CERNLIB_machine.h"
#include "_epio/pilot.h"
#if defined(CERNLIB_NEVER)
# USER=..... PW=.
# QSUB-r rftest
#
set -xS
CHARON=/u/v6/charon
cat > rftst1.f <<\EOF
#
# implement WRTEST here
#
EOF
cft77 -eh -b rftst1.o rftst1.f
cernlib
segldr -o rftst1.a rftst1.o LIB$
#
# !!! very important - make binary file type 'sbin' via -s sbin !!!
#
assign -a rftest.data -s sbin fort.11
rftst1.a
cat > rftst2.f <<\EOF
#
# implement RDTEST here
#
EOF
cft77 -eh -b rftst2.o rftst2.f
segldr -o rftst2.a rftst2.o LIB$
assign -a rftest.data -s sbin fort.11
assign -a rftest.sqnm fort.22
rftst2.a
cat > rftst3.f <<\EOF
#
# implement RATEST here
#
EOF
cft77 -eh -b rftst3.o rftst3.f
segldr -o rftst3.a rftst3.o LIB$
assign -a rftest.data -s sbin fort.11
assign -a rftest.sqnm fort.22
rftst3.a
#endif
