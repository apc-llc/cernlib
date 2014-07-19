*
* $Id: aixgo.F,v 1.1.1.1 1996/02/26 17:16:46 mclareni Exp $
*
* $Log: aixgo.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
#include "sys/CERNLIB_machine.h"
#include "_comis/pilot.h"
#if defined(CERNLIB_NEVER)
/*CMZ :  1.18/02 13/04/94  17.40.58  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   30/03/94*/
cc -c -g -DAIX hlshl.c
xlf -qextname -bE:aintimp.o -qrndsngl -qcharlen=32767 -g -o $1  $1.f hlshl.o \
libdl.a -L$PWD /cern/pro/lib/libkernlib.a
chmod +x $1
#endif
