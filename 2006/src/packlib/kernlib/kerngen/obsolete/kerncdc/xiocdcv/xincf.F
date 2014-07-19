*
* $Id: xincf.F,v 1.1.1.1 1996/02/15 17:51:16 mclareni Exp $
*
* $Log: xincf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:16  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XINCF (LUN,XBV,NXB)
 
C CERN PROGLIB# Z203    XINCF           .VERSION KERNCDC  1.13  820111
 
      DIMENSION    XBV(NXB)
 
      READ (LUN,8000,IOSTAT=NSTAT)  XBV
      IF (NSTAT)    61,24,64
   24 CONTINUE
      RETURN
 
   61 NXB = 0
      RETURN
 
   64 IF (NSTAT.EQ.65)       GO TO 61
      NXB = -7
      RETURN
 
 8000 FORMAT (14A10)
      END
#ifdef CERNLIB_XVECT_XINCF
#undef CERNLIB_XVECT_XINCF
#endif
