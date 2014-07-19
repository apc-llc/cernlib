*
* $Id: ubunch.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE UBUNCH (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNTMO  1.00  890208
C ORIG. 03/02/89 K.M.STORR
C
 
      DIMENSION    MS(99), MT(99), NCHP(9)
      PARAMETER    (IBLAN1 = ?I20202020)
      PARAMETER    (MASK1  = ?I000000FF)
 
 
      NCH = NCHP(1)
      IF   (NCH)             91,39,11
   11 NWT    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 31
 
C--                Pack the initial complete words
 
      DO 24  JT=1,NWT
      MT(JT) = IOR (IOR (IOR (
     +                  IAND(MS(JS+1),MASK1),
     +           ishft (IAND(MS(JS+2),MASK1), 8)),
     +           ishft (IAND(MS(JS+3),MASK1),16)),
     +           ishft      (MS(JS+4),       24) )
   24 JS = JS + 4
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Pack the trailing word
 
   31 MWD = IBLAN1
      JS  = NCH
 
      DO 34 JT=1,NTRAIL
      MWD = IOR (ishft(MWD,8), IAND(MS(JS),MASK1))
   34 JS  = JS - 1
      MT(NWT+1) = MWD
   39 RETURN
 
   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
