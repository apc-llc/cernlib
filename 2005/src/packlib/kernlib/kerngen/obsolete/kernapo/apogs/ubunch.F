*
* $Id: ubunch.F,v 1.1.1.1 1996/02/15 17:51:05 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:05  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UBUNCH (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNAPO  1.11  880222
C ORIG. 12/12/83  JZ
C Mod.  29/03/85  remove entry UH1TOC
C Mod.  15/02/88  for speed, JZ
C

      DIMENSION    MS(99), MT(99), NCHP(9)
      PARAMETER    (IBLAN1 = 16#20202020)
      PARAMETER    (MASK1  = 16#FF000000)


      NCH = NCHP(1)
      IF   (NCH)             91,39,11
   11 NWT    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 31

C--                Pack the initial complete words

      DO 24  JT=1,NWT
      MT(JT) = OR (OR (OR (
     +                  AND(MS(JS+1),MASK1),
     +           RSHFT (AND(MS(JS+2),MASK1), 8)),
     +           RSHFT (AND(MS(JS+3),MASK1),16)),
     +           RSHFT     (MS(JS+4),       24) )
   24 JS = JS + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   31 MWD = IBLAN1
      JS  = NCH

      DO 34 JT=1,NTRAIL
      MWD = OR (RSHFT(MWD,8), AND(MS(JS),MASK1))
   34 JS  = JS - 1
      MT(NWT+1) = MWD
   39 RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
