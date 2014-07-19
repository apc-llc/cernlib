*
* $Id: upkbytsv.F,v 1.1.1.1 1996/02/15 17:54:17 mclareni Exp $
*
* $Log: upkbytsv.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:17  mclareni
* Kernlib
*
*
*             slow version saved
#include "kerngen/pilot.h"
#if defined(CERNLIB_NOSHIFT)
      SUBROUTINE UPKBYT (MBV,JTHP,MIV,NINTP,NBITS)
C
C CERN PROGLIB# M422    UPKBYT          .VERSION KERNFOR  4.08  840613
C ORIG. 10/02/71, RE-WRITE 16/12/83  JZ
C
      DIMENSION    MIV(*), MBV(*),   JTHP(9), NINTP(9), NBITS(2)
C
#include "kerngen/q_jbit.inc"
* Ignoring t=pass
C
      JTH  = JTHP(1)
      NINT = NINTP(1)
      IF (NINT.LE.0)         RETURN
C
      NZB  = NBITS(1)
      IF (NZB.GT.0)          GO TO 11
      NZB  = 1
#if defined(CERNLIB_B32)
      NPWD = 32
#endif
#if defined(CERNLIB_B36)
      NPWD = 36
#endif
#if defined(CERNLIB_B48)
      NPWD = 48
#endif
#if defined(CERNLIB_B60)
      NPWD = 60
#endif
#if defined(CERNLIB_B64)
      NPWD = 64
#endif
      GO TO 12
C
   11 NPWD = NBITS(2)
C
   12 JBV  = 1
      JIV  = 0
      IF (JTH.LT.2)          GO TO 21
      JBV  = (JTH-1)/NPWD + 1
      JPOS = JTH - (JBV-1)*NPWD - 1
      IF (JPOS.EQ.0)         GO TO 21
      JIVE = NPWD - JPOS
      JJ   = JPOS*NZB + 1
      GO TO 22
C
C--                UNPACK EACH WORD
C
   21 JIVE = JIV + NPWD
      JJ   = 1
   22 JIVE = MIN0 (NINT,JIVE)
      IZW  = MBV(JBV)
C
   24 JIV  = JIV + 1
      MIV(JIV) = JBYT (IZW,JJ,NZB)
      IF (JIV.EQ.JIVE)       GO TO 27
      JJ   = JJ + NZB
      GO TO 24
C
   27 IF (JIV.EQ.NINT)       RETURN
      JBV  = JBV + 1
      GO TO 21
      END
#endif
