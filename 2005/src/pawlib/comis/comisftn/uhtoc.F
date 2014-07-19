*
* $Id: uhtoc.F,v 1.1.1.1 1996/02/26 17:16:54 mclareni Exp $
*
* $Log: uhtoc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:54  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if !defined(CERNLIB_PAW)
*CMZ :  1.18/14 18/10/94  11.38.24  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   18/10/94
      SUBROUTINE UHTOC (MS,NPW,MT,NCH)
C
C CERN PROGLIB# M409    UHTOC           .VERSION KERNFOR  4.21  890323
C ORIG. 10/02/89  JZ
C
      DIMENSION    MS(99)
      CHARACTER    MT*99

*+SELF, WORDSIZE.    # of characters/word
      PARAMETER   (NCHAPW=4)
*+SELF.
      CHARACTER    CHWORD*(NCHAPW)
      INTEGER      IWORD
      EQUIVALENCE (IWORD,CHWORD)
      IF   (NCH)             91, 29, 11
   11 IF (NPW.LE.0)          GO TO 91
      IF (NPW.EQ.1)          GO TO 21
      IF (NPW.LT.NCHAPW)     GO TO 31

C--------          NPW = maximum

      JT     = 0
      NWS    = NCH / NCHAPW
      NTRAIL = NCH - NWS*NCHAPW

      DO 14  JS=1,NWS
      IWORD  = MS(JS)
      MT(JT+1:JT+NCHAPW) = CHWORD
   14 JT  = JT + NCHAPW
      IF (NTRAIL.EQ.0)       RETURN

      IWORD = MS(NWS+1)
      MT(JT+1:JT+NTRAIL) = CHWORD(1:NTRAIL)
      RETURN

C--------          NPW = 1
C--                equivalent to 'CALL UH1TOC(MS,MT,NCH)'

   21 DO 24  JS=1,NCH
      IWORD  = MS(JS)
      MT(JS:JS) = CHWORD(1:1)
   24 CONTINUE
   29 RETURN

C--------          NPW = 2 ...

   31 JT     = 0
      NWS    = NCH / NPW
      NTRAIL = NCH - NWS*NPW
      DO 34  JS=1,NWS
      IWORD  = MS(JS)
      MT(JT+1:JT+NPW) = CHWORD(1:NPW)
   34 JT  = JT + NPW

      IF (NTRAIL.EQ.0)       RETURN

      IWORD = MS(NWS+1)
      MT(JT+1:JT+NTRAIL) = CHWORD(1:NTRAIL)
      RETURN

   91 print *,' UHTOC: wrong args.'
      END
#endif
