*
* $Id: uctoh.F,v 1.1.1.1 1996/02/26 17:16:52 mclareni Exp $
*
* $Log: uctoh.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:52  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if !defined(CERNLIB_PAW)
*CMZ :  1.18/14 18/10/94  11.39.49  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   18/10/94
      SUBROUTINE UCTOH (MS,MT,NPW,NCH)
C
C CERN PROGLIB# M409    UCTOH           .VERSION KERNFOR  4.21  890323
C ORIG. 10/02/89  JZ
C
      CHARACTER    MS*99
      DIMENSION    MT(99)
*+SELF, WORDSIZE.    # of characters/word
      PARAMETER   (NCHAPW=4)
*+SELF.
      CHARACTER    CHWORD*(NCHAPW)
      CHARACTER    BLANK *(NCHAPW)
      PARAMETER   (BLANK = ' ')
      INTEGER      IWORD
      EQUIVALENCE (IWORD,CHWORD)
      IF   (NCH)             91, 29, 11
   11 IF (NPW.LE.0)          GO TO 91
      IF (NPW.EQ.1)          GO TO 21
      IF (NPW.LT.NCHAPW)     GO TO 31

C--------          NPW = maximum

      JS     = 0
      NWT    = NCH / NCHAPW
      NTRAIL = NCH - NWT*NCHAPW

      DO 14  JT=1,NWT
      CHWORD = MS(JS+1:JS+NCHAPW)
      MT(JT) = IWORD
   14 JS     = JS + NCHAPW

      IF (NTRAIL.EQ.0)       RETURN

      CHWORD    = MS(JS+1:JS+NTRAIL)
      MT(NWT+1) = IWORD
      RETURN

C--------          NPW = 1
C--                equivalent to 'CALL UCTOH1(MS,MT,NCH)'

   21 CHWORD = BLANK

      DO 24 JS=1,NCH
      CHWORD(1:1) = MS(JS:JS)
      MT(JS)      = IWORD
   24 CONTINUE
   29 RETURN
C--------          NPW = 2 ...

   31 CHWORD = BLANK

      JS     = 0
      NWT    = NCH / NPW
      NTRAIL = NCH - NWT*NPW

      DO 34  JT=1,NWT
      CHWORD(1:NPW) = MS(JS+1:JS+NPW)
      MT(JT) = IWORD
   34 JS     = JS + NPW

      IF (NTRAIL.EQ.0)       RETURN

      CHWORD    = MS(JS+1:JS+NTRAIL)
      MT(NWT+1) = IWORD
      RETURN

   91 print *,' UCTOH: NCH<0 ',NCH
      END
#endif
