*
* $Id: ctoibm50.F,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: ctoibm50.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)
      SUBROUTINE CTOIBM(A,N,MODE)
C.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       CONVERT N WORDS OF ARRAY A FROM NORD FORMAT TO IBM       *
C.    *                                                                *
C.    *       MODE=1  16 BIT SIGNED INTEGER                            *
C.    *           =2  32 BIT SIGNED INTEGER                            *
C.    *           =3  FLOATING POINT                                   *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION A(1)
      EQUIVALENCE (TEMP,JTEMP)
C.
C.
C.    ------------------------------------------------------------------
C.
C.
      IF (N.LE.0)                                GO TO 99
      IF (MODE.NE.3)                             GO TO 60
C
C=====>        CONVERT FROM NORD F.P TO IBM F.P
C
      DO 50 I = 1,N
      TEMP   = A(I)
      IF(TEMP.EQ.0.)                             GO TO 50
      NCH    = IAND(ISHFT(JTEMP,-22),777B)
      NSIGN  = IAND(JTEMP,20000000000B)
C
C             GET MANTISSA,ADD IN THE 23RD BIT (NORMALIZE)
C
      NMAND  = IAND(JTEMP,17777777B) + 20000000B
C
C             CALCULATE CHARACTERISTIC FOR IBM
C
      NCHIBM = (NCH + 3) / 4
C
C             AND SHIFT COUNT FOR MANTISSA
C
      NSHMAN = MOD(NCH + 3,4) - 2
C
C             MAKE UP IBM F.P NUMBER
C
      JTEMP  = ISHFT(NMAND,NSHMAN)
      IF (NSIGN.NE.0)NCHIBM = NCHIBM + 200B
      CALL SBYT(NCHIBM,JTEMP,25,8)
      A(I)   = TEMP
  50  CONTINUE
      GO TO 99
C
C=====>        CONVERT 32 BIT SIGNED INTEGER
C
  60  IF (MODE.NE.2)                             GO TO 70
      GO TO 99
C
C=====>        CONVERT 16 BIT SIGNED INTEGER
C
  70  IF(MODE.NE.1)                              GO TO 99
      DO 80 I = 1,N
      CALL SBYT(0,A(I),17,16)
  80  CONTINUE
C
  99  RETURN
      END
#endif
