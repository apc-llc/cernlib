*
* $Id: cfribm50.F,v 1.1.1.1 1996/03/08 15:21:59 mclareni Exp $
*
* $Log: cfribm50.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:59  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)
      SUBROUTINE CFRIBM(A,N,MODE)
C.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       CONVERT N WORDS OF ARRAY A FROM IBM FORMAT TO NORD       *
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
C=====>        CONVERT FROM IBM F.P TO NORD F.P
C
      DO 50 I = 1,N
      TEMP   = A(I)
      IF(TEMP.EQ.0.)                             GO TO 50
      NSIGN  = IAND(JTEMP,20000000000B)
      NCHIBM = IAND(ISHFT(JTEMP,-24),177B)
      NMAIBM = IAND(JTEMP,77777777B)
      NCHND  = 4 * NCHIBM - 3
C
C             NORMALIZE
C
      IF (IAND(NMAIBM,40000000B).NE.0)           GO TO 40
      IF (IAND(NMAIBM,20000000B).NE.0)           GO TO 30
      IF (IAND(NMAIBM,10000000B).NE.0)           GO TO 20
      IF (IAND(NMAIBM, 4000000B).NE.0)           GO TO 10
C
C             BAD MANTISSA
C
      GO TO 50
C
  10  NSH    = 2
      GO TO 45
  20  NSH    = 1
      GO TO 45
  30  NSH    = 0
      GO TO 45
  40  NSH    = - 1
C
  45  NCHND  = NCHND + (2 - NSH)
      NMAND  = ISHFT(NMAIBM,NSH)
C
C             STORE
C
      CALL SBYT(NMAND,TEMP,1,22)
      CALL SBYT(NCHND,TEMP,23,9)
      CALL SBIT(NSIGN,TEMP,32)
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
      TEMP=A(I)
      IF(IAND(JTEMP,100000B).NE.0) CALL SBYT(177777B,A(I),17,16)
  80  CONTINUE
C
  99  RETURN
      END
#endif
