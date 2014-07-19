*
* $Id: ztoibm.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: ztoibm.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZTOIBM(IA,N,MODE)
C
C     ******************************************************************
C     *                                                                *
C     *       CONVERTS N WORDS OF ARRAY A IN MACHINE INDEPENDENT       *
C     *       FORMAT                                                   *
C     *                                                                *
C     *         MODE=0  BIT STRING                                     *
C     *         MODE=1  INTEGER 16 BITS                                *
C     *         MODE=2  INTEGER 32 BITS                                *
C     *         MODE=3  FLOATING POINTS IBM FORMAT                     *
C     *         MODE=4  ALPHANUMERIC TO INTEGER                        *
C     *         MODE=5  UNSIGNED INTEGER 16 BITS                       *
C     *         MODE=6  UNSIGNED 32 BITS INTEGER                       *
C     *         MODE=7  MIXTURE OF FLOATING AND INTEGERS               *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION NUM(40),JJ(4),LL(4),IA(1)
C
      DATA NCHARS/40/
      DATA IFIRST/0/
C
C     ------------------------------------------------------------------
C
      IF (N.LE.0)                                GO TO 99
      IF (MODE.EQ.4)                             GO TO 10
      IF (MODE.EQ.7)                             GO TO 40
C
      CALL CTOIBM(IA,N,MODE)
      GO TO 99
C
  10  IF(IFIRST.EQ.0)THEN
          IFIRST = 1
          CALL UCTOH1(' 1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ*+-'
     +   ,NUM(1),NCHARS)
      ENDIF
C
      DO 30 I = 1,N
C
      CALL UBLOW(IA(I),LL,4)
C
      DO 20 K = 1,4
      JJ(K)  = IUCOMP(LL(K),NUM(1),NCHARS)
      IF (JJ(K).EQ.0)  JJ(K) = 1
  20  CONTINUE
C
      IJ     = JJ(1)
C
      CALL SBYT(JJ(2),IJ, 9,8)
      CALL SBYT(JJ(3),IJ,17,8)
      CALL SBYT(JJ(4),IJ,25,8)
C
      IA(I)  = IJ
  30  CONTINUE
      GO TO 99
C
   40 NOLD   = 0
C
      DO 70 I = 1,N
      IF (IABS(IA(I)).GE.2**23)                  GO TO 60
C
      IF (NOLD.EQ.0)                             GO TO 50
      CALL CTOIBM(IA(LAST),NOLD,3)
      NOLD   = 0
C
   50 K      = IA(I)
      IF (K.LT.0)K = - K + 2**22
      IA(I)  = K
      GO TO 70
C
   60 IF (NOLD.EQ.0)LAST = I
      NOLD   = NOLD + 1
   70 CONTINUE
C
      IF(NOLD.NE.0) CALL CTOIBM(IA(LAST),NOLD,3)
C
   99 RETURN
      END
