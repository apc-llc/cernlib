*
* $Id: zfribm.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zfribm.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZFRIBM(IA,N,MODE)
C
C     ******************************************************************
C     *                                                                *
C     *       CONVERTS N WORDS OF ARRAY A FROM MACHINE                 *
C     *       INDEPENDENT FORMAT                                       *
C     *                                                                *
C     *         MODE=0  BIT STRING                                     *
C     *         MODE=1  SIGNED INTEGER 16 BITS                         *
C     *         MODE=2  SIGNED INTEGER 32 BITS                         *
C     *         MODE=3  FLOATING POINTS IBM FORMAT                     *
C     *         MODE=4  ALPHANUMERIC TO INTEGER                        *
C     *         MODE=5  UNSIGNED INTEGER 16 BITS                       *
C     *         MODE=6  UNSIGNED 32 BITS INTEGER                       *
C     *         MODE=7  MIXTURE OF FLOATING AND INTEGERS               *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION NUM(40),LL(4),IA(1)
#if defined(CERNLIB_VAX)
      INTEGER*2 JTEST(2),JTEMP
      EQUIVALENCE (ITEST,JTEST)
#endif
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
      CALL CFRIBM(IA,N,MODE)
      GO TO 99
C
  10  IF(IFIRST.EQ.0)THEN
         CALL UCTOH1(' 1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ*+-'
     +   ,NUM(1),NCHARS)
C
         IFIRST = 1
      ENDIF
C
      DO 30 I = 1,N
      IJ     = IA(I)
C
      DO 20 K = 1,4
      L      = 8 * K - 7
      JJ     = JBYT(IJ,L,8)
      IF (JJ.LE.0 .OR. JJ.GT.NCHARS) JJ = 1
      LL(K)  = NUM(JJ)
  20  CONTINUE
C
      CALL UBUNCH(LL,IA(I),4)
  30  CONTINUE
      GO TO 99
C
   40 NOLD   = 0
C
      DO 70 I = 1,N
#if !defined(CERNLIB_VAX)
      IF (IABS(IA(I)).GE.2**23)                  GO TO 60
#endif
#if defined(CERNLIB_VAX)
      ITEST=IA(I)
      JTEMP=JTEST(1)
      JTEST(1)=JTEST(2)
      JTEST(2)=JTEMP
      IF(IABS(ITEST).GE.2**23)                   GO TO 60
#endif
C
      IF (NOLD.EQ.0)                             GO TO 50
      CALL CFRIBM(IA(LAST),NOLD,3)
      NOLD   = 0
C
#if !defined(CERNLIB_VAX)
   50 K      = IA(I)
#endif
#if defined(CERNLIB_VAX)
   50 K      = ITEST
#endif
      IF (K.GT.2**22)K = - (K - 2**22)
      IA(I)  = K
      GO TO 70
C
   60 IF (NOLD.EQ.0)LAST = I
      NOLD   = NOLD + 1
   70 CONTINUE
C
      IF(NOLD.NE.0) CALL CFRIBM(IA(LAST),NOLD,3)
C
   99 RETURN
      END
