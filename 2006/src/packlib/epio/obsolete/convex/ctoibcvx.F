*
* $Id: ctoibcvx.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: ctoibcvx.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_CONVEX))&&(!defined(CERNLIB_DOUBLE))
#ifdef CERNLIB_UTIL_CTOIBSTF
#undef CERNLIB_UTIL_CTOIBSTF
#endif
      SUBROUTINE CTOIBM(A,NW,MODE)
      DIMENSION A(2)
C  W.W. 17/04/84  SACLAY/DPHPE
C
      EQUIVALENCE (AMM,MM)
      DATA NBDCON / 0 /
C
      IF(NW.LE.0)    RETURN
C
      DO 49 JV = 1, NW
      AMM = A(JV)
C
      IF(MODE.EQ.1)   GO TO             11
      IF(MODE.EQ.2)   GO TO             21
      IF(MODE.EQ.3)   GO TO             31
      GO TO                             49
C
C--                 IF  INTEGER 16 BIT
C
 11   MM = MM .AND. MASK(128-16)
      GO TO                             48
C
C--                 IF  INTEGER 32 BIT
C
 21   MM = MM .AND. MASK(128-32)
      GO TO                             48
C
C--                 IF  FLOATING
C--                 ALGORITHM TAKEN FROM HYDRA 3.43/2
C
 31   JSIGN = ISHFT(MM,-63)
      JEXP  = JBYT(MM,49,15)
      IF(JEXP.EQ.0)       GO TO         48
      IF(JEXP.LT.'20000'O)  GO TO         37
      IF(JEXP.GE.'60000'O)  GO TO         37
C
      J   = (JEXP+3) / 4
      JSH = 4*J - JEXP
      JEXP= J - '10000'O
C
C--                 25-BIT MANTISSA, ROUND IF LAST BIT IS 1
C
      JMAN  = ISHFT (MM,16)
      JMAN  = ISHFT (JMAN,-39-JSH)
      JRND  = ISHFT (JMAN,63)
      JRND  = ISHFTC(JRND,1,64)
      JMAN  = JMAN + JRND
      IF(ISHFT (JMAN,-25).EQ.0)  GO TO   33
C
C--                 ROUNDING WITH CARRY INTO EXPONENT FIELD
C
      JEXP = JEXP + 1
      JMAN = ISHFT (JMAN,-4)
      JRND = ISHFT (JMAN,63)
      JRND = ISHFTC(JRND,1,64)
      JMAN = JMAN + JRND
C
 33   IF(JEXP.LT.-63)  GO TO            35
      IF(JEXP.GE. 64)  GO TO            37
      MM = ISHFT (JMAN,32) .OR. JSIGN
      MM = ISHFTC(MM,7,64)    .OR. (JEXP+64)
      MM = ISHFTC(MM,24,64)
      GO TO                             48
C
C--                 UNDERFLOW
C
 35   MM = 0
      GO TO                             48
C
C--                 OVERFLOW
C
 37   MM = MASK(128-31)
      NBDCON = NBDCON + 1
C
 48   A(JV) = AMM
 49   CONTINUE
C
      RETURN
      END
#endif
