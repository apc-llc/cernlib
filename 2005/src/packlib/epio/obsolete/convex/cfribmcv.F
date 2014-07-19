*
* $Id: cfribmcv.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: cfribmcv.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_CONVEX))&&(!defined(CERNLIB_DOUBLE))
#ifdef CERNLIB_UTIL_CFRIBMSF
#undef CERNLIB_UTIL_CFRIBMSF
#endif
      SUBROUTINE CFRIBM(A,NW,MODE)
      DIMENSION A(2)
C  W.W. 17/04/84  SACLAY/DPHPE
C
      EQUIVALENCE (AMM,MM)
      DATA ANORM / 0. /
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
 11   JSIGN = JBIT(MM,16)
      IF(JSIGN.EQ.1) MM = MM .OR. MASK(48)
      GO TO                             48
C
C--                 IF  INTEGER 32 BIT
C
 21   JSIGN = JBIT(MM,32)
      IF(JSIGN.EQ.1) MM = MM .OR. MASK(32)
      GO TO                             48
C
C--                 IF  FLOATING
C--                 ALGORITHM TAKEN FROM HYDRA 3.43/2
C
 31   IF(MM .EQ. MASK(128-31))  GO TO   44
      JSIGN = JBIT(MM,32)
      JEXP  = (JBYT(MM,25,7)-64) * 4
      JMAN  = ISHFT(MM,40)
      MM = JMAN .OR. (JEXP + '40000'O)
      MM = ISHFTC(MM,49,64) .OR. JSIGN
      MM = ISHFTC(MM,63,64)
C
C--                 ADD ZERO TO NORMALIZE
C
      AMM = AMM + ANORM
      GO TO                             48
C
C----               BAD CONVERSION
C
 44   MM = '70000 77000000 37000000'O
      NBDCON = NBDCON + 1
C
 48   A(JV) = AMM
 49   CONTINUE
C
      RETURN
      END
#endif
