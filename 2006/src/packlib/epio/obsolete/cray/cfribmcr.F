*
* $Id: cfribmcr.F,v 1.1.1.1 1996/03/08 15:21:54 mclareni Exp $
*
* $Log: cfribmcr.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:54  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CRAY)
      SUBROUTINE CFRIBM(A,NW,MODE)
      DIMENSION A(*)
C
C  W.W. 17/04/84  SACLAY/DPHPE
C  modified for Cray  MJC  30/1/91
      EQUIVALENCE (BAD,IBAD),(BAD31,IBAD31)
C
      DATA ANORM / 0. /
      DATA IBAD /70000 77000000 37000000B/
      DATA IBAD31 /X'00000000 7FFFFFFF'/
C
      SAVE IBAD,IBAD31,ANORM
C
      JBIT (ZW,IZP)     = SHIFTR(ZW,IZP-1) .AND. MASK(127)
      JBYT (ZW,IZP,NZB) = SHIFTR(ZW,IZP-1) .AND. MASK(128-NZB)
C
      IF(NW.LE.0)    RETURN
C
      IF (MODE.EQ.1)   THEN
C
C--                 IF  INTEGER 16 BIT
C
        DO 1 JV = 1,NW
          JSIGN = JBIT(A(JV),16)
          IF(JSIGN.EQ.1) A(JV)=A(JV) .OR. MASK(48)
   1    CONTINUE
      ELSEIF (MODE.EQ.2)   THEN
C
C--                 IF  INTEGER 32 BIT
C
        DO 2 JV = 1,NW
          JSIGN = JBIT(A(JV),32)
          IF(JSIGN.EQ.1) A(JV)=A(JV) .OR. MASK(32)
   2    CONTINUE
      ELSEIF (MODE.EQ.3)   THEN
C
C--                 IF  FLOATING
C--                 ALGORITHM TAKEN FROM HYDRA 3.43/2
C
        DO 3 JV = 1,NW
          IF(A(JV) .EQ. BAD31)  THEN
C
C----               BAD CONVERSION
C
            A(JV) = BAD
          ELSE
            JSIGN = JBIT(A(JV),32)
            JEXP  = (JBYT(A(JV),25,7)-64) * 4
            JMAN  = SHIFTL(A(JV),40)
            A(JV) = JMAN .OR. (JEXP + 40000B)
            A(JV) = SHIFT(A(JV),49) .OR. JSIGN
            A(JV) = SHIFT(A(JV),63)
C
C--                 ADD ZERO TO NORMALIZE
C
            A(JV) = A(JV) + ANORM
          ENDIF
  3     CONTINUE
      ENDIF
C
      RETURN
      END
#endif
