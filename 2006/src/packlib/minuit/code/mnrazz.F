*
* $Id: mnrazz.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnrazz.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNRAZZ(YNEW,PNEW,Y,JH,JL)
#include "minuit/d506dp.inc"
CC        Called only by MNSIMP (and MNIMPR) to add a new point
CC        and remove an old one from the current simplex, and get the
CC        estimated distance to minimum.
CC
#include "minuit/d506cm.inc"
      DIMENSION PNEW(*), Y(*)
      DO 10 I=1,NPAR
   10 P(I,JH) = PNEW(I)
      Y(JH)=YNEW
      IF(YNEW .LT. AMIN) THEN
        DO 15 I=1,NPAR
   15   X(I) = PNEW(I)
        CALL MNINEX(X)
        AMIN = YNEW
        CSTATU = 'PROGRESS  '
        JL=JH
      ENDIF
      JH = 1
      NPARP1 = NPAR+1
   20 DO 25 J=2,NPARP1
      IF (Y(J) .GT. Y(JH))  JH = J
   25 CONTINUE
      EDM = Y(JH) - Y(JL)
      IF (EDM .LE. ZERO)  GO TO 45
      DO 35 I= 1, NPAR
      PBIG = P(I,1)
      PLIT = PBIG
      DO 30 J= 2, NPARP1
      IF (P(I,J) .GT. PBIG)  PBIG = P(I,J)
      IF (P(I,J) .LT. PLIT)  PLIT = P(I,J)
   30 CONTINUE
      DIRIN(I) = PBIG - PLIT
   35 CONTINUE
   40 RETURN
   45 WRITE (ISYSWR, 1000)  NPAR
      GO TO 40
 1000 FORMAT ('   FUNCTION VALUE DOES NOT SEEM TO DEPEND ON ANY OF THE',
     +    I3,' VARIABLE PARAMETERS.' /10X,'VERIFY THAT STEP SIZES ARE',
     +    ' BIG ENOUGH AND CHECK FCN LOGIC.'/1X,79(1H*)/1X,79(1H*)/)
      END
