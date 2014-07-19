*
* $Id: mnerrs.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mnerrs.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNERRS(NUMBER,EPLUS,EMINUS,EPARAB,GCC)
#include "minuit/d506dp.inc"
CC    Called by user, utility routine to get MINOS errors
CC    If NUMBER is positive, then it is external parameter number,
CC                  if negative, it is -internal number.
CC    values returned by MNERRS:
CC       EPLUS, EMINUS are MINOS errors of parameter NUMBER,
CC       EPARAB is 'parabolic' error (from error matrix).
CC                 (Errors not calculated are set = 0.)
CC       GCC is global correlation coefficient from error matrix
#include "minuit/d506cm.inc"
C
      IEX = NUMBER
      IF (NUMBER .LT. 0)  THEN
         IIN = -NUMBER
         IF (IIN .GT. NPAR)  GO TO 900
         IEX = NEXOFI(IIN)
      ENDIF
      IF (IEX .GT. NU .OR. IEX .LE. 0)  GO TO 900
      IIN = NIOFEX(IEX)
      IF (IIN .LE. 0)  GO TO 900
C             IEX is external number, IIN is internal number
      EPLUS = ERP(IIN)
        IF (EPLUS.EQ.UNDEFI)  EPLUS=0.
      EMINUS= ERN(IIN)
        IF (EMINUS.EQ.UNDEFI) EMINUS=0.
      CALL MNDXDI(X(IIN),IIN,DXDI)
      NDIAG = IIN*(IIN+1)/2
      EPARAB = ABS(DXDI*SQRT(ABS(UP*VHMAT(NDIAG))))
C              global correlation coefficient
      GCC = 0.
      IF (ISW(2) .LT. 2)  GO TO 990
      GCC = GLOBCC(IIN)
      GO TO 990
C                  ERROR.  parameter number not valid
  900 EPLUS = 0.
      EMINUS = 0.
      EPARAB = 0.
      GCC = 0.
  990 RETURN
      END
