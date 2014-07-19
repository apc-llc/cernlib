*
* $Id: hplwn.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplwn.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/00 01/11/93  11.30.38  by  O.Couet
*-- Author :    O.Couet   27/09/93
      SUBROUTINE  HPLWN(NT,X1,X2,Y1,Y2)
#include "hplot/hpl3.inc"
#include "hplot/hpl9.inc"
      XWMINI = X1
      XWMAXI = X2
      YWMINI = Y1
      YWMAXI = Y2
      IF(LOGX.NE.0)THEN
         IF(X1.LE.0..OR.X2.LE.0.)GO TO 10
         XWMINI = ALOG10(X1)
         XWMAXI = ALOG10(X2)
      ENDIF
      IF(LOGY.NE.0)THEN
         IF(Y1.LE.0..OR.Y2.LE.0.)GO TO 10
         YWMINI = ALOG10(Y1)
         YWMAXI = ALOG10(Y2)
      ENDIF
      CALL ISWN(NT,XWMINI,XWMAXI,YWMINI,YWMAXI)
      RETURN
   10 CALL HBUG('+Illegal values for Log scale','HPLWN',0)
      END
