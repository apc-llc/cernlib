*
* $Id: hntnew.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntnew.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 19/09/92  16.34.02  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/09/92
      LOGICAL FUNCTION HNTNEW(IDN)
*.==========>
*.
*.           Returns .TRUE. when new N-tuple, otherwise false.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      NIDN = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IDN)
      IF (IQ(LQ(LTAB-NIDN)-2) .EQ. ZLINK) THEN
         HNTNEW = .TRUE.
      ELSE
         HNTNEW = .FALSE.
      ENDIF
*
      END
