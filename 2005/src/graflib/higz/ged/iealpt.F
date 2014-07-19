*
* $Id: iealpt.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: iealpt.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 16/12/88  14.00.39  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEALPT(X,Y)
*.===========>
*.
*. Align couple of point if they are very close in X or Y
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
      PARAMETER (RALGN=0.008)
      DIMENSION X(*),Y(*)
*.______________________________________
*
      DX=(RWXMAX-RWXMIN)*RALGN
      DY=(RWYMAX-RWYMIN)*RALGN
      IF(ABS(X(2)-X(1)).LT.DX)THEN
         X(2)=X(1)
      ENDIF
      IF(ABS(Y(2)-Y(1)).LT.DY)THEN
         Y(2)=Y(1)
      ENDIF
*
      END
