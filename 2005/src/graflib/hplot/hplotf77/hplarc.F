*
* $Id: hplarc.F,v 1.1.1.1 1996/01/19 10:49:57 mclareni Exp $
*
* $Log: hplarc.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.11  by  O.Couet
*-- Author :
      SUBROUTINE HPLARC(XC,YC,RAD,APHI1,APHI2)
*.==========>
*.            Draws arc of circle
*.     (Transformation no 1 (cm) is selected)
*..=========>
#include "hplot/hpl2.inc"
*._____________________________
*
*----
*     Transformation no 1 (cm) is selected
*----
      IF(NTWIN.NE.1)THEN
         CALL ISELNT(1)
         NTWIN=1
      ENDIF
*
      CALL IGARC(XC,YC,RAD,RAD,APHI1,APHI2)
*
      END

