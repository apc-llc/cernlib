*
* $Id: hplsof.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplsof.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.24  by  O.Couet
*-- Author :
      SUBROUTINE HPLSOF(X,Y,CHTEXT,SSIZE,ANGLE,XMAX,IGOTO)
*.==========>
*.           Software characters : character HERSHEY
*.           cm are requested for X and Y.
*.           (Transformation no 1 is selected)
*..=========>
#include "hplot/hpl2.inc"
      CHARACTER*(*) CHTEXT
      CHARACTER*1 CHOPT
*._____________________________
*
      IF(NTWIN.NE.1)THEN
         CALL ISELNT(1)
         NTWIN=1
      ENDIF
*
      IF(IGOTO.GT.0)THEN
         CHOPT='R'
      ELSEIF(IGOTO.EQ.0)THEN
         CHOPT='C'
      ELSE
         CHOPT='L'
      ENDIF
      CALL IGTEXT(X,Y,CHTEXT,SSIZE,ANGLE,CHOPT)
      END

