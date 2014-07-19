*
* $Id: hplsiz.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplsiz.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.24  by  O.Couet
*-- Author :
      SUBROUTINE HPLSIZ(XSZ,YSZ,CHOPT)
*.==========>
*.           Sets or returns picture size in cm according to
*.           the option CHOPT.
*.           CHOPT = ' ' set picture size with values XSZ and YSZ
*.           CHOPT = 'R' returns picture size in XSZ and YSZ variable.
*..=========>
*
#include "hplot/hpl4.inc"
*
      CHARACTER*1 CHOPT
*._____________________________
      IF (CHOPT.EQ.' ')THEN
          CALL HPLSET('XSIZ',XSZ)
          CALL HPLSET('YSIZ',YSZ)
      ELSE IF(CHOPT.EQ.'R')THEN
          XSZ=XSIZ
          YSZ=YSIZ
      ENDIF
      END

