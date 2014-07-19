*
* $Id: hpltx1.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hpltx1.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/01 03/11/93  08.59.34  by  O.Couet
*-- Author :
      SUBROUTINE HPLTX1(KTYP,KORNT)
*.==========>
*.            Checks various types of character strings to ensure they
*.            do not go outside certain limits.
*..=========>
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
*._____________________________
      GOTO  (999,10,20,30,40,999,999,999,50),KTYP
*----
*     Global title
*----
   10 CONTINUE
      X1=0
      X2=XSIZ
      GOTO 900
*----
*     Histogram title
*----
   20 X1=XLOW
      X2=XHIGH
      GOTO 900
*----
*     Axis labels
*----
   30 IF(KORNT.EQ.1)THEN
         X1=XLOW
         X2=XHIGH
      ELSE
         X1=YLOW
         X2=YHIGH
      ENDIF
      GOTO 900
*----
*     Comments
*----
   40 CONTINUE
      X1=0
      X2=XSIZ
      GOTO 900
*----
*     Characters and numbers for scatter plots and tables
*----
   50 GOTO 999
*
  900 CONTINUE
*
 999  END
