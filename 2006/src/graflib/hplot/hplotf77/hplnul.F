*
* $Id: hplnul.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplnul.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/14 29/06/94  11.19.13  by  O.Couet
*-- Author :
      SUBROUTINE HPLNUL
*.==========>
*. Draws page number, global title and boxes only.
*. Transformation NTHIST is selected when calling HPLSWN.
*. The same coordinates of the last drawned histogram are taken.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl13.inc"
      LOGICAL LOLD
*._____________________________
*
      DO 10 I=1,NOPT
         IOPT(I)=0
  10  CONTINUE
      LOLD   = LOUTIT
      LOUTIT = .TRUE.
      LOSAME = .FALSE.
      LO3DPL = .FALSE.
      CALL HPLSWN
      CALL HPLDES(0)
      CALL HPLDAT
      LOUTIT = LOLD
      END
