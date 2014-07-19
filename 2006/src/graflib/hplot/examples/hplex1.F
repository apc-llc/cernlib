*
* $Id: hplex1.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplex1.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 01/07/92  16.13.03  by  O.Couet
*-- Author :
      SUBROUTINE HPLEX1
*.==========>
*.           Examples of basic HPLOT : 1-DIM histograms
*..=========>
*
      CALL HTITLE('EXAMPLE NO = 1')
*
      CALL HPLSIZ(14.,16.,' ')
      CALL HPLOT(110,' ',' ',0)
      CALL HPLSET('HTYP',333.)
      CALL HPLOT(110,' ',' ',0)
      CALL HPLAX('GeV/C',' ')
      CALL HPLSIZ(14.5,21.4,' ')
      CALL HPLZON(1,2,1,' ')
      CALL HPLOT(110,' ',' ',0)
      CALL HPLSET('HTYP',244.)
      CALL HPLOT(110,' ',' ',0)
      CALL HPLSET('HTYP',0.)
      CALL HPLZON(1,1,1,' ')
*
      END
