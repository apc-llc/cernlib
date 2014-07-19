*
* $Id: hplex3.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplex3.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 02/07/92  10.00.55  by  O.Couet
*-- Author :
      SUBROUTINE HPLEX3
*.==========>
*.           Examples of HPLOT options
*..=========>
*
      CALL HTITLE('EXAMPLE NO = 3')
*
      CALL HPLSIZ(14.5,20.,' ')
      CALL HPLSET('XMGL',1.0)
      CALL HPLSET('YMGL',1.0)
      CALL HPLSET('XMGR',1.0)
      CALL HPLSET('YMGU',1.0)
      CALL HPLSET('YHTI',0.6)
      CALL HPLSET('GSIZ',0.5)
      CALL HOPERA(110,'+',110,120,0.5,0.)
      CALL HOPERA(120,'+',120,130,0.5,0.)
      CALL HPLSET('PASS',5.)
      CALL HPLSET('CSHI',0.03)
      CALL HPLSET('XVAL',0.15)
      CALL HPLOPT('TIC ',1)
      CALL HPLOT(110,' ',' ',0)
      CALL HPLSET('HTYP',245.)
      CALL HPLOT(120,'S',' ',0)
      CALL HPLSET('HTYP',254.)
      CALL HPLOT(130,'S',' ',0)
      CALL HPLSOF(7.,12.,'LEP4 Very Preliminary',0.5,45.,99.,-1)
*
      END
