*
* $Id: hplex2.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplex2.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 02/07/92  10.01.41  by  O.Couet
*-- Author :
      SUBROUTINE HPLEX2
*.==========>
*.           Examples of basic HPLOT : 2-DIM histograms
*..=========>
*
      CALL HTITLE('EXAMPLE NO = 2')
*
      CALL HPLSIZ(14.,14.,' ')
      CALL HPLSET('YGTI',0.3)
      CALL HPLSET('XMGL',1.)
      CALL HPLSET('YMGL',1.)
      CALL HPLSET('XMGR',1.)
      CALL HPLSET('YMGU',1.)
      CALL HPLSET('VSIZ',0.2)
      CALL HPLSET('YHTI',0.6)
      CALL IGSET('MTYP',1.)
      CALL HPLOT(200,' ',' ',0)
      CALL HPLCON(200,10,1)
      CALL HPLSET('XMGL',1.5)
      CALL HPLSET('YMGL',2.0)
      CALL HPLSET('YHTI',1.5)
      CALL HPLEGO(200,30.,30.)
      CALL HPLSUR(200,30.,30.,1)
*
      END
