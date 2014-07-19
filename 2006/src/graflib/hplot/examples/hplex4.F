*
* $Id: hplex4.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplex4.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.13/00 01/07/92  16.13.04  by  O.Couet
*-- Author :
      SUBROUTINE HPLEX4
*.==========>
*.           Examples of HPLOT options
*..=========>
*
      DIMENSION X(100),Y(100),EX(100),EY(100)
*
      CALL HTITLE('EXAMPLE NO = 4')
*
      CALL HCOPY(110,310,' ')
      CALL HRESET(310,' ')
      CALL HPLSET('XMGL',1.)
      CALL HPLSET('YMGL',1.)
      CALL HPLSET('XMGR',1.)
      CALL HPLSET('YMGU',1.)
      CALL HPLSET('VSIZ',0.2)
      CALL HPLSET('XVAL',0.15)
      CALL HPLSET('YGTI',0.3)
      CALL HPLSET('YHTI',0.6)
      CALL HPLSIZ(14.5,21.,' ')
      CALL HPLZON(1,2,1,' ')
      CALL HMAXIM(310,200.)
      CALL HMINIM(310,-25.)
      CALL HPLOT(310,' ',' ',0)
      CALL HREBIN(110,X,Y,EX,EY,50,1,100)
      CALL HPLERR(X,Y,EX,EY,48,' ',25,0.15)
      CALL HPLKEY(9.,18.,25,'p,K^+!,K^-!,[S,W')
*
      CALL HPLOT(310,' ',' ',0)
      CALL HREBIN(110,X,Y,EX,EY,20,1,100)
      CALL HPLERR(X,Y,EX,EY,20,' ',22,0.2)
      CALL HPLKEY(9.,8.,22,'[p^+!,p^-!,m^+!,m^-')
      CALL HDELET(120)
      CALL HDELET(130)
      CALL HDELET(310)
*
      END
