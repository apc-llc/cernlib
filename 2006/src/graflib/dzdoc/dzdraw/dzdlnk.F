*
* $Id: dzdlnk.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdlnk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLNK(X01, Y01, X02, Y02, ILINK, NSREAL)
*.
*...DZDLNK   draw a linkbox + the link to a bank
*.
*. INPUT     :   X01, Y01   position of link box
*.               X02, Y02   the endpoint of the link
*.               ILINK      the link number
*.               NSREAL     number of structural links
*. SEQUENCE  : DZDPRM
*. CALLS     : DZDLI2 DZDSBX
*. CALLED    : DZDRA2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
#include "dzdprm.inc"
      REAL XPOL(10), YPOL(10)
*----
*       box for link in mother bank
      CALL DZDSBX(X01,Y01,ILINK,NSREAL)
*       line to bank
      XPOL(1) = X01 + 0.5*DXLK
      YPOL(1) = Y01
      XPOL(2) = XPOL(1)
      YPOL(2) = Y02 + DYB + 0.5*GRIDY
      XPOL(3) = X02 + 0.5 * DXB
      YPOL(3) = YPOL(2)
      XPOL(4) = XPOL(3)
      YPOL(4) = Y02 + DYB
      XPOL(5) = ARRLEN
      YPOL(5) = 270.
      IF(ILINK.GT.NSREAL)CALL ISLN(3)
      CALL DZDLI2(5,XPOL,YPOL,0)
      IF(ILINK.GT.NSREAL)CALL ISLN(1)
      END
**********************************************************************
