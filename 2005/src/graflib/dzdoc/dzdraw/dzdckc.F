*
* $Id: dzdckc.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdckc.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCKC(XC,YC,X0,Y0,LB,IB,L,IFL)
*.
*...DZDCKC  check where the locator points to in a bank
*.
*. INPUT     :    XC, YC    position of the locator
*.                X0, Y0    position of bank (center)
*.                LB, IB    the array containing the ZEBRA store
*.                          as used in the call to MZSTOR
*.                L         link to bank
*. OUTPUT    :
*.                L         link to possible next bank
*.                IFL       flag indicating where locator is(see DZDGET)
*.
*. COMMON    :
*. SEQUENCE  : DZDPRM
*. CALLED    : DZDRA2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      INTEGER LB(999),IB(999)
#include "dzdprm.inc"
      LOGICAL DZDINB
      EXTERNAL DZDINB
*--
      IF(X0+2.*DXTB .LE. XRANGE .AND. LB(L) .NE. 0)THEN
*        make its a bit smaller
         DXX=DXTB-3.*GRIDX
      ELSE
         DXX=DXTB
      ENDIF
      XLBC=2.5*GRIDX
      IFL = 0
      NS = IB(L-2)
      IF(NS .LT. 4) NS = 4
      IF(DZDINB(XC,YC,
     + X0-FLOAT(NS)*DXLK,X0,Y0,Y0+DYB))THEN
            IFL = 2
            GOTO 10
      ENDIF
*10/2
      IF(DZDINB(XC,YC,
     + X0,X0+XLBC,Y0,Y0+DYB))THEN
            IFL = 1
            GOTO 10
      ENDIF
      IF(DZDINB(XC,YC,
     + X0,X0+GRIDX,Y0+DYB,Y0+2.*DYB))THEN
            IFL = 5
            GOTO 10
      ENDIF
      IF(DZDINB(XC,YC,
     + X0+XLBC,X0+XLBC+DXX,Y0+DYB,Y0+2.*DYB))THEN
            IFL = 6
            GOTO 10
      ENDIF
      IF(DZDINB(XC,YC,
     + X0+XLBC,X0+XLBC+DXX,Y0,Y0+DYB))THEN
            IFL = 3
            GOTO 10
      ENDIF
*10/2
      IF(LB(L) .NE. 0)THEN
         IF(DZDINB(XC,YC,
     +   X0+XLBC+DXX+0.5*GRIDX,X0+XLBC+DXX+20.*GRIDX,Y0,Y0+DYB))THEN
            IFL = 4
            L = LB(L)
         ENDIF
      ENDIF
   10 CONTINUE
      RETURN
      END
***********************************************************************
