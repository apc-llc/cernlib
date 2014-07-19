*
* $Id: igelli.F,v 1.3 1999/06/21 16:45:21 couet Exp $
*
* $Log: igelli.F,v $
* Revision 1.3  1999/06/21 16:45:21  couet
* -  An ellipse with FAIS.NE.0, had an axis drawn.
*
* Revision 1.2  1998/03/20 15:46:06  couet
* - The ELLIPSE command always draw the ellipse with the first angle equal to
*   zero even if a non zero value is given.
*
*         null 0 10 0 10
*         ellipse 5 5 4 2 90 270
*
*   Some improvements have also be done in the ELLIPSE drawing:
*
*       nul 0 10 0 10 ; elli 5 5 2 4 1 360 30
*       set bord 1
*       nul 0 10 0 10 ; elli 5 5 2 4 1 360 30
*       set fais 1
*       set faci 2
*       nul 0 10 0 10 ; elli 5 5 2 4 1 360 30
*
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/05 17/10/95  18.17.05  by  O.Couet
*-- Author :    O.Couet   17/10/95
      SUBROUTINE IGELLI(XCI,YCI,RXI,RYI,PHIMIN,PHIMAI,THETA)
*.===========>
*.
*.  Draw an ellipse in the current NT
*.  (original code: Ellipse macro from Michael Kelsey)
*.
*.  Draws an ellipse in the current normalization transformation.
*.  The parameter THETA rotates the ellipse major and minor axes
*.  (RX and RY) relative to the coordinates by the given angle.
*.
*. _Input parameters:
*.
*.  REAL XC     :  X coord of centre.
*.  REAL YC     :  Y coord of centre.
*.  REAL RX     :  X radius of ellipse.
*.  REAL RY     :  Y radius of ellipse.
*.  REAL PHIMIN :  Minimum angle (degrees).
*.  REAL PHIMAX :  Maximum angle (degrees).
*.  REAL THETA  :  Rotation of axes of (degrees).
*.
*..==========>
#include "higz/higraf.inc"
#include "higz/hiatt.inc"
      PARAMETER (PI=3.1415926)
*.______________________________________
*
      XC = XCI
      YC = YCI
      RX = RXI
      RY = RYI
      IF (RY.EQ.0.) RY = RX
*
      PHIMAX = PHIMAI
      IF (PHIMAX.GT.360.) PHIMAX = MOD(PHIMAX,360.)
      NP   = INT(PHIMAX-PHIMIN)+1
      DPHI = (PHIMAX-PHIMIN)/NP
*
      DO 10 I=1,NP
         PHI      = (PI/180.)*(I*DPhi+PHIMIN)
         XGRAF(I) = RX*COS(Phi)
         YGRAF(I) = RY*SIN(Phi)
   10 CONTINUE
*
      If (THETA.NE.0) THEN
         TH = THETA*PI/180.
         Do 20 I=1,NP
            X        = XGRAF(I)
            Y        = YGRAF(I)
            XGRAF(I) = X*COS(Th)-Y*SIN(Th)
            YGRAF(I) = X*SIN(Th)+Y*COS(Th)
   20    CONTINUE
      ENDIF
*
      DO 30 I=1,NP
         XGRAF(I) = XGRAF(I)+XC
         YGRAF(I) = YGRAF(I)+YC
   30 CONTINUE
*
      NPT = NP
      IF (PHIMIN.EQ.0.AND.PHIMAX.EQ.360) THEN
         NPT = NP+1
         XGRAF(NPT) = XGRAF(1)
         YGRAF(NPT) = YGRAF(1)
      ELSE
         IF (IFAIS.NE.0.OR.IBORD.NE.0) THEN
            NPT = NP+2
            XGRAF(NPT-1) = XC
            YGRAF(NPT-1) = YC
            XGRAF(NPT) = XGRAF(1)
            YGRAF(NPT) = YGRAF(1)
         ENDIF
      ENDIF
*
      IF (IFAIS.NE.0) CALL IFA(NPT,XGRAF,YGRAF)
*
      IF (IFAIS.EQ.0.OR.IBORD.NE.0) Then
         ILNOLD = ILN
         CALL ISLN(1)
         CALL IPL (NPT,XGRAF,YGRAF)
         CALL ISLN(ILNOLD)
      ENDIF
*
      END
