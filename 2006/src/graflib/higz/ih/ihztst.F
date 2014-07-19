*
* $Id: ihztst.F,v 1.1.1.1 1996/02/14 13:10:55 mclareni Exp $
*
* $Log: ihztst.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/05 26/08/93  09.53.55  by  O.Couet
*-- Author :
      SUBROUTINE IHZTST(DEL,XYZ,I1,I2,IFACE,ABCD,IREP)
************************************************************************
*                                                                      *
*     IHZTST                                     Date:    18.03.90     *
*     Author: E. Chernyaev (IHEP)                Revised: 30.07.93     *
*                                                                      *
*     Function: Test edge against face (triangle)                      *
*                                                                      *
*     Input: DEL      - precision                                      *
*            XYZ(3,*) - nodes                                          *
*            I1       - 1-st node of edge                              *
*            I2       - 2-nd node of edge                              *
*            IFACE(3) - triangular face                                *
*            ABCD(4)  - face plane                                     *
*                                                                      *
*     Output: IREP:-1 - edge under face                                *
*                   0 - no decision                                    *
*                  +1 - edge before face                               *
*                                                                      *
************************************************************************
      REAL              XYZ(3,*),ABCD(4),D(3),DELTA(3),T(2)
      INTEGER           IFACE(3)
      EQUIVALENCE       (DX,DELTA(1)),(DY,DELTA(2)),(DZ,DELTA(3))
*-
      IREP   = 0
*
**          F I N D   I N T E R S E C T I O N   P O I N T S
*
      DX     = XYZ(1,I2) - XYZ(1,I1)
      DY     = XYZ(2,I2) - XYZ(2,I1)
      DZ     = XYZ(3,I2) - XYZ(3,I1)
      IF (ABS(DX).LE.DEL .AND. ABS(DY).LE.DEL)  GOTO 999
      IXY    = 1
      IF (ABS(DY) .GT. ABS(DX))                 IXY = 2
      A      = DY
      B      =-DX
      C      =-(A*XYZ(1,I1) + B*XYZ(2,I1))
      D(1)   = A*XYZ(1,IFACE(1)) + B*XYZ(2,IFACE(1)) + C
      D(2)   = A*XYZ(1,IFACE(2)) + B*XYZ(2,IFACE(2)) + C
      D(3)   = A*XYZ(1,IFACE(3)) + B*XYZ(2,IFACE(3)) + C
      K      = 0
      DO 100 I=1,3
        K1     = I
        K2     = I + 1
        IF (I .EQ. 3)                           K2 = 1
        IF (D(K1).GE.0. .AND. D(K2).GE.0.)      GOTO 100
        IF (D(K1).LT.0. .AND. D(K2).LT.0.)      GOTO 100
        D1     = D(K1) / (D(K1) - D(K2))
        D2     = D(K2) / (D(K1) - D(K2))
        XY     = D1*XYZ(IXY,IFACE(K2)) - D2*XYZ(IXY,IFACE(K1))
        K      = K + 1
        T(K)   = (XY-XYZ(IXY,I1)) / DELTA(IXY)
        IF (K .EQ. 2)                           GOTO 200
  100 CONTINUE
      GOTO 999
*
**          C O M P A R E   Z - D E P T H
*
  200 TMIN   = AMIN1(T(1),T(2))
      TMAX   = AMAX1(T(1),T(2))
      IF (TMIN.GT.1. .OR. TMAX.LT.0)            GOTO 999
      IF (TMIN .LT. 0.)                         TMIN = 0.
      IF (TMAX .GT. 1.)                         TMAX = 1.
      TMID   = (TMIN + TMAX) / 2.
      X      = DX*TMID + XYZ(1,I1)
      Y      = DY*TMID + XYZ(2,I1)
      Z      = DZ*TMID + XYZ(3,I1)
      DD     = ABCD(1)*X + ABCD(2)*Y + ABCD(3)*Z + ABCD(4)
      IF (DD .GT. DEL)                          GOTO 997
      IF (DD .LT.-DEL)                          GOTO 998
      GOTO 999
*
  997 IREP   =+1
      GOTO 999
  998 IREP   =-1
      GOTO 999
  999 RETURN
      END
