*
* $Id: ihimpf.F,v 1.2 2002/04/05 13:20:05 couet Exp $
*
* $Log: ihimpf.F,v $
* Revision 1.2  2002/04/05 13:20:05  couet
* - The variable KSURF was initialized but never used
*
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  14.06.00  by  O.Couet
*-- Author :
      SUBROUTINE IHIMPF(FUN,RMIN,RMAX,NX,NY,NZ,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHIMPF                                     Date:    22.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 12.01.92     *
*                                                         11.09.92     *
*                                                         25.08.93     *
*     Function: Draw implicit function FUN(X,Y,Z) = 0 in cartesian     *
*               coordinates using hidden surface removal algorithm     *
*               "Painter".                                             *
*                                                                      *
*     Input: FUN      - external routine FUN(X,Y,Z)                    *
*            RMIN(3)  - min scope coordinates                          *
*            RMAX(3)  - max scope coordinates                          *
*            NX       - number of steps along X                        *
*            NY       - number of steps along Y                        *
*            NZ       - number of steps along Z                        *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(*) - set of codes for this face                  *
*                ICODES(1) - 1                                         *
*                ICODES(2) - 1                                         *
*                ICODES(3) - 1                                         *
*              NP        - number of nodes in face                     *
*              IFACE(NP) - face                                        *
*              T(NP)     - additional function (lightness)             *
*                                                                      *
*            CHOPT - options: 'BF' - from BACK to FRONT                *
*                             'FB' - from FRONT to BACK                *
*                                                                      *
************************************************************************
#include "higz/hcmctr.inc"
#include "higz/hctnor.inc"
      PARAMETER         (FDEL = 0.)
      REAL              RMIN(3),RMAX(3)
      REAL              P(3,8),PF(8),PN(3,8),T(3)
      INTEGER           ICODES(3)
      CHARACTER*(*)     CHOPT
      EXTERNAL          FUN,DRFACE
*-
**          D E F I N E   O R D E R   O F   D R A W I N G
*
      IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b') THEN
        INCRX  =+1
        INCRY  =+1
        INCRZ  =+1
      ELSE
        INCRX  =-1
        INCRY  =-1
        INCRZ  =-1
      END IF
      IF (TNORM(1,3) .LT. 0.)           INCRX =-INCRX
      IF (TNORM(2,3) .LT. 0.)           INCRY =-INCRY
      IF (TNORM(3,3) .LT. 0.)           INCRZ =-INCRZ
      IX1    = 1
      IY1    = 1
      IZ1    = 1
      IF (INCRX .EQ. -1)                IX1   = NX
      IF (INCRY .EQ. -1)                IY1   = NY
      IF (INCRZ .EQ. -1)                IZ1   = NZ
      IX2    = NX - IX1 + 1
      IY2    = NY - IY1 + 1
      IZ2    = NZ - IZ1 + 1
      DX     = (RMAX(1)-RMIN(1)) / NX
      DY     = (RMAX(2)-RMIN(2)) / NY
      DZ     = (RMAX(3)-RMIN(3)) / NZ
*
**          D R A W   F U N C T I O N
*
      DO 530 IZ=IZ1,IZ2,INCRZ
        Z1     = (IZ-1)*DZ + RMIN(3)
        Z2     = Z1 + DZ
        P(3,1) = Z1
        P(3,2) = Z1
        P(3,3) = Z1
        P(3,4) = Z1
        P(3,5) = Z2
        P(3,6) = Z2
        P(3,7) = Z2
        P(3,8) = Z2
        DO 520 IY=IY1,IY2,INCRY
          Y1     = (IY-1)*DY + RMIN(2)
          Y2     = Y1 + DY
          P(2,1) = Y1
          P(2,2) = Y1
          P(2,3) = Y2
          P(2,4) = Y2
          P(2,5) = Y1
          P(2,6) = Y1
          P(2,7) = Y2
          P(2,8) = Y2
          IF (INCRX .EQ.+1) THEN
            X2    = RMIN(1)
            PF(2) = FUN(X2,Y1,Z1)
            PF(3) = FUN(X2,Y2,Z1)
            PF(6) = FUN(X2,Y1,Z2)
            PF(7) = FUN(X2,Y2,Z2)
          ELSE
            X1    = RMAX(1)
            PF(1) = FUN(X1,Y1,Z1)
            PF(4) = FUN(X1,Y2,Z1)
            PF(5) = FUN(X1,Y1,Z2)
            PF(8) = FUN(X1,Y2,Z2)
          END IF
          DO 510 IX=IX1,IX2,INCRX
            ICODES(1) = IX
            ICODES(2) = IY
            ICODES(3) = IZ
            IF (INCRX .EQ.+1) THEN
              X1     = X2
              X2     = X2 + DX
              PF(1)  = PF(2)
              PF(4)  = PF(3)
              PF(5)  = PF(6)
              PF(8)  = PF(7)
              PF(2)  = FUN(X2,Y1,Z1)
              PF(3)  = FUN(X2,Y2,Z1)
              PF(6)  = FUN(X2,Y1,Z2)
              PF(7)  = FUN(X2,Y2,Z2)
            ELSE
              X2     = X1
              X1     = X1 - DX
              PF(2)  = PF(1)
              PF(3)  = PF(4)
              PF(6)  = PF(5)
              PF(7)  = PF(8)
              PF(1)  = FUN(X1,Y1,Z1)
              PF(4)  = FUN(X1,Y2,Z1)
              PF(5)  = FUN(X1,Y1,Z2)
              PF(8)  = FUN(X1,Y2,Z2)
            END IF
            IF (PF(1) .GE. -FDEL)       GOTO 110
            IF (PF(2) .GE. -FDEL)       GOTO 120
            IF (PF(3) .GE. -FDEL)       GOTO 120
            IF (PF(4) .GE. -FDEL)       GOTO 120
            IF (PF(5) .GE. -FDEL)       GOTO 120
            IF (PF(6) .GE. -FDEL)       GOTO 120
            IF (PF(7) .GE. -FDEL)       GOTO 120
            IF (PF(8) .GE. -FDEL)       GOTO 120
            GOTO 510
  110       IF (PF(2) .LT. -FDEL)       GOTO 120
            IF (PF(3) .LT. -FDEL)       GOTO 120
            IF (PF(4) .LT. -FDEL)       GOTO 120
            IF (PF(5) .LT. -FDEL)       GOTO 120
            IF (PF(6) .LT. -FDEL)       GOTO 120
            IF (PF(7) .LT. -FDEL)       GOTO 120
            IF (PF(8) .LT. -FDEL)       GOTO 120
            GOTO 510
  120       P(1,1) = X1
            P(1,2) = X2
            P(1,3) = X2
            P(1,4) = X1
            P(1,5) = X1
            P(1,6) = X2
            P(1,7) = X2
            P(1,8) = X1
*
**          F I N D   G R A D I E N T S
*
            IF (IX .EQ. 1) THEN
              PN(1,1) = (PF(2) - PF(1)) / DX
              PN(1,4) = (PF(3) - PF(4)) / DX
              PN(1,5) = (PF(6) - PF(5)) / DX
              PN(1,8) = (PF(7) - PF(8)) / DX
            ELSE
              PN(1,1) = (PF(2) - FUN(X1-DX,Y1,Z1)) / (DX + DX)
              PN(1,4) = (PF(3) - FUN(X1-DX,Y2,Z1)) / (DX + DX)
              PN(1,5) = (PF(6) - FUN(X1-DX,Y1,Z2)) / (DX + DX)
              PN(1,8) = (PF(7) - FUN(X1-DX,Y2,Z2)) / (DX + DX)
            END IF
            IF (IX .EQ. NX) THEN
              PN(1,2) = (PF(2) - PF(1)) / DX
              PN(1,3) = (PF(3) - PF(4)) / DX
              PN(1,6) = (PF(6) - PF(5)) / DX
              PN(1,7) = (PF(7) - PF(8)) / DX
            ELSE
              PN(1,2) = (FUN(X2+DX,Y1,Z1) - PF(1)) / (DX + DX)
              PN(1,3) = (FUN(X2+DX,Y2,Z1) - PF(4)) / (DX + DX)
              PN(1,6) = (FUN(X2+DX,Y1,Z2) - PF(5)) / (DX + DX)
              PN(1,7) = (FUN(X2+DX,Y2,Z2) - PF(8)) / (DX + DX)
            END IF
*           F I N D   Y - G R A D I E N T
            IF (IY .EQ. 1) THEN
              PN(2,1) = (PF(4) - PF(1)) / DY
              PN(2,2) = (PF(3) - PF(2)) / DY
              PN(2,5) = (PF(8) - PF(5)) / DY
              PN(2,6) = (PF(7) - PF(6)) / DY
            ELSE
              PN(2,1) = (PF(4) - FUN(X1,Y1-DY,Z1)) / (DY + DY)
              PN(2,2) = (PF(3) - FUN(X2,Y1-DY,Z1)) / (DY + DY)
              PN(2,5) = (PF(8) - FUN(X1,Y1-DY,Z2)) / (DY + DY)
              PN(2,6) = (PF(7) - FUN(X2,Y1-DY,Z2)) / (DY + DY)
            END IF
            IF (IY .EQ. NY) THEN
              PN(2,3) = (PF(3) - PF(2)) / DY
              PN(2,4) = (PF(4) - PF(1)) / DY
              PN(2,7) = (PF(7) - PF(6)) / DY
              PN(2,8) = (PF(8) - PF(5)) / DY
            ELSE
              PN(2,3) = (FUN(X2,Y2+DY,Z1) - PF(2)) / (DY + DY)
              PN(2,4) = (FUN(X1,Y2+DY,Z1) - PF(1)) / (DY + DY)
              PN(2,7) = (FUN(X2,Y2+DY,Z2) - PF(6)) / (DY + DY)
              PN(2,8) = (FUN(X1,Y2+DY,Z2) - PF(5)) / (DY + DY)
            END IF
*           F I N D   Z - G R A D I E N T
            IF (IZ .EQ. 1) THEN
              PN(3,1) = (PF(5) - PF(1)) / DZ
              PN(3,2) = (PF(6) - PF(2)) / DZ
              PN(3,3) = (PF(7) - PF(3)) / DZ
              PN(3,4) = (PF(8) - PF(4)) / DZ
            ELSE
              PN(3,1) = (PF(5) - FUN(X1,Y1,Z1-DZ)) / (DZ + DZ)
              PN(3,2) = (PF(6) - FUN(X2,Y1,Z1-DZ)) / (DZ + DZ)
              PN(3,3) = (PF(7) - FUN(X2,Y2,Z1-DZ)) / (DZ + DZ)
              PN(3,4) = (PF(8) - FUN(X1,Y2,Z1-DZ)) / (DZ + DZ)
            END IF
            IF (IZ .EQ. NZ) THEN
              PN(3,5) = (PF(5) - PF(1)) / DZ
              PN(3,6) = (PF(6) - PF(2)) / DZ
              PN(3,7) = (PF(7) - PF(3)) / DZ
              PN(3,8) = (PF(8) - PF(4)) / DZ
            ELSE
              PN(3,5) = (FUN(X1,Y1,Z2+DZ) - PF(1)) / (DZ + DZ)
              PN(3,6) = (FUN(X2,Y1,Z2+DZ) - PF(2)) / (DZ + DZ)
              PN(3,7) = (FUN(X2,Y2,Z2+DZ) - PF(3)) / (DZ + DZ)
              PN(3,8) = (FUN(X1,Y2,Z2+DZ) - PF(4)) / (DZ + DZ)
            END IF
            FSURF = 0.
            CALL IHMCUB(FSURF,P,PF,PN,NNOD,NTRIA,XYZ,GRAD,ITRIA)
            IF (NTRIA .EQ. 0)   GOTO 510
            DO 410 I=1,NNOD
              CALL IHWTON(XYZ(1,I),XYZN(1,I))
              CALL IHLUMI(GRAD(1,I),W)
              GRAD(1,I) = W
  410       CONTINUE
            CALL IHZDEP(XYZN,NTRIA,ITRIA,DTRIA,ABCD,IORDER)
            IF (NTRIA .EQ. 0)   GOTO 510
            INCR  = 1
            IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b')       INCR =-1
            I1    = 1
            IF (INCR .EQ. -1)           I1 = NTRIA
            I2    = NTRIA - I1 + 1
            DO 420 I=I1,I2,INCR
              K      = IORDER(I)
              T(1)   = GRAD(1,IABS(ITRIA(1,K)))
              T(2)   = GRAD(1,IABS(ITRIA(2,K)))
              T(3)   = GRAD(1,IABS(ITRIA(3,K)))
              CALL DRFACE(ICODES,XYZ,3,ITRIA(1,K),T)
  420       CONTINUE
  510     CONTINUE
  520   CONTINUE
  530 CONTINUE
      END
