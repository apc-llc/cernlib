*
* $Id: ihiso.F,v 1.1.1.1 1996/02/14 13:10:49 mclareni Exp $
*
* $Log: ihiso.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:49  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/06 27/08/93  13.53.33  by  O.Couet
*-- Author :
      SUBROUTINE IHISO(NS,S,NX,NY,NZ,X,Y,Z,F,DRFACE,CHOPT)
************************************************************************
*                                                                      *
*     IHISO                                      Date:    21.07.92     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 22.08.93     *
*                                                                      *
*     Function: Draw set of isosurfaces for a scalar function          *
*               defined on a grid.                                     *
*                                                                      *
*     Input: NS          - number of isosurfaces                       *
*            S(*)        - isosurface values                           *
*            NX          - number of slices along X                    *
*            NY          - number of slices along Y                    *
*            NZ          - number of slices along Z                    *
*            X(*)        - slices along X                              *
*            Y(*)        - slices along Y                              *
*            Z(*)        - slices along Z                              *
*            F(NX,NY,NZ) - function values                             *
*                                                                      *
*            DRFACE(ICODES,XYZ,NP,IFACE,T) - routine for face drawing  *
*              ICODES(1) - isosurface number                           *
*              ICODES(2) - isosurface number                           *
*              ICODES(3) - isosurface number                           *
*              NP        - number of nodes in face                     *
*              IFACE(NP) - face                                        *
*              T(NP)     - additional function (lightness)             *
*                                                                      *
*            CHOPT - options: 'BF' - from BACK to FRONT                *
*                             'FB' - from FRONT to BACK                *
*                                                                      *
************************************************************************
#include "higz/hcmctr.inc"
      CHARACTER*(*)     CHOPT
      EXTERNAL          DRFACE
      REAL              S(*),X(*),Y(*),Z(*)
      REAL              P0(3),P1(3),P2(3),P3(3),F(NX,NY,NZ),T(3)
      REAL              P(3,8),PF(8),PN(3,8)
      INTEGER           IND(3,8),ICODES(3)
      DATA              IND/0,0,0, 1,0,0, 1,0,1, 0,0,1,
     &                      0,1,0, 1,1,0, 1,1,1, 0,1,1/
*-
      NSURF  = NS
      IF (NSURF .GT. NISO) THEN
        WRITE(*,*) 'IHISO: Warning: No. of isosurfaces great then',NISO
        WRITE(*,*) 'IHISO: Increase parameter NISO in COMMON /HCMCTR/'
      END IF
      IOPT   = 2
      IF (CHOPT(1:1).EQ.'B' .OR. CHOPT(1:1).EQ.'b')     IOPT = 1
*
**          F I N D   X - , Y - , Z - C R I T I C A L
**          This logic works for parallel projection only.
**          For central projection another logic should be implemented.
*
      P0(1)  = X(1)
      P0(2)  = Y(1)
      P0(3)  = Z(1)
      CALL IHWTON(P0,P0)
      P1(1)  = X(NX)
      P1(2)  = Y(1)
      P1(3)  = Z(1)
      CALL IHWTON(P1,P1)
      P2(1)  = X(1)
      P2(2)  = Y(NY)
      P2(3)  = Z(1)
      CALL IHWTON(P2,P2)
      P3(1)  = X(1)
      P3(2)  = Y(1)
      P3(3)  = Z(NZ)
      CALL IHWTON(P3,P3)
      IXCRIT = NX
      IYCRIT = NY
      IZCRIT = NZ
      IF (P1(3) .LT. P0(3))     IXCRIT = 1
      IF (P2(3) .LT. P0(3))     IYCRIT = 1
      IF (P3(3) .LT. P0(3))     IZCRIT = 1
*
**          L O O P   A L O N G   G R I D
**          This logic works for both (parallel & central) projections.
*
      INCRX  = 1
      INCRY  = 1
      INCRZ  = 1
  110 IF (INCRZ .GE. 0) THEN
        IF (IOPT .EQ. 1)        IZ1 = 1
        IF (IOPT .EQ. 1)        IZ2 = IZCRIT-1
        IF (IOPT .EQ. 2)        IZ1 = IZCRIT
        IF (IOPT .EQ. 2)        IZ2 = NZ - 1
      ELSE
        IF (IOPT .EQ. 1)        IZ1 = NZ - 1
        IF (IOPT .EQ. 1)        IZ2 = IZCRIT
        IF (IOPT .EQ. 2)        IZ1 = IZCRIT-1
        IF (IOPT .EQ. 2)        IZ2 = 1
      END IF
      DO 530 IZ=IZ1,IZ2,INCRZ
  120   IF (INCRY .GE. 0) THEN
          IF (IOPT .EQ. 1)      IY1 = 1
          IF (IOPT .EQ. 1)      IY2 = IYCRIT-1
          IF (IOPT .EQ. 2)      IY1 = IYCRIT
          IF (IOPT .EQ. 2)      IY2 = NY - 1
        ELSE
          IF (IOPT .EQ. 1)      IY1 = NY - 1
          IF (IOPT .EQ. 1)      IY2 = IYCRIT
          IF (IOPT .EQ. 2)      IY1 = IYCRIT-1
          IF (IOPT .EQ. 2)      IY2 = 1
        END IF
        DO 520 IY=IY1,IY2,INCRY
  130     IF (INCRX .GE. 0) THEN
            IF (IOPT .EQ. 1)    IX1 = 1
            IF (IOPT .EQ. 1)    IX2 = IXCRIT-1
            IF (IOPT .EQ. 2)    IX1 = IXCRIT
            IF (IOPT .EQ. 2)    IX2 = NX - 1
          ELSE
            IF (IOPT .EQ. 1)    IX1 = NX - 1
            IF (IOPT .EQ. 1)    IX2 = IXCRIT
            IF (IOPT .EQ. 2)    IX1 = IXCRIT-1
            IF (IOPT .EQ. 2)    IX2 = 1
          END IF
          DO 510 IX=IX1,IX2,INCRX
            NNOD   = 0
            NTRIA  = 0
            IREADY = 0
            DO 400 ISURF=1,NSURF
              FSURF = S(ISURF)
              IF (F(IX,  IY,  IZ)   .GE. FSURF) GOTO 210
              IF (F(IX+1,IY,  IZ)   .GE. FSURF) GOTO 220
              IF (F(IX,  IY+1,IZ)   .GE. FSURF) GOTO 220
              IF (F(IX+1,IY+1,IZ)   .GE. FSURF) GOTO 220
              IF (F(IX,  IY,  IZ+1) .GE. FSURF) GOTO 220
              IF (F(IX+1,IY,  IZ+1) .GE. FSURF) GOTO 220
              IF (F(IX,  IY+1,IZ+1) .GE. FSURF) GOTO 220
              IF (F(IX+1,IY+1,IZ+1) .GE. FSURF) GOTO 220
              GOTO 400
  210         IF (F(IX+1,IY,  IZ)   .LT. FSURF) GOTO 220
              IF (F(IX,  IY+1,IZ)   .LT. FSURF) GOTO 220
              IF (F(IX+1,IY+1,IZ)   .LT. FSURF) GOTO 220
              IF (F(IX,  IY,  IZ+1) .LT. FSURF) GOTO 220
              IF (F(IX+1,IY,  IZ+1) .LT. FSURF) GOTO 220
              IF (F(IX,  IY+1,IZ+1) .LT. FSURF) GOTO 220
              IF (F(IX+1,IY+1,IZ+1) .LT. FSURF) GOTO 220
              GOTO 400
*
**          P R E P A R E   C U B E   ( P A R A L L E P I P E D )
*
  220        IF (IREADY .NE.0)                  GOTO 310
             IREADY = 1
             DO 300 I=1,8
                KX     = IX + IND(1,I)
                KY     = IY + IND(2,I)
                KZ     = IZ + IND(3,I)
                P(1,I) = X(KX)
                P(2,I) = Y(KY)
                P(3,I) = Z(KZ)
                PF(I)  = F(KX,KY,KZ)
*           F I N D   X - G R A D I E N T
                IF (KX .EQ. 1) THEN
                  PN(1,I) = (F(2,KY,KZ)-F(1,KY,KZ))/(X(2)-X(1))
                ELSE IF (KX .EQ. NX) THEN
                  PN(1,I) = (F(KX,KY,KZ)-F(KX-1,KY,KZ))/(X(KX)-X(KX-1))
                ELSE
                  D1     = X(KX) - X(KX-1)
                  D2     = X(KX+1) - X(KX)
                  IF (D1 .EQ. D2) THEN
                    PN(1,I) = (F(KX+1,KY,KZ)-F(KX-1,KY,KZ))/(D1+D1)
                  ELSE
                    DF1     = F(KX,KY,KZ) - F(KX-1,KY,KZ)
                    DF2     = F(KX+1,KY,KZ) - F(KX,KY,KZ)
                    PN(1,I) = (DF1*D2*D2+DF2*D1*D1)/(D1*D2*D2+D2*D1*D1)
                  END IF
                END IF
*           F I N D   Y - G R A D I E N T
                IF (KY .EQ. 1) THEN
                  PN(2,I) = (F(KX,2,KZ)-F(KX,1,KZ))/(Y(2)-Y(1))
                ELSE IF (KY .EQ. NY) THEN
                  PN(2,I) = (F(KX,KY,KZ)-F(KX,KY-1,KZ))/(Y(KY)-Y(KY-1))
                ELSE
                  D1     = Y(KY) - Y(KY-1)
                  D2     = Y(KY+1) - Y(KY)
                  IF (D1 .EQ. D2) THEN
                    PN(2,I) = (F(KX,KY+1,KZ)-F(KX,KY-1,KZ))/(D1+D1)
                  ELSE
                    DF1    = F(KX,KY,KZ) - F(KX,KY-1,KZ)
                    DF2    = F(KX,KY+1,KZ) - F(KX,KY,KZ)
                    PN(2,I) = (DF1*D2*D2+DF2*D1*D1)/(D1*D2*D2+D2*D1*D1)
                  END IF
                END IF
*           F I N D   Z - G R A D I E N T
                IF (KZ .EQ. 1) THEN
                  PN(3,I) = (F(KX,KY,2)-F(KX,KY,1))/(Z(2)-Z(1))
                ELSE IF (KZ .EQ. NZ) THEN
                  PN(3,I) = (F(KX,KY,KZ)-F(KX,KY,KZ-1))/(Z(KZ)-Z(KZ-1))
                ELSE
                  D1     = Z(KZ) - Z(KZ-1)
                  D2     = Z(KZ+1) - Z(KZ)
                  IF (D1 .EQ. D2) THEN
                    PN(3,I) = (F(KX,KY,KZ+1)-F(KX,KY,KZ-1))/(D1+D1)
                  ELSE
                    DF1    = F(KX,KY,KZ) - F(KX,KY,KZ-1)
                    DF2    = F(KX,KY,KZ+1) - F(KX,KY,KZ)
                    PN(3,I) = (DF1*D2*D2+DF2*D1*D1)/(D1*D2*D2+D2*D1*D1)
                  END IF
                END IF
  300         CONTINUE
*
**          F I N D   S E T   O F   T R I A N G L E S
*
  310         CALL IHMCUB(S(ISURF),P,PF,PN,KNOD,KTRIA,
     &                    XYZ(1,NNOD+1),GRAD(1,NNOD+1),ITRIA(1,NTRIA+1))
              DO 330 I=NTRIA+1,NTRIA+KTRIA
                DO 320 J=1,3
                  IBASE = NNOD
                  IF (ITRIA(J,I) .LT. 0)        IBASE =-NNOD
                  ITRIA(J,I) = ITRIA(J,I) + IBASE
  320           CONTINUE
                IATTR(I) = ISURF
  330         CONTINUE
              NNOD   = NNOD + KNOD
              NTRIA  = NTRIA + KTRIA
  400       CONTINUE
*
**          D E P T H   S O R T,   D R A W I N G
*
            IF (NTRIA .EQ. 0)   GOTO 510
            DO 410 I=1,NNOD
              CALL IHWTON(XYZ(1,I),XYZN(1,I))
              CALL IHLUMI(GRAD(1,I),W)
              GRAD(1,I) = W
  410       CONTINUE
            CALL IHZDEP(XYZN,NTRIA,ITRIA,DTRIA,ABCD,IORDER)
            IF (NTRIA .EQ. 0)   GOTO 510
            INCR  = 1
            IF (IOPT .EQ. 1)            INCR =-1
            I1    = 1
            IF (INCR .EQ. -1)           I1 = NTRIA
            I2    = NTRIA - I1 + 1
            DO 420 I=I1,I2,INCR
              K      = IORDER(I)
              T(1)   = GRAD(1,IABS(ITRIA(1,K)))
              T(2)   = GRAD(1,IABS(ITRIA(2,K)))
              T(3)   = GRAD(1,IABS(ITRIA(3,K)))
              ICODES(1) = IATTR(K)
              ICODES(2) = IATTR(K)
              ICODES(3) = IATTR(K)
              CALL DRFACE(ICODES,XYZ,3,ITRIA(1,K),T)
  420       CONTINUE
  510     CONTINUE
          INCRX  =-INCRX
          IF (INCRX .LT. 0)     GOTO 130
  520   CONTINUE
        INCRY  =-INCRY
        IF (INCRY .LT. 0)       GOTO 120
  530 CONTINUE
      INCRZ  =-INCRZ
      IF (INCRZ .LT. 0)         GOTO 110
      END
