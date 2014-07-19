*
* $Id: ihrlin.F,v 1.1.1.1 1996/02/14 13:10:53 mclareni Exp $
*
* $Log: ihrlin.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:53  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/05 26/05/93  17.59.17  by  O.Couet
*-- Author :
      SUBROUTINE IHRLIN(P1,P2,NTMAX,NT,T)
************************************************************************
*                                                                      *
*     IHRLIN                                     Date:    14.05.93     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised:              *
*                                                                      *
*     Function: Find visible part of a line ("RASTER SCREEN")          *
*                                                                      *
*     Input: P1(2) - 1st point of the line                             *
*            P2(2) - 2nd point of the line                             *
*            NTMAX - max allowed number of visible segments            *
*                                                                      *
*     Output: NT     - number of visible segments of the line          *
*             T(2,*) - visible segments                                *
*                                                                      *
************************************************************************
#include "higz/hcrast.inc"
      REAL      P1(2),P2(2),T(2,*)
      INTEGER   X1,Y1,X2,Y2,DX,DY,DX2,DY2
*-
      IF (IFRAST .NE. 1) THEN
        NT     = 1
        T(1,1) = 0.
        T(2,1) = 1.
        GOTO 999
      END IF
      X1     = NXRAST * ((P1(1)-XRAST)/DXRAST) - 0.01
      Y1     = NYRAST * ((P1(2)-YRAST)/DYRAST) - 0.01
      X2     = NXRAST * ((P2(1)-XRAST)/DXRAST) - 0.01
      Y2     = NYRAST * ((P2(2)-YRAST)/DYRAST) - 0.01
      IFINVE = 0
      IF (Y1 .GT. Y2) THEN
        IFINVE = 1
        IW     = X1
        X1     = X2
        X2     = IW
        IW     = Y1
        Y1     = Y2
        Y2     = IW
      END IF
      NT     = 0
      IVIS   = 0
      IF (Y1 .GE. NYRAST)                       GOTO 999
      IF (Y2 .LT. 0)                            GOTO 999
      IF (X1.GE.NXRAST .AND. X2.GE.NXRAST)      GOTO 999
      IF (X1.LT.0 .AND. X2.LT.0)                GOTO 999
*
**          S E T   I N I T I A L   V A L U E S
*
      INCRX  =+1
      DX     = X2 - X1
      IF (DX .LT. 0) THEN
        DX     =-DX
        INCRX  =-1
      END IF
      DY     = Y2 - Y1
      DX2    = DX + DX
      DY2    = DY + DY
      IF (DY .GT. DX)                   GOTO 200
*
**          D X   . G T .   D Y
*
      DT     = 1. / (DX+1)
      DDTT   = 0.5*DT
      TCUR   =-DT
      TT     =-(DX + DY2)
      IY     = Y1
      KB     = IY*NXRAST + X1 - INCRX
      DO 120 IX=X1,X2,INCRX
        KB     = KB + INCRX
        TCUR   = TCUR + DT
        TT     = TT + DY2
        IF (TT .GE. 0) THEN
          IY     = IY + 1
          TT     = TT - DX2
          KB     = KB + NXRAST
        END IF
        IF (IY .LT. 0)                  GOTO 110
        IF (IY .GE. NYRAST)             GOTO 110
        IF (IX .LT. 0)                  GOTO 110
        IF (IX .GE. NXRAST)             GOTO 110
        IW     = KB / NBW
        IB     = KB - IW*NBW + 1
        IF(IAND(IRAST(JRAST+IW+1),MASK(IB)) .NE. 0)     GOTO 110
        IF (IVIS .GT. 0)                GOTO 120
        IVIS   = 1
        NT     = NT + 1
*       T(1,NT)= TCUR - DDTT
        T(1,NT)= TCUR
        GOTO 120
  110   IF (IVIS .EQ. 0)                GOTO 120
        IVIS   = 0
*       T(2,NT)= TCUR + DDTT
        T(2,NT)= TCUR
        IF (NT .EQ. NTMAX)              GOTO 300
  120 CONTINUE
      IF (IVIS .GT. 0)                  T(2,NT)= TCUR + DT + DDTT
      GOTO 300
*
**          D Y   . G T .   D X
*
  200 DT     = 1. / (DY+1)
      DDTT   = 0.5*DT
      TCUR   =-DT
      TT     =-(DY + DX2)
      IX     = X1
      IF (Y2 .GE. NYRAST)               Y2 = NYRAST - 1
      KB     = (Y1-1)*NXRAST + IX
      DO 220 IY=Y1,Y2
        KB     = KB + NXRAST
        TCUR   = TCUR + DT
        TT     = TT + DX2
        IF (TT .GE. 0) THEN
          IX     = IX + INCRX
          TT     = TT - DY2
          KB     = KB + INCRX
        END IF
        IF (IY .LT. 0)                  GOTO 210
        IF (IX .LT. 0)                  GOTO 210
        IF (IX .GE. NXRAST)             GOTO 210
        IW     = KB / NBW
        IB     = KB - IW*NBW + 1
        IF (IAND(IRAST(JRAST+IW+1),MASK(IB)) .NE. 0)     GOTO 210
        IF (IVIS .GT. 0)                GOTO 220
        IVIS   = 1
        NT     = NT + 1
*       T(1,NT)= TCUR - DDTT
        T(1,NT)= TCUR
        GOTO 220
  210   IF (IVIS .EQ. 0)                GOTO 220
        IVIS   = 0
*       T(2,NT)= TCUR + DDTT
        T(2,NT)= TCUR
        IF (NT .EQ. NTMAX)              GOTO 300
  220 CONTINUE
      IF (IVIS .GT. 0)                  T(2,NT)= TCUR + DT
      GOTO 300
*
**          C H E C K   D I R E C T I O N   O F   P A R A M E T E R
*
  300 IF (NT .EQ. 0)                    GOTO 999
      DT     = DT*1.1
      IF (T(1,1) .LE. DT)               T(1,1)  = 0.
      IF (T(2,NT) .GE. 1.-DT)           T(2,NT) = 1.
      IF (IFINVE .EQ. 0)                GOTO 999
      DO 310 I=1,NT
        T1     = T(1,I)
        T2     = T(2,I)
        T(1,I) = 1. - T2
        T(2,I) = 1. - T1
  310 CONTINUE
      GOTO 999
*
  999 RETURN
      END
