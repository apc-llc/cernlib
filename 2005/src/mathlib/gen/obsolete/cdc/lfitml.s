*
* $Id: lfitml.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: lfitml.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
          IDENT LFIT
ONE       DATA 1.0
COUNT     BSS  1
AZERO     BSS  1
          ENTRY LFIT         .SEE FORTRAN VERSION OF MBM OR
*     SUBROUTINE LFIT(X,Y,L,KEY,A,B,E)
+VFD36/4HLFIT,24/6
LFIT      DATA 0             .MENZEL ,FORMULAS OF PHYSICS, P. 116
          SB2  X1
          SA2  A1+1
          SB1  X2
          SA3  A2+1
          SB3  X3
          SX7  A0
          SA7  AZERO
          SA3  A3+1
          SA3  X3
          SA0  X3
          SA4  A2+3
          SB4  X4
          SA5  A4+1
          SB5  X5
          SA5  A5+1
          SB6  X5
          SB7  2
          SA3  B3
          SB3  X3
          LT   B3,B7,L25     .IF(L-2)25,1,1
          SA5  ONE
          SB7  B0            .J=0
          SX6  B0            .COUNT=0.0
          SX3  B0            .SUMY=0
          SX4  B0            .SUMX=0
LOOP1     SA1  B7+B1
          SX2  A0
          NZ   X2,L1
          ZR   X1,L10        .IF(IY(J)) 5,10,5
L1        SA2  B2+B7
          FX3  X3+X1         .SUMY=SUMY+FIPS(J)
          FX6  X6+X5         .COUNT=COUNT+1
          NX3  B0,X3
          FX4  X4+X2         .SUMX=SUMX+FICS(J)
          NX6  B0,X6
          NX4  B0,X4
L10       SB7  B7+1
          LT   B7,B3,LOOP1
          ZR   X6,L25        .IF(COUNT.EQ.0) GO TO 25
          RX3  X3/X6         .YMED=SUMY/COUNT
          SB7  B0
          SX0  B0            .SUMXY=0
          SX7  B0            .SUMYY=0
          RX4  X4/X6         .XMD=SUMX /COUNT
          FX6  X6-X5
          NX6  B0,X6
          FX6  X6-X5         .COUNT=COUNT-2.0
          NX6  B0,X6
          SX5  B0            .SUMXX=0
          SA6  COUNT
LOOP2     SA1  B1+B7
          SX2  A0
          NZ   X2,L2
          ZR   X1,L20        .IF(IY(J)) 15,20,15
L2        SA2  B2+B7
          FX1  X1-X3         .SCARTY=FIPS(J)-YMED
          NX1  B0,X1
          FX2  X2-X4         .SCARTX=FICS(J)-XMED
          NX2  B0,X2
          FX6  X1*X2
          FX0  X6+X0         .SUMXY=SUMXY+SCARTX*SCARTY
          NX0  B0,X0
          FX2  X2*X2
          FX1  X1*X1
          FX5  X5+X2         .SUMXX=SUMXX+SCARTX*SCARTX
          NX5  B0,X5
          FX7  X7+X1         .SUMYY=SUMYY+SCARTY*SCARTY
          NX7  B0,X7
L20       SB7  B7+1
          LT   B7,B3,LOOP2
          ZR   X5,L25        .IF(SUMXX.EQ.0.) GO TO 25
          RX6  X0/X5
          SA6  B4            .A=SUMXY/SUMXX
          BX1  X6
          FX6  X6*X4
          FX6  X3-X6
          NX6  B0,X6
          SA6  B5            .B=YMED-A*XMED
          FX6  X0*X1
          SA5  COUNT
          FX6  X7-X6
          NX6  B0,X6
          SX7  B0
          ZR   X5,L101       .IF(COUNT.LE.2)GO TO 101
          RX6  X6/X5
          SA6  B6            .E=(SUMYY-SUMXY*A)/(COUNT-2.)
          JP   REST
L25       SX7  B0
          SA7  B4            .A=0.0
          SA7  B5            .B=0.0
L101      SA7  B6            .E=0.0
REST      SA5  AZERO
          SA0  X5
          JP   LFIT
          END
#endif
