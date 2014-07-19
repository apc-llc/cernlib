*
* $Id: upak3.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: upak3.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT UPAK3
#include "entftn.inc"
          ENTRY       UPAK3
* SUBROUTINE UPAK3(A,X,N)
 +        VFD         30/0HUPAK3,30/3
          ENTFTN  UPAK3,3
          SA3         B3
          SB7         1
          MX0         20
          MX2         6
          SB3         X3
          SA1         B2-B7
          LX2         59
 NLOOP    SA1         A1+B7
          SB5         3
 HUP      BX3         X0*X1
          BX4         X2*X1
          ZR          X3,ZERO
          BX3         -X2*X3
          LX4         1
          BX6         X3
          AX6         6
          BX6         X2*X6
          BX3         X3+X6
          AX4         54
          SB6         X4-48
          AX3         5
          PX7         B6,X3
 STORE    SA7         B1
          SB3         B3-B7
          ZR          B3,UPAK3
          LX1         20
          SB5         B5-B7
          SB1         B1+B7
          LT          B0,B5,HUP
          EQ          B0,B0,NLOOP
 ZERO     MX7         0
          EQ          B0,B0,STORE
          END
