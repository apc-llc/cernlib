*
* $Id: pak3.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: pak3.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT PAK3
#include "entftn.inc"
          ENTRY       PAK3
* SUBROUTINE PAK3(A,X,N)
 +        VFD         24/0HPAK3,36/3
          ENTFTN  PAK3,3
          SB7         1
          SA1         B1
          MX0         47
          SA3         B3
          MX2         1
          BX0         -X0
          SB3         X3
          LX2         20
          BX0         X2+X0
          SB6         B0+48
          MX2         54
 NLOOP    SB5         3
          SX6         B0
 HUP      ZR          X1,ZERO
          BX3         X1
          SA4         PU
          NG          X1,NEG
          IX7         X1-X4
          PL          X7,REPLP
 ZTEST    SA4         PL
          IX7         X1-X4
          NG          X7,ZERO
 OKAY     UX3         B4,X3
          SX4         B4+B6
          AX3         35
          BX4         -X2*X4
          LX6         20
          BX3         X0*X3
          LX4         13
          SB5         B5-B7
          SA1         A1+B7
          BX4         X3+X4
          SB3         B3-B7
          BX6         X6+X4
 ENDT     ZR          B3,STORE
          LT          B0,B5,HUP
          SA6         B2
          SB2         B2+B7
          EQ          B0,B0,NLOOP
 STORE    EQ          B0,B5,SA6
          LX6         20
          SB5         B5-B7
          EQ          B0,B0,STORE
 SA6      SA6         B2
          EQ          B0,B0,PAK3
 NEG      BX1         -X1
          IX7         X1-X4
          PL          X7,REPLN
          EQ          B0,B0,ZTEST
 ZERO     MX3         0
          SA1         A1+B7
          SB5         B5-B7
          LX6         20
          SB3         B3-B7
          EQ          B0,B0,ENDT
 REPLP    SA3         PU
          EQ          B0,B0,OKAY
 REPLN    SA3         NU
          EQ          B0,B0,OKAY
 NU       DATA    60213777777777777777B
 PU       DATA    17567777777777777777B
 PL       DATA    16604000000000000000B
          END
