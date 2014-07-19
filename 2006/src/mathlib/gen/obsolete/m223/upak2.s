*
* $Id: upak2.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: upak2.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT       UPAK2
#include "entftn.inc"
          ENTRY       UPAK2
* SUBROUTINE UPAK2(B,C,N,M)
         ENTFTN       UPAK2,4
          SA4         B4               LOAD NB OF MANTISSA BITS
          SB7         1
          SX5         X4-25
          PL          X5,UPAK2         TOO MANY MANTISSA BITS
          SX5         17
          IX5         X5-X4
          PL          X5,UPAK2         TOO FEW MANTISSA BITS
          SA3         B3               LOAD STORE COUNT
          MX2         1                SIGN MASK
          SB5         29
          SB4         X4-1             NUMBER OF MANTISSA BITS
          SB5         B5-B4            NB OF EXPONENT BITS
          AX4         B4,X2
          SB4         B4+31            MANTISSA SHIFT COUNT
          SX7         B0
          LX4         B4,X4            MANTISSA MASK
          SB6         108
          SB4         B6-B4
          SB5         B5-2
          AX5         B5,X2
          LX5         59               EXPONENT MASK
          SB3         X3+B2            HIGHEST STORE ADDRESS
          SA1         B1-B7            LOAD DUMMY A(-1)
          SB5         B5+B7
          LX3         B5,X2            BIAS RIGHT ADJUSTED
          SB5         B5+B7
          SX3         X3+47
          SA0         X3
*
 LOOP     SA1         A1+B7            LOAD NEXT WORD TO BE UNPACKED
          SB1         B7+B7
 SEC      BX3         X5*X1            GET EXPONENT
          LX3         B5,X3
          SX6         A0
          IX3         X3-X6            SUBTRACT BIAS
          BX6         X1*X4            GET MANTISSA
          SB6         X3
          LX6         B4,X6
          BX3         X2*X1            GET SIGN
          PX6         B6,X6
          ZR          X3,NOCOMP
          IX6         X7-X6            COMPLEMENT
 NOCOMP   SA6         B2               STORE UNPACKED WORD
          SB1         B1-B7
          SB2         B2+B7            AUGMENT STORE COUNT
          GE          B2,B3,UPAK2
          EQ          B1,B0,LOOP
          LX1         30
          EQ          B0,B0,SEC
          END
