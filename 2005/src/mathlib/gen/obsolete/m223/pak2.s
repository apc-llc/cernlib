*
* $Id: pak2.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: pak2.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT       PAK2
#include "entftn.inc"
*
* REGISTERS WITH FIXED DEFINITON INSIDE THE LOOP ARE
*        B1 = EXPONENT SHIFT COUNT
*        B2 = STORE ADDRESS
*        B3 = GET COUNT
*        B5 = MANTISSA SHIFT COUNT
*        B7 = 1
*        A0 = BIAS
*        A1 = GET WORD ADDRESS
*        A7 = STORE WORD ADDRESS
*        X2 = UPPER LIMIT OF FLOATING NUMBER
*        X4 = MANTISSA MASK
*        X5 = EXPONENT MASK
*        X6 = LOWER LIMIT OF FLOATING NUMBER
*        X7 = WORD TO BE STORED
*
          ENTRY       PAK2
* SUBROUTINE PAK2(A,B,N,M)
 ZERO     DATA        00004000000000000000B
          DATA        10004000000000000000B
          DATA        13204000000000000000B
          DATA        15204000000000000000B
          DATA        16204000000000000000B
          DATA        16604000000000000000B
          DATA        17004000000000000000B
 MAXMIN   DATA        37207777777777777777B
          DATA        27207777777777777777B
          DATA        23207777777777777777B
          DATA        21207777777777777777B
          DATA        20207777777777777777B
          DATA        17577777777777777777B
          DATA        17377777777777777777B
*
          ENTFTN      PAK2,4
          SA4         B4               LOAD MANTISSA BITS
          SA3         B3               LOAD GET COUNT
          SX5         X4-25
          PL          X5,PAK2          TOO MANY MANTISSA BITS ASKED FOR
          SX5         17
          IX5         X5-X4
          PL          X5,PAK2          TOO FEW MANTISSA BITS ASKED FOR
          MX2         1
          SX5         29
          SB6         X4-18
          SB7         1
          IX5         X5-X4
          SX7         B0
          SB5         X5               EXPONENT BITS
          SB3         X3+B1            GET COUNT
          SB4         X4-1             MANTISSA BITS
          LX0         B5,X2            BIAS
          SA1         B1-B7            LOAD DUMMY A(0)
          AX5         B5,X2            EXPONENT MASK LEFT ADJUSTED
          SB1         60
          SB5         B5+B7
          AX4         B4,X2            MANTISSA MASK
          SA2         MAXMIN+B6        LOAD UPPER LIMIT
          LX5         B5,X5            EXPONENT MASK RIGHT ADJUSTED
          LX4         48               MANTISSA MASK
          SA0         X0+47
          SB1         B1-B5            EXPONENT SHIFT COUNT
          BX6         X2
          SB5         B4-17            SHIFT COUNT
          SB4         B6
          SA2         ZERO+B6          LOAD $ZERO$ LIMIT
*
 LOOP     SB6         A1+B7            AUGMENT LOAD COUNT
          GE          B6,B3,FINIS
          SA1         B6               LOAD NEXT WORD
*
          FX0         X6-X1
          PL          X0,LTEST
          BX1         X6               NUMBER REPLACED BY UPPER LIMIT
          EQ          B0,B0,INRANGE
 LTEST    FX0         X1+X6
          PL          X0,ZTEST
          BX1         -X6              NUMBER REPLACED BY LOWER LIMIT
          EQ          B0,B0,INRANGE
 ZTEST    PL          X1,PZTEST
          FX0         X1+X2
          NG          X0,INRANGE
          BX1         -X2              NUMBER REPLACED BY $NEG. ZERO$
          EQ          B0,B0,INRANGE
 PZTEST   FX0         X1-X2
          PL          X0,INRANGE
          BX1         X2               NUMBER REPLACED BY $POS. ZERO$
 INRANGE  MX3         1
          BX0         X7               SAVE FIRST PACKED WORD
          BX7         X1*X3            GET SIGN
          ZR          X7,NOCOMP
          BX1         -X1
 NOCOMP   UX3         B6,X1
          SX1         A0+B6            ADD BIAS TO EXPONENT
          BX1         X5*X1            MASK NEW EXPONENT
          LX1         B1,X1            SHIFT NEW EXPONENT
          BX7         X1+X7            ADD NEW EXPONENT
          BX1         X3*X4            GET MANTISSA
          LX1         B5,X1            SHIFT MANTISSA
          BX7         X1+X7            ADD MANTISSA
          ZR          X0,LOOP
          LX7         30
          BX7         X0+X7            ADD THE TWO PACKED WORDS
          SA7         B2               STORE PACKED WORD
          SX7         B0
          SB2         B2+B7
          EQ          B0,B0,LOOP
 FINIS    ZR          X7,PAK2
          SA7         B2               STORE LAST WORD
          EQ          B0,B0,PAK2
          END
