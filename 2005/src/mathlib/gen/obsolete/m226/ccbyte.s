*
* $Id: ccbyte.s,v 1.1.1.1 1996/04/01 15:03:20 mclareni Exp $
*
* $Log: ccbyte.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:20  mclareni
* Mathlib gen
*
*
          IDENT       CCBYTE
          ENTRY       CCBYTE
* SUBROUTINE CCBYTE(ARRAY,NCHAR,MODE)
#include "entftn.inc"
*     B2 = NCH
*     B3 = 4 IF USASCII,       0 IF EBCDIC
*     B4 = NP  POSITION INSIDE 2-WORD STORE COUNT
*     B6 = STORE COUNT
*     B7 = 1
*     B1 = ADDRESS OF A(1)
*     X0 = 6-BIT MASK
*     X1 = WORD TO CONVERT
*     X2 = PHASE SHIFT CONSTANTS
*     X6 = CONVERTED CHARACTER
*     X7 = CONVERTED CHARACTERS
 EBCDIC   DATA        00003335154134401600B
          DATA        00007237125513530076B
          DATA        00005614747357325475B
          DATA        00000000000036771720B
          DATA        40404040404040404040B          USASCII
          DATA        40404040404040404040B
          DATA        40404040404040404040B
          DATA        40406463626160404040B
          DATA        40405255537170676665B
          DATA        40405642407544515057B
          DATA        40407740454772404043B
          DATA        40407340404676744041B
 EBC      DATA        6HEBCDIC
*
          ENTFTN      CCBYTE,3
          SA3         B3
          SA4         EBC
          IX4         X4-X3
          SB3         B0
          ZR          X4,HOPP
          SB3         4
*     N2F=NCH/15
 HOPP     SA1         B2
          SX2         15
          SB7         1
          PX0         B0,X2
          SB2         X1
          NX0         B0,X0
          BX4         X1
          PX1         B0,X1
          NX1         B0,X1
          FX0         X1/X0
          UX7         B4,X0
          LX5         B4,X7
*     M=MOD(NCH,10)
          SX3         10
          PX6         B0,X3
          NX7         B0,X6
          FX7         X1/X7
          UX7         B4,X7
          LX7         B4,X7
          PX7         B0,X7
          DX7         X7*X6
          UX6         B0,X7
          IX4         X4-X6
          SB5         X4-1
          PL          B5,JUMP3
          SB5         9
*     L=MIN(M,1)
 JUMP3    ZR          X4,JUMP1
          SX4         B7
*     NG=NCH/10
 JUMP1    SX3         10
          PX0         B0,X3
          NX0         B0,X0
          FX0         X1/X0
          UX7         B4,X0
          LX6         B4,X7
*     NG=NG+L
          IX7         X6+X4
          SB6         X7-1
*     NST=(2* NG-1)/15
          FX2         X1+X1
          SA3         =1.
          FX2         X2-X3
          SX3         15
          PX0         B0,X3
          NX0         B0,X0
          FX0         X2/X0
          UX7         B4,X0
          LX0         B4,X7
*     NP=NCH-15*N2F-1
          BX7         X5
          LX7         4
          SX3         B7
          IX6         X7-X5
          BX6         -X6
          SX6         X6+B2
          IX6         X6-X3
          PL          X6,JUMP5
          SX6         14
*
 JUMP5    SX7         B0
          SX1         B5
          SB2         X1
          LX1         1
          SX1         X1+B5
          LX1         1
          SB5         X1
          SA1         B1+B6
          LX1         B5,X1
          SB6         X0
          SB5         X6-7
          SB4         X6
          MX0         6
          SA2         =30030731732510000000B
          EQ          B5,JUMP2
          SX4         B4
          SX5         52
          NG          B5,SHIFT
          SX4         B5
          SX5         56
 SHIFT    LX4         3
          IX4         X5-X4
          SB4         X4               SHIFT COUNT
          EQ          B0,B0,MASK
 JUMP2    SB4         56
*
 MASK     BX6         X0*X1            MASK 6-BIT-BYTE
          LX6         6
          NZ          B3,ASCI
          SB5         B0
          SA0         EBCDIC
          SX5         X6-12B
          NG          X5,CHAR
          SB5         B5+9
          SX5         X6-23B
          NG          X5,CHAR
          SB5         B5+9
          SX5         X6-33B
          NG          X5,CHAR
          SB5         B5+9
          SX5         X6-45B
          NG          X5,CHAR
          SX6         X6-44B
 DCH      MX3         3
          LX3         6
          BX5         X3*X6            MASK ROW POINTER
          AX5         3
          SB5         X5
          AX3         3
          SA4         A0+B5            LOAD TABLE WORD
          BX3         X3*X6            MASK COLUMN POINTER
          LX5         B7,X3            2*COL PTER
          IX5         X5+X3            3*COL PTER
          LX5         1                6*COL PTER
          MX3         6
          SB5         X5
          LX3         6
          AX4         B5,X4            SHIFT TABLE WORD
          BX6         X4*X3            MASK LAST 6 BITS OF 8-BIT-BYTE
          NZ          B3,JUMP4
          SX6         X6+100B
 JUMP4    SB5         B4-56
          EQ          B5,B0,SPLIT
 ADD      LX6         B4,X6
          SB5         52
          LX1         54
          BX7         X6+X7
          SB5         B5-B4
          ZR          B5,STORE
          SB2         B2-B7
          SB4         B4+8
          PL          B2,MASK
*
 GET      SB2         9
          SB5         A1-B1
          NG          B5,CCBYTE
          SA1         A1-B7
          LX1         54
          EQ          B0,B0,MASK
*
 CHAR     LX2         B5,X2
          SB5         B0-B5
          MX5         9
          SB5         B5+60
          BX5         X2*X5
          LX2         B5,X2
          LX5         9
          SB5         B4-56
          IX6         X5+X6
          EQ          B5,B0,SPLIT      CHECK ON SPLIT-THE CHAR.-CONDITIO
          EQ          B0,B0,ADD
*
 ASCI     SB5         36
          SX3         X6-33B
          SA0         EBCDIC+B3
          NG          X3,CHAR
          EQ          B0,B0,DCH
*
 SPLIT    LX6         B4,X6
          MX5         4
          BX3         X5*X6            MASK THE LAST 4 BITS OF 8 BIT BYT
          BX7         X3+X7            ADD THE LAST 4 BITS OF 8 BIT BYTE
          LX5         4
          BX3         X5*X6            GET FIRST 4BITS OF 8BIT BYTE
          SA7         B6+B1            STORE
          SB6         B6-B7            REDUCE STORE COUNT
          SB4         4
          BX7         X3               TRANSFER FIRST 4 BITS AFTER SPLIT
          SB2         B2-B7
          LX1         54
          EQ          B0,B0,MASK
*
 STORE    SA7         B6+B1            STORE
          SB6         B6-B7            REDUCE STORE COUNT
          NG          B6,CCBYTE        LAST WORD STORED
          SB4         B0               RESET SHIFT COUNT
          SB2         B2-B7
          SX7         B0
          LT          B2,B0,GET
          EQ          B0,B0,MASK
          END
