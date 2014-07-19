*
* $Id: fto360.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: fto360.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT  FTO360
*
* CERN PROGLIB# M430    FTO360          .VERSION KERNCDC  2.01  820727
*
          ENTRY  FTO360
* FTO360 - COMPASS VERSION - SEPTEMBER 1976 - GENEVIEVE ROUX
*     SUBROUTINE FTO360(IA,NN,MD)
TRACE     VFD    36/6HFTO360,24/3
FTO360    EQ     FTO360+400000B
          SB1    X1                B1=ADDRESS OF A
          SA1    A1+1              X1=ADDRESS OF N
          SB6    B1          .INITIALIZE FOR REAL
          SA2    X1
          SB2    X2+B1             B2=ADDRESS OF A(N+1)
          SA1    A1+1              ADDRESS OF THIRD PARAMETER
          ZR     X1,REAL
          SA1    X1                X1=MODE
          ZR     X1,REAL           MODE=0 - REAL
          SX2    X1-1
          ZR     X2,INT            MODE=1 - INTEGER
          SA2    HF
          IX2    X2-X1
          ZR     X2,REAL           MODE=1HF - REAL
          SA2    HFLOAT
          IX2    X2-X1
          ZR     X2,REAL           MODE=5HFLOAT - REAL
          SA2    HI
          IX2    X2-X1
          ZR     X2,INT            MODE=1HI - INTEGER
          SA2    HINT
          IX2    X2-X1
          ZR     X2,INT            MODE=3HINT - INTEGER
          EQ     FTO360
INT       BSS    0
          SA2    B1                PICKS UP NUMBER IN X2
          SB6    B0                SET INTEGER INDCATOR
          SB7    0                 POSITIVE NUMBER
          PL     X2,PLUS
          BX2    -X2
          SB7    1
PLUS      MX3    31
          ZR     X2,LOAD
          LX3    31
          SX7    B7
          IX3    X3+X7
          IX4    X3-X2
          PL     X4,OK
          BX2    X3
OK        ZR     B7,LOAD
          MX3    32
          LX3    32
          MX0    73B
          IX2    X2+X0             X2=X2-1
          BX2    -X2
          BX2    X2*X3
          EQ     LOAD
HF        DATA   1HF
HFLOAT    DATA   5HFLOAT
HI        DATA   1HI
HINT      DATA   3HINT
I4Z       DATA   7777777777777777B
I2T23     DATA   40000000B
I4Z8S     DATA   7777777700000000B
MXNO      DATA   23147777777700000000B
MNNO      DATA   13144000000000000000B
IXNOPL    DATA   17777777777B
IXNONG    DATA   37777777777B
REAL      BSS    0
          SA2    B1                X2=A(I)
          SB7    B0                POSITIVE NUMBER
          PL     X2,POS
          SB7    1                 NEGATIVE NUMBER
          BX2    -X2               X2=-A(I)
POS       BSS    0
          ZR     X2,LOAD
          SA3    MXNO              X3=MXNO
          MX0    0
          IX3    X2-X3             A(I)-MXNO
          IX6    X0+X3
          MI     X3,LTMXNO         A(I) .LT. MNO
          EQ     B7,B0,MAXPL
          SA2    IXNONG            A(I)=37777777777B
          EQ     LOAD
MAXPL     BSS    0
          SA2    IXNOPL
          EQ     LOAD
LTMXNO    BSS    0
          SA3    MNNO              X3=MNNO
          IX3    X3-X2             MNNO-A(I)
          IX6    X0+X3
          MI     X6,INRANGE
          MX2    0                 A(I)=0
          EQ     LOAD
INRANGE   BSS    0
          MX4    48
          LX4    48
          BX0    X2                X0=A(I)
          BX7    X2*X4             MANTISSA=X7=A(I) .A. MASK(48)
          SX3    1720B
          AX0    60B               SHIFT(A(I),48)
          IX6    X0-X3             EXPONENT=X6=SHIFT(A(I),48)-976
          SX0    57B
          IX5    X0-X6             X5=47-EXP
          MX0    73B               X0=-1
          PL     X5,LT47
          IX6    X6+X0             EXP=EXP-1
LT47      BSS    0
          MX5    72B               X5=MASK(58) OR -3B
          BX4    X6
          BX3    X6
          MX2    0
          AX4    73B
          BX1    X4-X3
          BX3    -X5*X1
          BX1    X3-X4
          IX4    X2+X1            J=X4=MOD(EXP,4)
          AX6    2B                SHIFT(EXP,-2)
          NG     X4,A44
          IX6    X6-X0             EXP=EXP+1
          IX4    X5+X4             J=J-3
          EQ     JLT0
A44       IX4    X4-X0       J=J-1
JLT0      BSS    0
          SB4    X4
          LX7    B4,X7             MAN=SHIFT(MAN,NEW J)
          SA4    I2T23
          IX4    X4+X7             J=MAN+I2T23
          SX0    100B
          IX6    X6+X0             NEX=NEX+64D
          MX5    12
          BX3    X4*X5             J .A. MASK(12)
          SB4    -30B              B4=-24
          ZR     X3,A59
          SX0    177B
          IX3    X6-X0             EXP-127
          NZ     X3,A58
          SA5    I4Z8S
          BX7    X7*X5
          AX7    30B
          EQ     A60
A58       BSS    0
          SB4    -34B
          MX0    73B               X0=-1
          IX6    X6-X0             NEX=NEX+1
A59       BSS    0
          LX7    B4,X4             MAN=SHIFT(J,KL)
          MX3    24
          LX3    24
          BX7    X7*X3             MAN=MAN .A. 7777777B
A60       BSS    0
          LX6    30B               EXP=SHIFT(EXP,24)
          BX2    X6+X7             A(I)=EXP .OR. MAN
          SX3    B7
          LX3    37B               SHIFT OF SIGN BIT
          BX2    X2+X3             A(I)=A(I) .OR. SIGN BIT
LOAD      BX6    X2
          SA6    B1                    STORE NEW NUMBER
NEWNO     BSS    0
          SB1    B1+1
          EQ     B2,B1,FTO360
          EQ     B6,B0,INT
          EQ     B0,B0,REAL
          END
          IDENT BUNCH32
          ENTRY BUNCH32
* SUBROUTINE BUNCH32(ARRAY,N)
          VFD      42/7HBUNCH32,18/BUNCH32
SA        BSSZ     1
BUN       SA1      SA
          SA0      X1
BUNCH32   DATA     0
          SX6      A0
          SB1      X1
          SA6      SA
          SA1      A1+1
          SB2      X1
          SA0   B0
          MX7   28
          SB7   1
          SB6   B0
          MX6   0
          SA2   B2
          SB5   -32
          MX3   1
          SB4   28
          BX3   -X3
          SB2   X2
          EQ    IP
 OP       SB4   B4+92
          SA6   A0+B1
          SB3   B4-B7
          MX6   0
          SA0   A0+B7
          AX4   B3,X3
          BX1   X1*X4
          EQ    SH
 IP       SA1   B1+B6
          EQ    B6,B2,ED
          SB6   B6+B7
          BX1   -X7*X1
 SH       LX5   B4,X1
          SB4   B4+B5
          BX6   X5+X6
          GT    B4,B5,IP
          LT    B4,B5,OP
          SB4   28
          SA6   A0+B1
          SA0   A0+B7
          MX6   0
          EQ    IP
 ED       SB4   B4-28
          EQ       B0,B4,BUN
          SA6   A0+B1
          EQ       BUN
          END
