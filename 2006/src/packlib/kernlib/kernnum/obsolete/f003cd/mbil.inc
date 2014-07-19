*
* $Id: mbil.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mbil.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mbil.inc
*
*CALL     BASIC
*CALL     MPY
 MAIN     MACRO     TYPE,N,IX,IY,JY,JZ,I,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_MBIL*
          HEADER    "NAME",SAVEA0
 TEMP3    BSS       3
          EXIT      "NAME"              (N,X,X2,Y,Y12,Y21,Z,Z2)
          PROLOG    [2],N,IX,IY,JY,JZ,I   A0=(X), X1=(Y), X2=(Z)
          "P"MBIL   N,IX,IY,JY,JZ,TEMP3,I,J
 [1]      EPILOG
 [2]      MX6       0                   TMBIL = 0.
          IFEQ      LENGTH,2,1
          MX7       0
          JP        [1]
          ENDM
 PROLOG   MACRO     EX,NN,IX,IY,JY,JZ,ONE  A0=(X), X1=(Y), X2=(Z)
          BSS       0                   X1=(N)    A1=((N))
 +        SA3       A1+4      0         X3=(Y12)  A3=((Y12))
          S_ONE     1         1                             ONE=1
 +        SA5       X1        2         X5=N      A5=(N)
          SA1       A3-ONE    3         X1=(Y)    A1=((Y))
          SA4       A3+ONE    4         X4=(Y21)  A4=((Y21))
          SX6       A0        5         X6=OLD A0
*                             6 X6
*                             7 X3
 +        SA2       A4+ONE    8         X2=(Z)    A2=((Z))
          SA6       SAVEA0    9 X5      SAVEA0=OLD A0
          S_NN      X5       10 X1                          NN=N
 +        SA5       A2+ONE   11 X4      X5=(Z2)   A5=((Z2))
          IX7       X4-X1    12         X7=(Y21)-(Y)
          IX6       X3-X1    13 X7      X6=(Y12)-(Y)
          SA4       A1-ONE   14 X6      X4=(X2)   A4=((X2))
 +        S_IY      X7       15 X2                          IY=(Y21)-(Y)
          SA3       A4-ONE   16         X3=(X)    A3=((X))
*                            17
          LE        NN,B0,EX 18 X5      IF N.LE.0 GOTO EXIT
 +        IX7       X5-X2    19         X7=(Z2)-(Z)
          S_JY      X6       20 X7                          JY=(Y12)-(Y)
          S_JZ      X7       21 X4                          JZ=(Z2)-(Z)
          NO                 22
*                            23 X3
 +        IX6       X4-X3    24         X6=(X2)-(X)
          SA0       X3       25 X6                A0=(X)
          S_IX      X6       26                             IX=(X2)-(X)
 PROLOG   ENDM
