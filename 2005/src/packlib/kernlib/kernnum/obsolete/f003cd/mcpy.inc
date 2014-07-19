*
* $Id: mcpy.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mcpy.inc
*
*CALL     BASIC
*CALL     CPY
 MAIN     MACRO     TYPE,M,N,IX,JX,IZ,JZ,J
          SETYPE    TYPE                SET PREFIX, SUFFIX, LENGTH
 NAME     MICRO     1,,*"P"_MCPY_"S"*
          HEADER    "NAME",SAVEA0
          EXIT      "NAME"              (M,N,X,X12,X21,Z,Z12,Z21)
          PROLOG    M,N,IX,JX,IZ,JZ,J
 TAU      IFEQ      TYPE,CTYPE
          DMCPY     M,N,X0,IX,JX,A0,IZ,JZ,X1,J
 TAU      ELSE
          "P"MCPY   M,N,X0,IX,JX,A0,IZ,JZ,X1,J
 TAU      ENDIF
          EPILOG
          ENDM
 PROLOG   MACRO     MM,NN,IX,JX,IZ,JZ,ONE
*                                       X0=(X)    A0=(Z)
          BSS       0                   X1=(M)    A1=((M))
          SA4       X1        0         X4=M      A4=(M)
          S_ONE     1         1                             ONE=1
                              2
          SA5       A1+ONE    3         X5=(N)    A5=((N))
 +        SA1       A1+5      4         X1=(Z)    A1=((Z))
          SA3       A5+ONE    5         X3=(X)    A3=((X))
          SA2       A1+ONE    6         X2=(Z12)  A2=((Z12))
                              7 X4
 +        S_MM      X4        8                             MM=M
          SA4       A2+ONE    9         X4=(Z21)  A4=((Z21))
                             10 X5
          LE        MM,B0,EXIT  X1      IF M.LE.0 EXIT WITH A0 PRESERVED
 +        SA5       X5       12 X3      X5=N      A5=(N)
          SX6       A0       13 X2      X6=OLD A0
          BX0       X3       14 X6      X0=(X)
          SA3       A3+ONE   15 X0      X3=(X12)  A3=((X12))
 +        IX7       X2-X1    16 X4      X7=(Z12)-(Z)
          SA2       A3+ONE   17 X7      X2=(X21)  A2=((X21))
          SA6       SAVEA0   18
                             19 X5
 +        IX6       X4-X1    20         X6=(Z21)-(Z)
          S_JZ      X7       21 X6                          JZ=(Z12)-(Z)
          S_NN      X5       22 X3
          S_IZ      X6       23                             IZ=(Z21)-(Z)
                             24 X2
 +        LE        NN,B0,EXIT          IF N.LE.0 EXIT WITH A0 PRESERVED
          IX7       X3-X0    26         X7=(X12)-(X)
          IX6       X2-X0    27 X7      X6=(X21)-(X)
 +        S_JX      X7       28 X6                          JX=(X12)-(X)
          S_IX      X6       29                             IX=(X21)-(X)
          SA0       X1       30                   A0=(Z)
 PROLOG   ENDM
