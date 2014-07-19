*
* $Id: mmpy.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mmpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mmpy.inc
*
*CALL     BASIC
*CALL     SCL
*CALL     SCA
*CALL     MPY
 MMNA     SET       1
 MMNS     SET       2
 MMPA     SET       3
 MMPS     SET       4
 MMPY     SET       5
 MAIN     MACRO     TYPE,FCN,M,N,IX,JX,JY,IZ,J
          SETYPE    TYPE
          IFEQ      FCN,MMNA,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*MMNA*
          IFEQ      FCN,MMNS,3
 SIGNX    MICRO     1,,*-*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*MMNS*
          IFEQ      FCN,MMPA,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*+*
 VERB     MICRO     1,,*MMPA*
          IFEQ      FCN,MMPS,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*-*
 VERB     MICRO     1,,*MMPS*
          IFEQ      FCN,MMPY,3
 SIGNX    MICRO     1,,*+*
 SIGNZ    MICRO     1,,*0*
 VERB     MICRO     1,,*MMPY*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME",SAVEA0
 TEMP2    BSS       2                   WORKING STORE
          EXIT      "NAME"              (M,N,X,X12,X21,Y,Y2,Z,Z2)
          PROLOG    M,N,IX,JX,JY,IZ,J
*                                       X1=(X), X2=(Y), A0=(Z)
          "P"MMPY   "SIGNX",M,N,IX,JX,JY,"SIGNZ",A0,IZ,TEMP2,J
          EPILOG
          ENDM
 PROLOG   MACRO     MM,NN,IX,JX,JY,IZ,ONE X1=(X),X2=(Y),A0=(Z)
          BSS       0                   X1=(M)    A1=((M))
 +        S_ONE     1         0                             ONE=1
          SA2       X1        1         X2=M      A2=(M)
          SA3       A1+ONE    2         X3=(N)    A3=((N))
 +        SX6       A0        3         X6=OLD A0
          SA4       A3+ONE    4 X6      X4=(X)    A4=((X))
          SA6       SAVEA0    5         SAVEA0=OLD A0
*                             6
*                             7
 +        SA5       A4+ONE    8 X2      X5=(X12)  A5=((X12))
          S_MM      X2        9 X3                          MM=M
          SA1       A5+ONE   10         X1=(X21)  A1=((X21))
          SA3       X3       11 X4      X3=N      A3=(N)
 +        SA2       A1+ONE   12         X2=(Y)    A2=((Y))
          BX0       X4       13         X0=(X)
*                            14 X0
          LE        MM,B0,EXIT  X5      IF M.LE.0 GO TO EXIT
 +        SA4       A2+ONE   16         X4=(Y2)   A4=((Y2))
*                            17 X1
*                            18 X3
          IX6       X5-X0    19 X2      X6=(X12)-(X)=JX
          SA5       A4+ONE   20 X6      X5=(Z)    A5=((Z))
          IX7       X1-X0    21         X7=(X21)-(X)=IX
 +        SA1       A5+ONE   22 X7      X1=(Z2)   A1=((Z2))
          S_JX      X6       23 X4                          JX=(X12)-(X)
          S_NN      X3       24                             NN=N
          IX6       X4-X2    25         X6=(Y2)-(Y)=JY
 +        S_IX      X7       26 X6                          IX=(X21)-(X)
*                            27 X5
          LE        NN,B0,EXIT          IF N.LE.0 GO TO EXIT
          S_JY      X6       29 X1                          JY=(Y2)-(Y)
 +        IX7       X1-X5    30         X7=(Z2)-(Z)=IZ
          BX1       X0       31 X7      X1=(X)
          S_IZ      X7       32 X1                          IZ=(Z2)-(Z)
          SA0       X5       33                   A0=(Z)
 PROLOG   ENDM
