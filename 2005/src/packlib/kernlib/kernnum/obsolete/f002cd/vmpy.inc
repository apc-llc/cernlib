*
* $Id: vmpy.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: vmpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* vmpy.inc
*
*CALL     BASIC
*CALL     MPY
 MAIN     MACRO     TYPE,N,JX,JY,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VMPY*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,Y,Y2)
          PROLOG    [2],N,JX,JY,J       X0=(X), X1=(Y)
          "P"_VMPY  "PLUS",N,X0,JX,X1,JY,"NULL",B0,J
 [1]      EPILOG
 [2]      MX6       0                   TVMPY=0.
          IFEQ      LENGTH,2,1
          MX7       0
          JP        [1]
          ENDM
 PROLOG   MACRO     EX,NN,JX,JZ,ONE
*                                       X0=(X)
*                                       X1=(Y)
*                                       X5=N
          BSS       0                   X1=(N)    A1=((N))
 +        S_ONE     1         0                             ONE=1
          SA5       X1        1         X5=N      A5=(N)
          SA3       A1+ONE    2         X3=(X)    A3=((X))
 +        SA1       A1+3      3         X1=(Z)    A1=((Z))
                              4 A1
          SA2       A1+ONE    5         X2=(Z2)   A2=((Z2))
          SA4       A3+ONE    6         X4=(X2)   A4=((X2))
                              7
                              8 X5
 +        S_NN      X5        9 X3                          NN=N
          BX0       X3       10 X1      X0=(X)
                             11 X0
          LE        NN,B0,EX 12 X2      IF N.LE.0 EXIT WITH A0 PRESERVED
 +        IX6       X2-X1    13 X4      X6=(Z2)-(Z)
          IX7       X4-X3    14 X6      X7=(X2)-(X)
          S_JZ      X6       15 X7                          JZ=(Z2)-(Z)
          S_JX      X7       16                             JX=(X2)-(X)
 PROLOG   ENDM
