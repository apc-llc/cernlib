*
* $Id: vmpa.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: vmpa.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* vmpa.inc
*
*CALL     BASIC
*CALL     MPY
 MAIN     MACRO     TYPE,N,JX,JY,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VMPA*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,Y,Y2,Z)
          PROLOG    [2],N,JX,JY,J       X4=(X),X3=(Y),X5=(Z)
          "P"VMPY   "PLUS",N,X4,JX,X3,JY,"PLUS",X5,J
 [1]      EPILOG
 [2]      BSS       0                   CASE N<1
          SA4       X5                  X4=Z'
          IFEQ      LENGTH,2,1
          SA5       X5+J                X5=Z''
          BX6       X4                  X6=Z'
          IFEQ      LENGTH,2,1
          BX7       X5                  X7=Z''
          JP        [1]
          ENDM
 PROLOG   MACRO     EXIT,NN,JX,JY,ONE    X4=(X), X3=(Y), X5=(Z)
          BSS       0                   X1=(N)    A1=((N))
 +        S_ONE     1         0                             ONE=1
          SA5       X1        1         X5=N      A5=(N)
          SA4       A1+ONE    2         X4=(X)    A4=((X))
*                             3
 +        SA3       A4+2      4         X3=(Y)    A3=((Y))
          SA2       A4+ONE    5         X2=(X2)   A2=((X2))
          SA1       A3+ONE    6         X1=(Y2)   A1=((Y2))
*                             7
*                             8 X5
 +        S_NN      X5        9 X4                          NN=N
          SA5       A1+ONE   10         X5=(Z)    A5=((Z))
*                            11 X3
          LE        NN,B0,EXIT  X2      IF N .LE. 0 EXIT WITH A0 PRESERVED
 +        IX7       X2-X4    13 X1      X7=(X2)-(X)
          IX6       X1-X3    14 X7      X6=(Y2)-(Y)
          S_JX      X7       15 X6                          JX=(X2)-(X)
          S_JY      X6       16                             JY=(Y2)-(Y)
                             17
 PROLOG   ENDM
