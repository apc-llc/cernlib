*
* $Id: vscl.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vscl.inc
*
*CALL     BASIC
*CALL     SCL
 MAIN     MACRO     TYPE,N,X,JX,Z,JZ,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VSCL*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,S,X,X2,Z,Z2)
          PROLOG    N,X,JX,Z,JZ,J
 TAU      IFEQ      LENGTH,2
          SA2       A1+1                X2=S''
 TAU      ENDIF
          "P"VSCL   N,X,JX,Z,JZ,J
          EPILOG
          ENDM
 PROLOG   MACRO     NN,X,JX,Y,JY,ONE         X1=S, X6=N-1, A1=(S)
          BSS       0                   X1=(N)    A1=((N))
 +        S_ONE     1         0                             ONE=1
          SA5       X1        1         X5=N      A5=(N)
          SA1       A1+ONE    2         X1=(S)    A1=((S))
*                             3
 +        SA2       A1+3      4         X2=(Y)    A2=((Y))
          SA4       A1+ONE    5         X4=(X)    A4=((X))
          SA3       A2+ONE    6         X3=(Y2)   A3=((Y2))
*                             7
*                             8 X5
 +        S_NN      X5        9 X1                          NN=N
          SA5       A4+ONE   10         X5=(X2)   A5=((X2))
*                            11 X2
          LE        NN,B0,EXIT  X4      IF N.LE.0 EXIT WITH A0 PRESERVED
 +        SA1       X1       13 X3      X1=S      A1=(S)
          IX7       X3-X2    14         X7=(Y2)-(Y)
          SX6       NN-ONE   15 X7      X6=N-1
          S_Y       X2       16 X6                          Y=(Y)
 +        S_JY      X7       17 X5                          JY=(Y2)-(Y)
          IX7       X5-X4    18         X7=(X2)-(X)
          S_X       X4       19 X7                          X=(X)
          S_JX      X7       20 X1                          JX=(X2)-(X)
 PROLOG   ENDM
