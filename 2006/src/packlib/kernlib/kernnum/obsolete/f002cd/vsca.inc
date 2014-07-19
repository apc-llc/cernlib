*
* $Id: vsca.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vsca.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vsca.inc
*
*CALL     BASIC
*CALL     SCA
 SCA      SET       1
 SCS      SET       2
 MAIN     MACRO     TYPE,FCN,N,JX,JY,Z,JZ,J
          SETYPE    TYPE
          IFEQ      FCN,SCA,2
 SIGN     MICRO     1,,*+*
 VERB     MICRO     1,,*VSCA*
          IFEQ      FCN,SCS,2
 SIGN     MICRO     1,,*-*
 VERB     MICRO     1,,*VSCS*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,S,X,X2,Y,Y2,Z,Z2)
          PROLOG    N,JX,JY,Z,JZ,J
          "P"VSCA   N,JX,"SIGN",JY,Z,JZ,J
          EPILOG
          ENDM
 PROLOG   MACRO     NN,JX,JY,Z,JZ,ONE   X4=(X) X2=(Y) X1=S OR
*                                                         X0=S' X1=S''
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
 +        SA1       X1       13 X3      X1=S'     A1=(S')
          IX7       X3-X2    14         X7=(Y2)-(Y)
          SA3       A3+ONE   15 X7      X3=(Z)    A3=((Z))
          S_JY      X7       16                             JY=(Y2)-(Y)
*                            17 X5
 +        IX6       X5-X4    18         X6=(X2)-(X)
          SA5       A3+ONE   19 X6      X5=(Z2)   A5=((Z2))
          S_JX      X6       20 X1                          JX=(X2)-(X)
 TAU      IFEQ      LENGTH,2            TYPE IS RTYPE OR DTYPE
          BX0       X1                  X0=S'
          SA1       A1+ONE              X1=S''
 TAU      ENDIF
          S_Z       X3                                      Z=(Z)
          IX7       X5-X3               X7=(Z2)-(Z)
          S_JZ      X7                                      JZ=(Z2)-(Z)
 PROLOG   ENDM
