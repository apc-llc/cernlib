*
* $Id: vadd.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: vadd.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* vadd.inc
*
*CALL     BASIC
*CALL     ADD
 ADD      SET       1
 SUB      SET       2
 MAIN     MACRO     TYPE,FCN,N,JX,JY,Z,JZ,J
          SETYPE    TYPE
          IFEQ      FCN,ADD,2
 SIGN     MICRO     1,,*+*
 VERB     MICRO     1,,*VADD*
          IFEQ      FCN,SUB,2
 SIGN     MICRO     1,,*-*
 VERB     MICRO     1,,*VSUB*
 NAME     MICRO     1,,*"P"_"VERB"*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,Y,Y2,Z,Z2)
          PROLOG    EXIT,N,JX,JY,Z,JZ,J   X2=(X) X4=(Y)
          "P"VADD   N,JX,"SIGN",JY,Z,JZ,J
          EPILOG
          ENDM
 PROLOG   MACRO     EXIT,NN,JX,JY,Z,JZ,ONE
          BSS       0                   X1=(N)    A1=((N)
 +        S_ONE     1         0                             ONE=1
          SA3       X1        1         X3=N      A3=(N)
          SA2       A1+ONE    2         X2=(X)    A2=((X))
*                             3
 +        SA1       A2+ONE    4         X1=(X2)   A1=((X2))
          SA5       A2+4      5         X5=(Z)    A5=((Z))
          SA4       A1+ONE    6         X4=(Y)    A4=((Y))
*                             7
*                             8 X3
 +        S_NN      X3        9 X2                          NN=N
          SA3       A4+ONE   10         X3=(Y2)   A3=((Y2))
*                            11 X1
          LE        NN,B0,EXIT  X5      IF N.LE.0 EXIT WITH A0 PRESERVED
 +        IX0       X1-X2    13 X4      X0=(X2)-(X)
          SA1       A5+ONE   14 X0      X1=(Z2)   A1=((Z2))
          S_JX      X0       15                             JX=(X2)-(X)
          S_Z       X5       16                             Z=(Z)
*                            17 X3
 +        IX3       X3-X4    18         X3=(Y2)-(Y)
*                            19 X3
          S_JY      X3       20                             JY=(Y2)-(Y)
*                            21 X1
          IX1       X1-X5    22         X1=(Z2)-(Z)
*                            23 X1
          S_JZ      X1       24                             JZ=(Z2)-(Z)
 PROLOG   ENDM                          X2=(X) X4=(Y)
