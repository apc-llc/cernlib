*
* $Id: vdiv.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: vdiv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* vdiv.inc
*
*CALL     BASIC
*CALL     DIV
 MAIN     MACRO     TYPE,N,JX,JY,JZ,LFAIL,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VDIV*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,Y,Y2,Z,Z2,IFAIL)
          PROLOG    [2],N,JX,JY,JZ,LFAIL,J        IF N=0 GOTO [2]
          "P"VDIV   N,X1,JX,X2,JY,X3,JZ,J
          SX6       J                                       X6=IFAIL
 [1]      SA6       LFAIL               STORE IFAIL IN LFAIL
          EPILOG
 [2]      SX6       0                   IFAIL=0
          JP        [1]
          ENDM
 PROLOG   MACRO     EX,NN,JX,JY,JZ,LFAIL,ONE
*                                       X1=(X)
*                                       X2=(Y)
*                                       X3=(Z)
          BSS       0                   X1=(N)    A1=((N))
 +        S_ONE     1         0                             ONE=1
          SA3       X1        1         X3=N      A3=(N)
          SA1       A1+ONE    2         X1=(X)    A1=((X))
*                             3
 +        SA2       A1+2      4         X2=(Y)    A2=((Y))
          SA5       A1+ONE    5         X5=(X2)   A5=((X2))
          SA4       A2+ONE    6         X4=(Y2)   A4=((Y2))
*                             7
*                             8 X3
 +        S_NN      X3        9 X1                          NN=N
          SA3       A4+ONE   10         X3=(Z)    A3=((Z))
*                            11 X2
*                            12 X5
          IX7       X5-X1    13 X4      X7=(X2)-(X)
          SA5       A3+ONE   14 X7      X5=(Z2)   A5=((Z2))
 +        IX6       X4-X2    15         X6=(Y2)-(Y)
          SA4       A5+ONE   16 X6      X4=(IFAIL) A4=((IFAIL))
          S_JX      X7       17 X3                          JX=(X2)-(X)
          S_JY      X6       18                             JY=(Y2)-(Y)
*                            19
*                            20
*                            21 X5
*                            22
          IX7       X5-X3    23 X4      X7=(Z2)-(Z)
          S_LFAIL   X4       24 X7                        LFAIL=(IFAIL)
          S_JZ      X7       25                             JZ=(Z2)-(Z)
*                            26
          LE        NN,B0,EX 27         IF N.LE.0 EXIT WITH LFAIL SET
 PROLOG   ENDM
