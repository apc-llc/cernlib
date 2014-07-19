*
* $Id: mran.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mran.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mran.inc
*
*CALL     BASIC
*CALL     RAN
 MAIN     MACRO     TYPE,M,N,Z,IZ,JZ,I,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_MRAN*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXT       RANDOM.             FTN SYSTEM LOCATION OF SEED
          EXIT      "NAME"              (M,N,A,B,Z,Z12,Z21)
          PROLOG    EXIT,M,N,Z,IZ,JZ,I
          "P"MRAN   M,N,Z,IZ,JZ,I,J
          BX6       X4                  X6=SEED
          SA6       RANDOM.
          EPILOG
          ENDM
 PROLOG   MACRO     EX,MM,NN,Z,IZ,JZ,ONE  X0=A, X3=C, X4=SEED, X5=MULT
          BSS       0                   X1=(M)    A1=((M))
 +        S_ONE     1         0                             ONE=1
          SA2       X1        1         X2=M      M2=(A)
          SA3       A1+ONE    2         X3=(N)    A3=((N))
*                             3
 +        SA4       A3+ONE    4         X4=(A)    A4=((A))
          SA5       A3+3      5         X5=(Z)    A5=((Z))
          SA1       A4+ONE    6         X1=(B)    A1=((B))
*                             7
*                             8 X2
 +        S_MM      X2        9 X3                          MM=M
          SA3       X3       10         X3=N      A3=(N)
          SA2       A5+ONE   11 X4      X2=(Z12)  A2=((Z12))
          SA4       X4       12 X5      X4=A      A4=(A)
 +        BX6       X5       13 X1      X6=(Z)
          S_Z       X5       14                             Z=(Z)
          SA5       A2+ONE   15         X5=(Z21)  A5=((Z21))
          SA1       X1       16         X1=B      A1=(B)
*                            17 X3
 +        LE        MM,B0,EX 18 X2      IF M.LE.0 EXIT WITH A0 PRESERVED
          S_NN      X3       19 X4                          NN=N
          IX7       X2-X6    20         X7=(Z12)-(Z)
*                            21 X7
*                            22 X5
 +        LE        NN,B0,EX 23 X1      IF N.LE.0 EXIT WITH A0 PRESERVED
          FX3       X1-X4    24         X3=B-A=C
          BX0       X4       25         X0=A
 +        SA4       RANDOM.  26 X0      X4=SEED   A4=(SEED)
          S_JZ      X7       27 X3                          JZ=(Z12)-(Z)
          IX7       X5-X6    28         X7=(Z21)-(Z)
 +        SA5       A4+ONE   29 X7      X5=MULT   A5=(MULT)
          NX3       X3       30         X3=C=B-A
          S_IZ      X7       31                             IZ=(Z21)-(Z)
 PROLOG   ENDM
