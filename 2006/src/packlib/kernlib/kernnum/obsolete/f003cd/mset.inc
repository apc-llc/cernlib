*
* $Id: mset.inc,v 1.1.1.1 1996/02/15 17:48:05 mclareni Exp $
*
* $Log: mset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:05  mclareni
* Kernlib
*
*
*
* mset.inc
*
*CALL     BASIC
*CALL     SET
 MAIN     MACRO     TYPE,M,N,Z,IZ,JZ,I,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_MSET*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (M,N,S,Z,Z12,Z21)
          PROLOG    EXIT,M,N,Z,IZ,JZ,J  X6=S
          "P"MSET   M,N,Z,IZ,JZ,I,J
          EPILOG
          ENDM
 PROLOG   MACRO     EX,MM,NN,Z,IZ,JZ,ONE
          BSS       0                   X1=(M)    A1=((M))
          SA4       X1                  X4=M      A4=(M)
          S_ONE     1                                       ONE=1
          SA5       A1+ONE              X5=(N)    A5=((N))
          SA1       A5+ONE              X1=(S)    A1=((S))
          SA2       A1+ONE              X2=(Z)    A2=((Z))
          SA3       A2+ONE              X3=(Z12)  A3=((Z12))
          S_MM      X4                                      MM=M
          SA4       A3+ONE              X4=(Z21)  A4=((Z21))
          LE        MM,B0,EX            IF M.LE.0 EXIT WITH A0 PRESERVED
          SA5       X5                  X5=N      A5=(N)
          SA1       X1                  X1=S      A1=(S)
          S_Z       X2                                      Z=(Z)
          IX7       X3-X2               X7=(Z12)-(Z)
          IX6       X4-X2               X6=(Z21)-(Z)
          S_JZ      X7                                      JZ=(Z12)-(Z)
          S_IZ      X6                                      IZ=(Z21)-(Z)
          S_NN      X5                                      NN=N
          BX6       X1                  X6=S
          LE        NN,B0,EX            IF N.LE.0 EXIT WITH A0 PRESERVED
 PROLOG   ENDM
