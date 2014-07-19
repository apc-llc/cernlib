*
* $Id: vran.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vran.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vran.inc
*
*CALL     BASIC
*CALL     RAN
 MAIN     MACRO     TYPE,N,Z,JZ,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VRAN*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXT       RANDOM.             FTN SYSTEM LOCATION OF SEED
          EXIT      "NAME"
          PROLOG    EXIT,N,Z,JZ,J       X4=SEED, X5=MULT
          "P"_VRAN  N,Z,JZ,J
          SA7       RANDOM.             SAVE OLD SEED
          EPILOG
          ENDM
 PROLOG   MACRO     EX,NN,Z,JZ,ONE
          BSS       0                   X1=(N)    A1=((N))
 +        SA3       A1+2                X3=(B)    A3=((B))
          S_ONE     1                                       ONE=1
 +        SA1       X1                  X1=N      A1=(N)
          SA4       A3+ONE              X4=(Z)    A4=((Z))
          SA2       A3-ONE              X2=(A)    A2=((A))
          SA5       A4+ONE              X5=(Z2)   A5=((Z2))
          SA3       X3                  X3=B      A3=(B)
          S_NN      X1                                      NN=N
          S_Z       X4                                      Z=(Z)
          SA2       X2                  X2=A      A2=(A)
          IX6       X5-X4               X6=(Z2)-(Z)
          SA4       RANDOM.             X4=SEED   A4=(SEED)
          S_JZ      X6                                      JZ=(Z2)-(Z)
          LE        NN,B0,EX            IF N.LE.0 EXIT WITH A0 PRESERVED
          SA5       A4+ONE              X5=MULT   A5=(MULT)
          RX3       X3-X2               X3=B-A=C
          BX0       X2                  X0=A
          NX3       X3
*                                       X0=A
*                                       X3=C
*                                       X4=SEED
*                                       X5=MULT
 PROLOG   ENDM
