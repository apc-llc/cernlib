*
* $Id: vset.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vset.inc
*
*CALL     BASIC
*CALL     SET
 MAIN     MACRO     TYPE,N,JZ,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VSET*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"
          PROLOG    EXIT,N,JZ           X4=(Z), X6=S' [X7=S'']
          "P"VSET   N,X4,JZ,J
          EPILOG
          ENDM
 PROLOG   MACRO     EXIT,NN,JZ
          BSS       0                   X1=(N)    A1=((N))
 +        SA2       A1+1      0         X2=(S)    A2=((S))
          SA1       X1        1         X1=N      A1=(N)
          NO                  2
 +        SA4       A2+1      3         X4=(Z)    A4=((Z))
          SA5       A2+2      4         X5=(Z2)   A5=((Z2))
*                             5
*                             6
*                             7 X2
 TAU      IFEQ      LENGTH,2
          SA3       X2+1      8 X1      X3=S''    A3=(S'')
 TAU      ENDIF
          SA2       X2        9         X2=S'     A2=(S')
          S_NN      X1       10 X4                          NN=N
*                            11 X5
          IX5       X5-X4    12         X5=(Z2)-(Z)
*                            13 X5
          LE        NN,B0,EXIT          IF N<1 EXIT WITH A0 PRESERVED
          S_JZ      X5       15 X3                          JZ=(Z2)-(Z)
 TAU      IFEQ      LENGTH,2
          BX7       X3       16 X2      X7=S''
 TAU      ENDIF
          BX6       X2       17 X7      X6=S'
 PROLOG   ENDM                          X4=(Z)
