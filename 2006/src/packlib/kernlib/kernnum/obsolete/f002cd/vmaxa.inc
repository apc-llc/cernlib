*
* $Id: vmaxa.inc,v 1.1.1.1 1996/02/15 17:47:59 mclareni Exp $
*
* $Log: vmaxa.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:59  mclareni
* Kernlib
*
*
*
* vmaxa.inc
*
*CALL     BASIC
*CALL     MAXA
 MAIN     MACRO     TYPE,N,JX,L,LOCL,LOCE,K,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VMAXA*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2,L,E)
          PROLOG    [2],N,JX,LOCL,LOCE,J
          "P"VMAXA  N,JX,L,K,J
 [1]      STOREF    TYPE,LOCE
          SX6       L
          SA6       LOCL
          EPILOG
 [2]      SX6       0                   E=0.
 TAU      IFEQ      LENGTH,2
          SX7       0
 TAU      ENDIF
          S_L       0
          JP        [1]
          ENDM
 PROLOG   MACRO     EX,N,JX,LOCL,LOCE,ONE
*                                       X1=(X)
          BSS       0                   X1=(N)    A1=((N))
          S_ONE     1                                       ONE=1
          SA2       A1+3                X2=(L)    A2=((L))
          SA3       X1                  X3=N      A3=(N)
          SA1       A1+ONE              X1=(X)    A1=((X))
          SA4       A2+ONE              X4=(E)    A4=((E))
          SA5       A1+ONE              X5=(X2)   A5=((X2))
          S_LOCL    X2                                      LOCL=(L)
          S_N       X3                                      N=N
          IX6       X5-X1               X6=(X2)-(X)=JX
          S_LOCE    X4                                      LOCE=(E)
          S_JX      X6                                      JX=(X2)-(X)
          LT        N,ONE,EX            IF N<1
 PROLOG   ENDM
