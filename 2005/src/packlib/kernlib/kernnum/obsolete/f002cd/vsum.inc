*
* $Id: vsum.inc,v 1.1.1.1 1996/02/15 17:48:00 mclareni Exp $
*
* $Log: vsum.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:00  mclareni
* Kernlib
*
*
*
* vsum.inc
*
*CALL     BASIC
*CALL     SUM
 MAIN     MACRO     TYPE,N,JX,J
          SETYPE    TYPE
 NAME     MICRO     1,,*"P"_VSUM*
          HEADER    "NAME"              A0 IS NEITHER USED NOR SAVED
          EXIT      "NAME"              (N,X,X2)
          PROLOG    EXIT,N,JX           X4=X(1) [X5=X''(1)]
          "P"_VSUM  N,JX,J              X6 [,X7]=SUM
          EPILOG
          ENDM
 PROLOG   MACRO     EXIT,NN,JX
          BSS       0                   X1=(N)    A1=((N))
 +        SA2       A1+1                X2=(X)    A2=((X))
          SA3       A1+2                X3=(X2)   A3=((X2))
 +        SA1       X1                  X1=N      A1=(N)
          MX6       0                   X6=0.
          SA4       X2                  X4=X(1)   A4=(X(1))
          IX0       X3-X2               X0=(X2)-(X)
          S_NN      X1                                      NN=N
 TAU      IFEQ      LENGTH,2
          SA5       X2+1                X5=X''(1) A5=(X''(1))
          MX7       0                   X7=0.
 TAU      ENDIF
          LE        NN,B0,EXIT          IF N<1 EXIT WITH X6[,X7]=0.
          S_JX      X0                                      JX=(X2)-(X)
 PROLOG   ENDM
