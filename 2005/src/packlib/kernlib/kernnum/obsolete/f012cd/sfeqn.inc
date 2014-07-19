*
* $Id: sfeqn.inc,v 1.1.1.1 1996/02/15 17:48:13 mclareni Exp $
*
* $Log: sfeqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:13  mclareni
* Kernlib
*
*
*
* sfeqn.inc
*
*CALL     BASIC
*CALL     FEQN
*CALL     MPY
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,IA,JA,K,B,I,J
          SETYPE    TYPE
 GNAME    MICRO     1,,*FEQN*
 NAME     MICRO     1,,*"P"S"GNAME"*
          HEADER    "NAME",SAVEA0
 SAVEA    BSS       1                   (A)
          EXIT      "NAME"              (N,A,IDIM,K,B)
          PROLOG    ERROR,SAVEA,N,JA,K,B,J            X1=(A)
          S_IA      LENGTH              IA=LENGTH
          SA0       X1                  A0=(A)
          "P"FEQN   N,A0,IA,JA,K,B,I,J
          EPILOG
 HMPRNT   VFD       60/10H_"P"S_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,X3
          ENDM
 PROLOG   MACRO     ERROR,SAVEA,NN,JA,KK,B,ONE           X1=(A)
          BSS       0                   X1=(N)    A1=((N))
          SA2       A1+2                X2=(IDIM) A2=((IDIM))
          S_ONE     1                                       ONE=1
          SA4       X1                  X4=N      A4=(N)
          SA1       A1+ONE              X1=(A)    A1=((A))
          SX7       A0                  X7=OLD A0
          SA3       A2+ONE              X3=(K)    A3=((K))
          SA7       SAVEA0              SAVEA0=OLD A0
          SA5       A3+ONE              X5=(B)    A5=((B))
          S_NN      X4                                      NN=N
          SA4       X2                  X4=IDIM   A4=(IDIM)
          SX0       NN                  X0=N
          SA3       X3                  X3=K      A3=(K)
          LT        NN,ONE,ERROR        IF N<1
          S_JA      X4                                      JA=IDIM
          NO
          LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1                  X6=(A)
          S_B       X5                                      B=(B)
          S_KK      X3                                      KK=K
          SA6       SAVEA               SAVEA=(A)
          LT        KK,ONE,ERROR        IF K<1
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
 PROLOG   ENDM
