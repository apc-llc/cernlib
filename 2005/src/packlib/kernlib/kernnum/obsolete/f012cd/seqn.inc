*
* $Id: seqn.inc,v 1.1.1.1 1996/02/15 17:48:13 mclareni Exp $
*
* $Log: seqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:13  mclareni
* Kernlib
*
*
*
* seqn.inc
*
*CALL     BASIC
*CALL     SFACT
*CALL     SCL
*CALL     MPY
*CALL     SCA
*CALL     FEQN
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,IA,JA,K,B,I,J
          SETYPE    TYPE
 GNAME    MICRO     1,,*EQN*
 NAME     MICRO     1,,*"P"S"GNAME"*
          HEADER    "NAME",SAVEA0
 LOCIFAIL BSS       1                   (IFAIL)
 SAVEA    BSS       1                   (A)
 SAVEK    BSS       1                   K
 SAVEB    BSS       1                   (B)
 TEMP5    BSS       5
          EXIT      "NAME"              (N,A,IDIM,IFAIL,K,B)
          PROLOG    ERROR,SAVEA,LOCIFAIL,SAVEK,SAVEB,N,JA,J  X1=(A)
          TSFACT    SAVEA,N,JA,TEMP5,K,B,IA,I,J   X5=IFAIL
          SA2       SAVEK               X2=K
          SA3       SAVEB               X3=(B)
          SA1       SAVEA               X1=(A)
          NZ        X5,[1]              IF NOT POSITIVE DEFINITE
          S_IA      LENGTH              IA=LENGTH
          S_K       X2                  LOAD K
          S_B       X3                  LOAD B
          SA0       X1                  A0=(A)
          "P"FEQN   N,A0,IA,JA,K,B,I,J
          SX5       B0                  X5=IFAIL=0
 [1]      SA2       LOCIFAIL            X2=(IFAIL)
          BX6       X5                  X6=IFAIL
          SA6       X2                  STORE IFAIL
          EPILOG
 HMPRNT   VFD       60/10H_"P"S_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,X3
          ENDM
 PROLOG   MACRO     ERROR,SAVEA,LOCIFAIL,SAVEK,SAVEB,NN,JA,ONE X1=(A)
          BSS       0                   X1=(N)    A1=((N))
          SA2       A1+2                X2=(IDIM) A2=((IDIM))
          S_ONE     1                                       ONE=1
          SA4       X1                  X4=N      A4=(N)
          SA1       A1+ONE              X1=(A)    A1=((A))
          SX7       A0                  X7=OLD A0
          NO
          SA3       A2+2                X3=(K)    A3=((K))
          SA7       SAVEA0              SAVEA0=OLD A0
          SA5       A3+ONE              X5=(B)    A5=((B))
          S_NN      X4                                      NN=N
          SA4       X2                  X4=IDIM   A4=(IDIM)
          SX0       NN                  X0=N
          SA3       X3                  X3=K      A3=(K)
          SA2       A2+ONE              X2=(IFAIL) A2=((IFAIL))
          LT        NN,ONE,ERROR        IF N<1
          S_JA      X4                                      JA=IDIM
          NO
          LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1                  X6=(A)
          BX7       X5                  X7=(B)
          SA6       SAVEA               SAVEA=(A)
          SA7       SAVEB               SAVEB=(B)
          MI        X3,ERROR            IF K<0
          BX6       X2                  X6=(IFAIL)
          BX7       X3                  X7=K
          SA6       LOCIFAIL            LOCIFAIL=((IFAIL))
          SA7       SAVEK               SAVEK=K
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
 PROLOG   ENDM
