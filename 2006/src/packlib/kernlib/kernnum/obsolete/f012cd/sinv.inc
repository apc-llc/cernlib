*
* $Id: sinv.inc,v 1.1.1.1 1996/02/15 17:48:13 mclareni Exp $
*
* $Log: sinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:13  mclareni
* Kernlib
*
*
*
* sinv.inc
*
*CALL     BASIC
*CALL     SFACT
*CALL     SCL
*CALL     MPY
*CALL     SCA
*CALL     SFINV
*CALL     UTL
*CALL     CPY
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,IA,JA,I,J,K,L
          SETYPE    TYPE
 GNAME    MICRO     1,,*INV*
 NAME     MICRO     1,,*"P"S"GNAME"*
          HEADER    "NAME",SAVEA0
 LOCIFAIL BSS       1                   (IFAIL)
 SAVEA    BSS       1                   (A)
 TEMP5    BSS       5
          EXIT      "NAME"              (N,A,IDIM,IFAIL)
          PROLOG    ERROR,SAVEA,LOCIFAIL,N,JA,J      X1=(A)
          TSFACT    SAVEA,N,JA,TEMP5,IA,I,J,K,L       X5=IFAIL
          SA1       SAVEA               X1=(A)
          NZ        X5,[1]              IF NOT POSITIVE DEFINITE
          TSFINV    TYPE,SAVEA,N,JA,TEMP5,IA,I,J,K,L        X1=(A)
          SX5       B0                  X5=IFAIL=0
 [1]      SA2       LOCIFAIL            X2=(IFAIL)
          BX6       X5                  X6=IFAIL
          SA6       X2                  STORE IFAIL
          EPILOG
 HMPRNT   VFD       60/10H_"P"S_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,B0
          ENDM
 PROLOG   MACRO     ERROR,SAVEA,LOCIFAIL,NN,JA,ONE        X1=(A)
          BSS       0                   X1=(N)    A1=((N))
          SA2       A1+2                X2=(IDIM) A2=((IDIM))
          S_ONE     1                                       ONE=1
          SA4       X1                  X4=N      A4=(N)
          SA1       A1+ONE              X1=(A)    A1=((A))
          SX7       A0                  X7=OLD A0
          NO
          SA7       SAVEA0              SAVEA0=OLD A0
          S_NN      X4                                      NN=N
          SA4       X2                  X4=IDIM   A4=(IDIM)
          SX0       NN                  X0=N
          SA2       A2+ONE              X2=(IFAIL) A2=((IFAIL))
          LT        NN,ONE,ERROR        IF N<1
          S_JA      X4                                      JA=IDIM
          NO
          LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1                  X6=(A)
          SA6       SAVEA               SAVEA=(A)
          BX6       X2                  X6=(IFAIL)
          SA6       LOCIFAIL            LOCIFAIL=((IFAIL))
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
 PROLOG   ENDM
