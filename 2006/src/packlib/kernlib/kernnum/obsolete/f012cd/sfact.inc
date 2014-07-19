*
* $Id: sfact.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: sfact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* sfact.inc
*
*CALL     BASIC
*CALL     SFACT
*CALL     DET
*CALL     SCL
*CALL     MPY
*CALL     SCA
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,JA,A1K,AKK,I,J,K
          SETYPE    TYPE
 GNAME    MICRO     1,,*FACT*
 NAME     MICRO     1,,*"P"S"GNAME"*
          HEADER    "NAME",SAVEA0
 ARGLIST  BSS       1                   ((IFAIL))
 SAVEA    BSS       1                   (A)
 TEMP6L   BSS       6
          BSS       LENGTH              DET
          EXIT      "NAME"              (N,A,IDIM,IFAIL,DET,JFAIL)
          PROLOG    ERROR,SAVEA,ARGLIST,N,JA,J       X1=(A)
          TSFACTD   SAVEA,N,JA,TEMP6L,A1K,AKK,I,J,K   X5=IFAIL
*                                                           X6,X7=DET
*                                                           X0=JFAIL
          SA2       ARGLIST             X2=((IFAIL))
          SA4       X2+1                X4=(DET)  A4=((DET))
          SA1       X2                  X1=(IFAIL) A1=((IFAIL))
          SA2       X2+2                X2=(JFAIL) A2=((JFAIL))
          SA6       X4                  STORE DET
 TAU      IFEQ      LENGTH,2
          SA7       X4+1
 TAU      ENDIF
          BX6       X5                  X6=IFAIL
          BX7       X0                  X7=JFAIL
          SA6       X1                  STORE IFAIL
          SA7       X2                  STORE JFAIL
          EPILOG
 HMPRNT   VFD       60/10H_"P"S_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,X3
          ENDM
 PROLOG   MACRO     ERROR,SAVEA,ARGLIST,NN,JA,ONE X1=(A)
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
          LT        NN,ONE,ERROR        IF N<1
          S_JA      X4                                      JA=IDIM
          NO
          LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1                  X6=(A)
          SA6       SAVEA               SAVEA=(A)
          SX6       A2+ONE              X6=((IFAIL))
          SA6       ARGLIST             ARGLIST=((IFAIL))
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
 PROLOG   ENDM
