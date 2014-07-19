*
* $Id: sfinv.inc,v 1.1.1.1 1996/02/15 17:48:13 mclareni Exp $
*
* $Log: sfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:13  mclareni
* Kernlib
*
*
*
* sfinv.inc
*
*CALL     BASIC
*CALL     SFINV
*CALL     UTL
*CALL     CPY
*CALL     MPY
*CALL     SCL
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,IA,JA,I,J,K,L
          SETYPE    TYPE
 GNAME    MICRO     1,,*FINV*
 NAME     MICRO     1,,*"P"S"GNAME"*
          HEADER    "NAME",SAVEA0
 SAVEA    BSS       1                   (A)
 TEMP4    BSS       4
          EXIT      "NAME"              (N,A,IDIM)
          PROLOG    ERROR,SAVEA,N,JA,J            X1=(A)
          TSFINV    TYPE,SAVEA,N,JA,TEMP4,IA,I,J,K,L        X1=(A)
          EPILOG
 HMPRNT   VFD       60/10H_"P"S_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,B0
          ENDM
 PROLOG   MACRO     ERROR,SAVEA,NN,JA,ONE           X1=(A)
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
          LT        JA,NN,ERROR         IF IDIM<N
          BX6       X1                  X6=(A)
          SA6       SAVEA               SAVEA=(A)
 TAU      IFEQ      LENGTH,2
          S_JA      JA+JA                              JA=LENGTH*IDIM
 TAU      ENDIF
 PROLOG   ENDM
