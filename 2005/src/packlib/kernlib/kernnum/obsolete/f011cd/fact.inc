*
* $Id: fact.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: fact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* fact.inc
*
*CALL     BASIC
*CALL     FACT
*CALL     DET
*CALL     MAXA
*CALL     SCL
*CALL     MPY
*CALL     SCA
*CALL     XCH
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,JA,A,R,I,J,K
          SETYPE    TYPE
 GNAME    MICRO     1,,*FACT*
 NAME     MICRO     1,,*"P"_"GNAME"*
          HEADER    "NAME",SAVEA0
 ARGLIST  BSS       1                   ((IFAIL))
 SAVEA    BSS       1                   (A(1,1))
 SAVER    BSS       1                   (R(1))
 TEMP7L   BSS       7
          BSS       LENGTH
          EXIT      "NAME"              (N,A,IDIM,R,IFAIL,DET,JFAIL)
          PROLOG    ERROR,SAVEA,SAVER,N,JA,J  X1=(A), X3=(R),A3=((R))
          SX6       A3+J                X6=((IFAIL))
          SA6       ARGLIST             ARGLIST=((IFAIL))
          TFACT     TYPE,SAVEA,SAVER,N,JA,TEMP7L,A,R,I,J,K
          SA2       ARGLIST             X2=((IFAIL))
          SA4       X2+1                X4=(DET)  A4=((DET))
          SA1       X2                  X1=(IFAIL)A1=((IFAIL))
          SA2       X2+2                X2=(JFAIL)A2=((JFAIL))
          SA6       X4                  STORE DET
 TAU      IFEQ      LENGTH,2
          SA7       X4+1
 TAU      ENDIF
          BX6       X5                  X6=IFAIL
          BX7       X0                  X7=JFAIL
          SA6       X1                  STORE IFAIL
          SA7       X2                  STORE JFAIL
          EPILOG
 HMPRNT   VFD       60/10H "P"_"GNAME"
 ERROR    TMPRNT    HMPRNT,X4,X2,B0
          ENDM
