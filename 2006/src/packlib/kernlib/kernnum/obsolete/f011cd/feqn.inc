*
* $Id: feqn.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: feqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* feqn.inc
*
*CALL     BASIC
*CALL     FEQN
*CALL     MPY
*CALL     SUBST
*CALL     XCH
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,JA,K,B,I,J,L
          SETYPE    TYPE
 GNAME    MICRO     1,,*FEQN*
 NAME     MICRO     1,,*"P"_"GNAME"*
          HEADER    "NAME",SAVEA0
 SAVEA    BSS       1
 TEMP1    BSS       1
          EXIT      "NAME"              (N,A,IDIM,R,K,B)
          PROLOG    ERROR,SAVEA,N,JA,K,B,J    X3=(R)    X1=(A)   JA=IDIM
          TFEQN     TYPE,SAVEA,N,JA,K,B,TEMP1,I,J,L   X1=(A), X3=(R)
          EPILOG
 HMPRNT   VFD       60/10H "P"_"GNAME"
 ERROR    TMPRNT    HMPRNT,X0,X4,X5
          ENDM
