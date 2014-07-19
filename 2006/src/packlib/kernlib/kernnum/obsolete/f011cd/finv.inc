*
* $Id: finv.inc,v 1.1.1.1 1996/02/15 17:48:12 mclareni Exp $
*
* $Log: finv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:12  mclareni
* Kernlib
*
*
*
* finv.inc
*
*CALL     BASIC
*CALL     FINV
*CALL     MPY
*CALL     SCA
*CALL     SCL
*CALL     SUBST
*CALL     XCH
*CALL     TMPRNT
 MAIN     MACRO     TYPE,N,IA,JA,I,J,K,L
          SETYPE    TYPE
 GNAME    MICRO     1,,*FINV*
 NAME     MICRO     1,,*"P"_"GNAME"*
          HEADER    "NAME",SAVEA0
 SAVEA    BSS       1                   (A(1,1))
 SAVER    BSS       1                   (R(1))
 TEMP4    BSS       4
          EXIT      "NAME"              (N,A,IDIM,R)
          PROLOG    ERROR,SAVEA,SAVER,N,JA,J   X1=(A), X3=(R)
          EQ        N,J,EPILOG          IF N=1 RETURN
          TFINV     TYPE,SAVEA,SAVER,N,JA,TEMP4,IA,I,J,K,L X1=(A),X3=(R)
          EPILOG
 HMPRNT   VFD       60/10H "P"_"GNAME"
 ERROR    TMPRNT    HMPRNT,X4,X2,B0
          ENDM
