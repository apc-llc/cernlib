*
* $Id: hwusta.F,v 1.1.1.1 1996/03/08 17:02:18 mclareni Exp $
*
* $Log: hwusta.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:18  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.49  by  Unknown
*-- Author :
CDECK  ID>, HWUSTA.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWUSTA(NAME)
C     MAKES PARTICLE TYPE 'NAME' STABLE
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IPDG,IWIG
      CHARACTER*4 NAME
      CALL HWUIDT(3,IPDG,IWIG,NAME)
      IF (IWIG.EQ.20) CALL HWWARN('HWUSTA',500,*999)
      MODEF(IWIG)=0
      WRITE (6,10) IWIG,NAME
   10 FORMAT(10X,'PARTICLE TYPE',I4,'=',A4,' SET STABLE')
  999 END
