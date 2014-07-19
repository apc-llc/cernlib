*
* $Id: hwuidt.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwuidt.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUIDT.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      SUBROUTINE HWUIDT(IOPT,IPDG,IWIG,NWIG)
C
C     TRANSLATES PARTICLE IDENTIFIERS:
C     IPDG = PARTICLE DATA GROUP CODE
C     IWIG = HERWIG IDENTITY CODE
C     NWIG = HERWIG CHARACTER*4 NAME
C
C     IOPT= 1 GIVEN IPDG, RETURNS IWIG AND NWIG
C     IOPT= 2 GIVEN IWIG, RETURNS IPDG AND NWIG
C     IOPT= 3 GIVEN NWIG, RETURNS IPDG AND IWIG
C----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IOPT,IPDG,IWIG,I
      CHARACTER*4 NWIG
      IF (IOPT.EQ.1) THEN
        DO 10 I=1,NRES
        IF (IDPDG(I).EQ.IPDG) THEN
          IWIG=I
          NWIG=RNAME(I)
          RETURN
        ENDIF
   10   CONTINUE
        IWIG=20
        NWIG=RNAME(20)
        CALL HWWARN('HWUIDT',101,*999)
      ELSEIF (IOPT.EQ.2) THEN
        IF (IWIG.LT.1.OR.IWIG.GT.NRES) THEN
          IPDG=0
          NWIG=RNAME(20)
          CALL HWWARN('HWUIDT',102,*999)
        ELSE
          IPDG=IDPDG(IWIG)
          NWIG=RNAME(IWIG)
          RETURN
        ENDIF
      ELSEIF (IOPT.EQ.3) THEN
        DO 30 I=1,NRES
        IF (RNAME(I).EQ.NWIG) THEN
          IWIG=I
          IPDG=IDPDG(I)
          RETURN
        ENDIF
   30   CONTINUE
        IWIG=20
        IPDG=0
        CALL HWWARN('HWUIDT',103,*999)
      ELSE
        CALL HWWARN('HWUIDT',404,*999)
      ENDIF
  999 END
