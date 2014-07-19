*
* $Id: hwsval.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwsval.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWSVAL.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWSVAL(ID)
C     TRUE FOR VALENCE PARTON ID IN INCOMING HADRON INHAD
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER ID,IDHAD
      LOGICAL HWSVAL
      HWSVAL=.FALSE.
      IDHAD=IDHW(INHAD)
      IF (IDHAD.EQ.73.OR.IDHAD.EQ.75) THEN
        IF (ID.EQ.1.OR.ID.EQ.2) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.91.OR.IDHAD.EQ.93) THEN
        IF (ID.EQ.7.OR.ID.EQ.8) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.30) THEN
        IF (ID.EQ.1.OR.ID.EQ.8) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.38) THEN
        IF (ID.EQ.2.OR.ID.EQ.7) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.59) THEN
        IF (ID.LT.6.OR.(ID.GT.6.AND.ID.LT.12)) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.71.OR.IDHAD.EQ.72) THEN
        IF (ID.EQ.13) HWSVAL=.TRUE.
      ELSE
        CALL HWWARN('HWSVAL',100,*999)
      ENDIF
  999 END
