*
* $Id: hwssud.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwssud.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWSSUD.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWSSUD(I)
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWSSUD,DMIN
      INTEGER I,N0,IS,ID
      DOUBLE PRECISION QSCA,XLAST,HWSGQQ,DIST(13)
      COMMON/HWTABC/XLAST,N0,IS,ID
      DATA DMIN/1.D-15/
      QSCA=QEV(N0+I,IS)
      CALL HWSFUN(XLAST,QSCA,IDHW(INHAD),NSTRU,DIST,JNHAD)
      IF (ID.EQ.13) DIST(ID)=DIST(ID)*HWSGQQ(QSCA)
      IF (DIST(ID).LT.DMIN) DIST(ID)=DMIN
      HWSSUD=SUD(N0+I,IS)/DIST(ID)
      END
