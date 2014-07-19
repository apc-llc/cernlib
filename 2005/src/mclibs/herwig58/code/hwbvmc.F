*
* $Id: hwbvmc.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbvmc.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBVMC.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWBVMC(ID)
C     VIRTUAL MASS CUTOFF FOR PARTON TYPE ID
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWBVMC
      INTEGER ID
      IF (ID.EQ.13) THEN
        HWBVMC=RMASS(ID)+VGCUT
      ELSEIF (ID.LT.13) THEN
        HWBVMC=RMASS(ID)+VQCUT
      ELSEIF (ID.EQ.59) THEN
        HWBVMC=RMASS(ID)+VPCUT
      ELSE
        HWBVMC=RMASS(ID)
      ENDIF
      END
