*
* $Id: hwrint.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrint.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRINT.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      INTEGER FUNCTION HWRINT(IMIN,IMAX)
C ... RANDOM INTEGER IN [IMIN,IMAX]. N.B. ASSUMES IMAX.GE.IMIN
      INTEGER IMIN,IMAX
      DOUBLE PRECISION HWRGEN,RN
    1 RN=HWRGEN(0)
      IF (RN.EQ.1.) GO TO 1
      RN=RN*(IMAX-IMIN+1)
      HWRINT=IMIN+INT(RN)
      END
