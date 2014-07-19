*
* $Id: hwdwwt.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdwwt.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDWWT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      FUNCTION HWDWWT(EMSQ,A,B,C)
      DOUBLE PRECISION HWDWWT
C     MATRIX ELEMENT SQUARED FOR V-A WEAK DECAY
C------------------------------------------------------------------------
      DOUBLE PRECISION EMSQ,A,B,C
      HWDWWT=(A-EMSQ)*(EMSQ-B)*C
      END
