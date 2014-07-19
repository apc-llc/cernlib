*
* $Id: hwdhgc.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdhgc.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDHGC.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWDHGC(TAU,FNREAL,FNIMAG)
C  CALCULATE THE COMPLEX FUNCTION F OF HHG eq 2.18
C  FOR USE IN H-->GAMMGAMM DECAYS
C-----------------------------------------------------------------------
      DOUBLE PRECISION TAU,FNREAL,FNIMAG,FNLOG,FNSQR,PIFAC
      PARAMETER (PIFAC=3.141519D0)
      IF (TAU.GT.1) THEN
        FNREAL=(ASIN(1/SQRT(TAU)))**2
        FNIMAG=0
      ELSEIF (TAU.LT.1) THEN
        FNSQR=SQRT(1-TAU)
        FNLOG=LOG((1+FNSQR)/(1-FNSQR))
        FNREAL=-0.25 * (FNLOG**2 - PIFAC**2)
        FNIMAG= 0.5  * PIFAC*FNLOG
      ELSE
        FNREAL=0.25*PIFAC**2
        FNIMAG=0
      ENDIF
      END
