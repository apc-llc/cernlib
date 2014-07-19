*
* $Id: hwhigt.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhigt.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGT.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWHIGT(RATIO)
      DOUBLE PRECISION HWHIGT
C  CALCULATE MOD SQUARED I FOR RATIO = Mtop / Mhiggs
C  I DEFINED AS IN BARGER & PHILLIPS p433
C  WARNING: THIS IS A FACTOR OF 3 GREATER THAN EHLQ'S ETA FUNCTION
C-----------------------------------------------------------------------
      DOUBLE PRECISION PI,RATIO,RAT2,FREAL,FIMAG,ETALOG,AIREAL,AIIMAG
      PARAMETER (PI=3.14159D0)
      RAT2=RATIO**2
      IF     (RAT2.GT.0.25) THEN
         FREAL=-2.*ASIN(0.5/RATIO)**2
         FIMAG=0
      ELSEIF (RAT2.LT.0.25) THEN
         ETALOG=LOG( (0.5+SQRT(0.25-RAT2)) / (0.5-SQRT(0.25-RAT2)) )
         FREAL=0.5 * (ETALOG**2 - PI**2)
         FIMAG=PI * ETALOG
      ELSE
         FREAL=0.5 * (          - PI**2)
         FIMAG=0
      ENDIF
      AIREAL=3*( 2*RAT2 + RAT2*(4*RAT2-1)*FREAL )
      AIIMAG=3*(          RAT2*(4*RAT2-1)*FIMAG )
      HWHIGT=AIREAL**2 + AIIMAG**2
      END
