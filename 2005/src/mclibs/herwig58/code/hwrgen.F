*
* $Id: hwrgen.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrgen.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRGEN.
*CMZ :-        -26/04/91  12.42.30  by  Federico Carminati
*-- Author :    F. James, modified by Mike Seymour
C-----------------------------------------------------------------------
      FUNCTION HWRGEN(I)
C     MAIN RANDOM NUMBER GENERATOR
C     USES METHOD OF l'Ecuyer, (VIA F.JAMES, COMP PHYS COMM 60(1990)329)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION HWRGEN,HWRSET,HWRGET
      INTEGER I,ISEED(2),K,IZ,JSEED(2)
      SAVE ISEED
      DATA ISEED/12345,67890/
      K=ISEED(1)/53668
      ISEED(1)=40014*(ISEED(1)-K*53668)-K*12211
      IF (ISEED(1).LT.0) ISEED(1)=ISEED(1)+2147483563
      K=ISEED(2)/52774
      ISEED(2)=40692*(ISEED(2)-K*52774)-K*3791
      IF (ISEED(2).LT.0) ISEED(2)=ISEED(2)+2147483399
      IZ=ISEED(1)-ISEED(2)
      IF (IZ.LT.1) IZ=IZ+2147483562
      HWRGEN=DBLE(IZ)*4.656613001013252D-10
C--->                (4.656613001013252D-10 = 1.D0/2147483589)
      RETURN
      ENTRY HWRSET(JSEED)
      IF (JSEED(1).EQ.0.OR.JSEED(2).EQ.0) CALL HWWARN('HWRSET',99,*999)
      ISEED(1)=JSEED(1)
      ISEED(2)=JSEED(2)
 999  RETURN
      ENTRY HWRGET(JSEED)
      JSEED(1)=ISEED(1)
      JSEED(2)=ISEED(2)
      RETURN
      END
