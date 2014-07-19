CDECK  ID>, HWUBPR.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWUBPR
C-----------------------------------------------------------------------
C     PRINTS OUT DATA ON PARTON SHOWER
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER I,J
      IF (PRVTX) THEN
        WRITE(6,10) INHAD,XFACT
  10    FORMAT(///10X,'DATA ON LAST PARTON SHOWER:   INHAD =',I3,
     &  '    XFACT =',E11.3//'  IPAR ID     TM  DA1 CMO AMO CDA',
     &  ' ADA  P-X     P-Y     P-Z   ENERGY    MASS',
     &  '   V-X        V-Y        V-Z        V-C*T')
        DO 20 J=1,NPAR
  20    WRITE(6,30) J,RNAME(ABS(IDPAR(J))),TMPAR(J),JDAPAR(1,J),
     &   (JCOPAR(I,J),I=1,4),(PPAR(I,J),I=1,5),(VPAR(I,J),I=1,4)
  30    FORMAT(I5,1X,A8,L2,5I4,F7.2,4F8.2,4E11.4)
      ELSE
        WRITE(6,40) INHAD,XFACT
  40    FORMAT(///10X,'DATA ON LAST PARTON SHOWER:   INHAD =',I3,
     &  '    XFACT =',E11.3//'  IPAR ID     TM  DA1 CMO AMO CDA',
     &  ' ADA  P-X     P-Y     P-Z   ENERGY    MASS')
        DO 50 J=1,NPAR
  50    WRITE(6,60) J,RNAME(ABS(IDPAR(J))),TMPAR(J),JDAPAR(1,J),
     &   (JCOPAR(I,J),I=1,4),(PPAR(I,J),I=1,5)
  60    FORMAT(I5,1X,A8,L2,5I4,F7.2,4F8.2)
      ENDIF
      END
