CDECK  ID>, HWUDKL.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWUDKL(ID,PMOM,DISP)
C-----------------------------------------------------------------------
C     Given a real or virtual particle, flavour ID and 4-momentum PMOM,
C     returns DISP its distance travelled in mm.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,PMOM(4),DISP(4),PMOM2,SCALE
      INTEGER ID
      EXTERNAL HWRGEN
      PMOM2=(PMOM(4)+PMOM(3))*(PMOM(4)-PMOM(3))-PMOM(1)**2-PMOM(2)**2
      SCALE=-GEV2MM*LOG(HWRGEN(0))
     &      /SQRT((PMOM2-RMASS(ID)**2)**2+(PMOM2/DKLTM(ID))**2)
      CALL HWVSCA(4,SCALE,PMOM,DISP)
      END
