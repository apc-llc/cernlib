CDECK  ID>, HWDXLM
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWDXLM(DKVRTX,STAB)
C-----------------------------------------------------------------------
C     Sets STAB=.TRUE. if DKVRTX lies outside the specified region.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION DKVRTX(4),RCYL,RSPH,ZMAX,R,S
      LOGICAL STAB
      PARAMETER (RCYL=20,RSPH=100,ZMAX=500)
      STAB=.FALSE.
      IF (IOPDKL.EQ.1) THEN
C Cylinderical geometry
         S=SQRT(DKVRTX(1)**2+DKVRTX(2)**2)
         IF (S.GE.RCYL.OR.ABS(DKVRTX(3)).GE.ZMAX) STAB=.TRUE.
      ELSEIF (IOPDKL.EQ.2) THEN
C Spherical geometry
         R=SQRT(DKVRTX(1)**2+DKVRTX(2)**2+DKVRTX(3)**2)
         IF (R.GE.RSPH) STAB=.TRUE.
      ELSE
C User supplied geometry
      ENDIF
      RETURN
  999 END
