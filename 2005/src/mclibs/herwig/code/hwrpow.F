CDECK  ID>, HWRPOW.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWRPOW(XVAL,XJAC)
C-----------------------------------------------------------------------
C     RETURNS XVAL DISTRIBUTED ON (XMIN,XMAX) LIKE XVAL**XPOW
C     AND CORRESPONDING JACOBIAN FACTOR XJAC
C     SET FIRST=.TRUE. IF NEW XMIN,XMAX OR XPOW
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,XVAL,XJAC,XMIN,XMAX,XPOW,P,Q,A,B,C,Z
      LOGICAL FIRST
      EXTERNAL HWRGEN
      SAVE Q,A,B,C
      COMMON/HWRPIN/XMIN,XMAX,XPOW,FIRST
      IF (FIRST) THEN
        P=XPOW+1.
        IF (P.EQ.0.) CALL HWWARN('HWRPOW',500,*999)
        Q=1./P
        A=XMIN**P
        B=XMAX**P-A
        C=B*Q
        FIRST=.FALSE.
      ENDIF
      Z=A+B*HWRGEN(0)
      XVAL=Z**Q
      XJAC=XVAL*C/Z
  999 END
