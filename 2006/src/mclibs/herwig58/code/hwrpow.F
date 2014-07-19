*
* $Id: hwrpow.F,v 1.1.1.1 1996/03/08 17:02:16 mclareni Exp $
*
* $Log: hwrpow.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:16  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWRPOW.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWRPOW(XVAL,XJAC)
C     RETURNS XVAL DISTRIBUTED ON (XMIN,XMAX) LIKE XVAL**XPOW
C     AND CORRESPONDING JACOBIAN FACTOR XJAC
C     SET FIRST=.TRUE. IF NEW XMIN,XMAX OR XPOW
      DOUBLE PRECISION HWRGEN,XVAL,XJAC,XMIN,XMAX,XPOW,P,Q,A,B,C,Z
      COMMON/HWRPIN/XMIN,XMAX,XPOW,FIRST
      LOGICAL FIRST
      SAVE Q,A,B,C
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
