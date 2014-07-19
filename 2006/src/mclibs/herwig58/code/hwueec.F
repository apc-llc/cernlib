*
* $Id: hwueec.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwueec.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUEEC.
*CMZ :-        -26/04/91  14.22.30  by  Federico Carminati
*-- Author :    Bryan Webber and Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWUEEC(IL)
C------------------------------------------------------------------------
C     Loads cross-section coefficients, for kinematically open channels,
C     in llbar-->qqbar; lepton label, IL=1-6: e,nu_e,mu,nu_mu,tau,nu_tau.
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION Q2
      INTEGER IL,JL,IQ
      Q2=EMSCA**2
      JL=IL+10
      MAXFL=0
      TQWT=0.
      DO 10 IQ=1,NFLAV
      IF (EMSCA.GT.2.*RMASS(IQ)) THEN
         MAXFL=MAXFL+1
         MAPQ(MAXFL)=IQ
         CALL HWUCFF(JL,IQ,Q2,CLQ(1,MAXFL))
         TQWT=TQWT+CLQ(1,MAXFL)
      ENDIF
  10  CONTINUE
      IF (MAXFL.EQ.0) CALL HWWARN('HWUEEC',100,*999)
  999 END
