*
* $Id: hwhegx.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhegx.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHEGX.
*CMZ :-        -17/07/92  16.42.56  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHEGX
C     COMPUTES DIFFERENTIAL CROSS SECTION DSIGMA IN (Y,Q2,ETA,Z,PHI)
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION S,G,T,U,C1,C2,D1,D2,F1,F2,COSBET,WPROP,
     & D(4,4),C(4,4),QU,QD,QE,QW,PHOTON,EMWSQ,EMSSQ,CFAC,LEP,Y,
     & Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF(18),MFIN1(18),
     & MFIN2(18),RS,SMA,W2,RSHAT
      DOUBLE PRECISION TMAX,TMIN,A1,A2,B1,B2,I0,I1,I2,I3,I4,I5
      DOUBLE PRECISION MUSQ,MDSQ,ETA,Q1,COSTHE
      INTEGER IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,IPROO,I,J
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
C---INPUT VARIABLES
      IF (IERROR.NE.0) RETURN
      DSIGMA=0
      IF (IFLAVU.LE.12) THEN
        QU=QFCH(MOD(IFLAVU-1,6)+1)
        QD=QFCH(MOD(IFLAVD-1,6)+1)
        CFAC=CAFAC
      ELSE
        QU=QFCH(MOD(IFLAVU-1,6)+11)
        QD=QFCH(MOD(IFLAVD-1,6)+11)
        CFAC=1
      ENDIF
      QE=QFCH(11)
      QW=+1
      EMWSQ=RMASS(198)**2
      EMSCA=PHEP(5,3)
      EMSSQ=EMSCA**2
      MUSQ=RMASS(IFLAVU)**2
      MDSQ=RMASS(IFLAVD)**2
      ETA=(SHAT+Q2)/EMSSQ/Y
      IF (ETA.GT.ONE) RETURN
C---CALCULATE KINEMATIC TERMS
      G=0.5*(ETA*EMSSQ*Y-Q2) -0.5*(MUSQ+MDSQ)
      S=0.5*ETA*EMSSQ
      T=0.5*ETA*EMSSQ*(1-Y)
      U=0.5*Q2
      C1=0.5*ETA*EMSSQ*Y*Z
      C2=0.5*ETA*EMSSQ*Y*(1-Z)
      COSBET=(-ETA*EMSSQ*Y+Q2*(2-Y))/(Y*(ETA*EMSSQ-Q2))
      IF (SHAT.LE.(RMASS(IFLAVU)+RMASS(IFLAVD))**2) RETURN
      Q1=SQRT((SHAT**2+MUSQ**2+MDSQ**2
     &  -2*SHAT*MUSQ-2*SHAT*MDSQ-2*MUSQ*MDSQ)/SHAT**2)
      COSTHE=(1+(MDSQ-MUSQ)/SHAT-2*Z)/Q1
      IF (ABS(COSTHE).GE.1 .OR. ABS(COSBET).GE.1) RETURN
      D1=0.25*(ETA*EMSSQ-Q2)*(1+(MDSQ-MUSQ)/SHAT-Q1*
     &     (COSTHE*COSBET+SQRT((1-COSTHE**2)*(1-COSBET**2))*COS(PHI)))
      D2=S-U-D1
      F1=D1+C1-G            -MDSQ
      F2=U+T-F1
C---CALCULATE TRACE TERMS
      CALL HWVZRO(16,D)
      CALL HWVZRO(16,C)
      D(1,1)=2*F1*C2*S
      D(2,2)=2*C1*D2*T
      D(3,3)=-D1*(2*F2*G-D2*(F1+2*U))
     &       -D2*F1*(F2+U-D2+F1)
     &       +2*F1*F2*U
     &       -G*(-2*D1*(F1+F2+U)-F1*(D2+2*U)+2*D2*(U-F2)+2*U*(F2-U+G))
      D(4,4)=2*F1*C2*S
      D(1,2)=(D1+U-F2)*(D1*F2-F1*D2)-G*(D1*(F2+U)+U*(U-F2-G)+F1*D2)
      D(1,3)=D1*F2*(-2*F1+U-F2+D1)
     &      +F1*(F2*(D2-2*U)+F1*D2)
     &      +G*(-D1*(2*F1+F2+U)-F1*(D2+2*U)+U*(F2-U+G))
      D(1,4)=-2*F1*(D1+U)*(F2+G)
      D(2,3)=D1*(D2*(F1+2*(U-F2))+F2*(F2-U-D1))
     &      +F1*D2**2
     &      +G*(D1*(F2+U)+D2*(F1-2*(U-F2))+U*(U-F2-G))
      D(2,4)=-D1*F2*(U-F2+D1)
     &       -F1*D2*(U-D1-G-F2)
     &       -G*(U*(F2-U+G)-D1*(F2+U))
      D(3,4)=D1*(F1*(D2+2*F2)+F2*(F2-U-D1))
     &      +F1*(2*F2*U-D2*(U+F1))
     &      +G*(D1*(2*F1+F2+U)+U*(2*F1-F2+U-G))
C---REGULATE PROPAGATORS
      TMAX=EMSSQ-2*G
      TMIN=PHEP(5,2)**2
      A1=2*C1+MDSQ*(G+U)/G
      A2=2*C2+MUSQ*(G+U)/G
      B1=(2*U+MUSQ)/(2*G+2*U)
      B2=(2*U+MDSQ)/(2*G+2*U)
      I0=LOG(TMAX/TMIN)
      I1=1/A1*(I0-LOG((A1+B1*TMAX)/(A1+B1*TMIN)))
      I2=1/A2*(I0-LOG((A2+B2*TMAX)/(A2+B2*TMIN)))
      I3=(B1*I1-B2*I2)/(B1*A2-B2*A1)
      I4=1/A1*(I1+1/(A1+B1*TMAX)-1/(A1+B1*TMIN))
      I5=1/A2*(I2+1/(A2+B2*TMAX)-1/(A2+B2*TMIN))
      WPROP=1/((2*G-EMWSQ)**2+GAMW**2*EMWSQ)
C---CALCULATE COEFFICIENTS
      C(1,1)=    QU**2/(2*U+EMWSQ)**2                       *I5
      C(2,2)=    QD**2/(2*U+EMWSQ)**2                       *I4
      C(3,3)=    QW**2/(2*U+EMWSQ)**2    *WPROP             *I0
      C(4,4)=    QE**2/(2*S)**2          *WPROP             *I0
      C(1,2)=  2*QU*QD/(2*U+EMWSQ)**2                       *I3
      C(1,3)=  2*QW*QU/(2*U+EMWSQ)**2    *WPROP*(2*G-EMWSQ) *I2
      C(1,4)=  2*QU*QE/(2*S*(2*U+EMWSQ)) *WPROP*(2*G-EMWSQ) *I2
      C(2,3)=  2*QW*QD/(2*U+EMWSQ)**2    *WPROP*(2*G-EMWSQ) *I1
      C(2,4)=  2*QD*QE/(2*S*(2*U+EMWSQ)) *WPROP*(2*G-EMWSQ) *I1
      C(3,4)=  2*QW*QE/(2*S*(2*U+EMWSQ)) *WPROP             *I0
C---CALCULATE PHOTON STRUCTURE FUNCTION
      PHOTON=ALPHEM * (1+(1-ETA)**2) / (2*PIFAC*ETA)
C---SUM ALL TENSOR CONTRIBUTIONS
      DO 10 I=1,4
      DO 10 J=1,4
 10     DSIGMA=DSIGMA + C(I,J)*D(I,J)
C---CALCULATE TOTAL SUMMED AND AVERAGED MATRIX ELEMENT SQUARED
      DSIGMA = DSIGMA * 2*CFAC*(4*PIFAC*ALPHEM)**3/SWEIN**2
C---CALCULATE DIFFERENTIAL CROSS-SECTION
      DSIGMA = DSIGMA * GEV2NB*PHOTON/(512*PIFAC**4*ETA*EMSSQ)
 999  END
