*
* $Id: hwhiga.F,v 1.1.1.1 1996/03/08 17:02:15 mclareni Exp $
*
* $Log: hwhiga.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:15  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHIGA.
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ulrich Baur & Nigel Glover, adapted by Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWHIGA(S,T,U,EMH2,WTQQ,WTQG,WTGQ,WTGG)
C     Gives amplitudes squared for q-qbar, q(bar)-g and gg -> Higgs +jet
C     IAPHIG (set in HWIGIN)=0: zero mass approximation =1: exact result
C                           =2: infinite mass limit.
C     Only top loop included. A factor (alpha_s**3*alpha_W) is extracted
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER I
      LOGICAL NOMASS
      DOUBLE PRECISION S,T,U,EMH2,WTQQ,WTQG,WTGQ,WTGG,EMW2,RNGLU,RNQRK,
     & FLUXGG,FLUXGQ,FLUXQQ,EMQ2,TAMPI(7),TAMPR(7)
#if !defined(CERNLIB_IBM)
      DOUBLE COMPLEX HWHIGB,HWHIGC,HWHIGD,HWHIG5,HWHIG1,HWHIG2,
     & BI(4),CI(7),DI(3),EPSI,TAMP(7)
#endif
#if defined(CERNLIB_IBM)
      COMPLEX*16 HWHIGB,HWHIGC,HWHIGD,HWHIG5,HWHIG1,HWHIG2,
     & BI(4),CI(7),DI(3),EPSI,TAMP(7)
#endif
      EXTERNAL HWHIGB,HWHIGC,HWHIGD,HWHIG5,HWHIG1,HWHIG2
      COMMON/SMALL/EPSI
      COMMON/CINTS/BI,CI,DI
      EPSI=CMPLX(ZERO,-1.D-10)
      EMW2=RMASS(198)**2
C Spin and colour flux factors plus enhancement factor
      RNGLU=1./FLOAT(NCOLO**2-1)
      RNQRK=1./FLOAT(NCOLO)
      FLUXGG=.25*RNGLU**2*ENHANC(6)**2
      FLUXGQ=.25*RNGLU*RNQRK*ENHANC(6)**2
      FLUXQQ=.25*RNQRK**2*ENHANC(6)**2
      IF (IAPHIG.EQ.2) THEN
C Infinite mass limit in loops
         WTGG=2./3.*FLOAT(NCOLO*(NCOLO**2-1))
     &       *(EMH2**4+S**4+T**4+U**4)/(S*T*U*EMW2)*FLUXGG
         WTQQ= 16./9.*(U**2+T**2)/(S*EMW2)*FLUXQQ
         WTQG=-16./9.*(U**2+S**2)/(T*EMW2)*FLUXGQ
         WTGQ=-16./9.*(S**2+T**2)/(U*EMW2)*FLUXGQ
         RETURN
      ELSEIF (IAPHIG.EQ.1) THEN
C Exact result for loops
         NOMASS=.FALSE.
      ELSEIF (IAPHIG.EQ.0) THEN
C Small mass approximation in loops
         NOMASS=.TRUE.
      ELSE
         CALL HWWARN('HWHIGA',500,*999)
      ENDIF
C Include only top quark contribution
      EMQ2=RMASS(6)**2
      BI(1)=HWHIGB(NOMASS,S,0,0,EMQ2)
      BI(2)=HWHIGB(NOMASS,T,0,0,EMQ2)
      BI(3)=HWHIGB(NOMASS,U,0,0,EMQ2)
      BI(4)=HWHIGB(NOMASS,EMH2,0,0,EMQ2)
      BI(1)=BI(1)-BI(4)
      BI(2)=BI(2)-BI(4)
      BI(3)=BI(3)-BI(4)
      CI(1)=HWHIGC(NOMASS,S,0,0,EMQ2)
      CI(2)=HWHIGC(NOMASS,T,0,0,EMQ2)
      CI(3)=HWHIGC(NOMASS,U,0,0,EMQ2)
      CI(7)=HWHIGC(NOMASS,EMH2,0,0,EMQ2)
      CI(4)=(S*CI(1)-EMH2*CI(7))/(S-EMH2)
      CI(5)=(T*CI(2)-EMH2*CI(7))/(T-EMH2)
      CI(6)=(U*CI(3)-EMH2*CI(7))/(U-EMH2)
      DI(1)=HWHIGD(NOMASS,U,T,EMH2,EMQ2)
      DI(2)=HWHIGD(NOMASS,S,U,EMH2,EMQ2)
      DI(3)=HWHIGD(NOMASS,S,T,EMH2,EMQ2)
C Compute complex amplitudes
      TAMP(1)=HWHIG1(S,T,U,EMH2,EMQ2,1,2,3,4,5,6)
      TAMP(2)=HWHIG2(S,T,U,EMH2,EMQ2,1,2,3,0,0,0)
      TAMP(3)=HWHIG1(T,S,U,EMH2,EMQ2,2,1,3,5,4,6)
      TAMP(4)=HWHIG1(U,T,S,EMH2,EMQ2,3,2,1,6,5,4)
      TAMP(5)=HWHIG5(S,T,U,EMH2,EMQ2,1,0,4,0,0,0)
      TAMP(6)=HWHIG5(T,S,U,EMH2,EMQ2,2,0,5,0,0,0)
      TAMP(7)=HWHIG5(U,T,S,EMH2,EMQ2,3,0,6,0,0,0)
      DO 20 I=1,7
      TAMPI(I)= REAL(TAMP(I))
  20  TAMPR(I)=-IMAG(TAMP(I))
C Square and add prefactors
      WTGG=0.03125*FLOAT(NCOLO*(NCOLO**2-1))
     &    *(TAMPR(1)**2+TAMPI(1)**2+TAMPR(2)**2+TAMPI(2)**2
     &     +TAMPR(3)**2+TAMPI(3)**2+TAMPR(4)**2+TAMPI(4)**2)*FLUXGG
      WTQQ= 16.*(U**2+T**2)/(U+T)**2*EMQ2**2/(S*EMW2)
     &     *(TAMPR(5)**2+TAMPI(5)**2)*FLUXQQ
      WTQG=-16.*(U**2+S**2)/(U+S)**2*EMQ2**2/(T*EMW2)
     &     *(TAMPR(6)**2+TAMPI(6)**2)*FLUXGQ
      WTGQ=-16.*(S**2+T**2)/(S+T)**2*EMQ2**2/(U*EMW2)
     &     *(TAMPR(7)**2+TAMPI(7)**2)*FLUXGQ
 999  RETURN
      END
