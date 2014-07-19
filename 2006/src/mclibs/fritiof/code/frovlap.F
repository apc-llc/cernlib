*
* $Id: frovlap.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frovlap.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* FROVLAP ******************************
 
 
      SUBROUTINE FROVLAP
 
C...This subroutine determines the parameters of overlaping functions
C...for given cross sections SIGtot and SIGel.  Output is
C   AOP(3-4): OMEGA0,BETA ---- eikonal ovlap=1 - exp(-2Omega0*exp(-beta*b2))
C   AOP(5-6): GAUA,GAUB ---- Gaussian ovlap = 1-(1-gaexp(-gb*b2))**2
C   AOP(7-8) REC0,ALFA ---- gray disk = alfa  if b<REC0.
C   Fitting such that SIGtot = 2 integral d^b (1-sqrt(1-OVLAP));
C                       SIGinel =  integral d^b (OVLAP);
C...Units: AOP(4),AOP(6) are in Fm^-2; AOP(7) is in Fermi.
 
 
      PARAMETER (KSZ1=20,PI=3.1415926)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      dimension OMEG(2)
      SAVE /FRPARA1/,/FRINTN0/
 
      SGTOT = VFR(10)
      SGEL = VFR(11)
      SGTOT = SGTOT/0.389
      SGEL = SGEL/0.389
      SGINEL = SGTOT - SGEL
 
      RATIO = (SGINEL)/SGTOT
 
C............Eikonal parameters....................................
 
      OMEG(1) = 0.
      OMEG(2) = 2.
5      FOMEG = RATIO - 0.5*FRSUM(-2.*OMEG(2))/FRSUM(-OMEG(2))
      IF(ABS(FOMEG).LT.0.001) THEN
      OMEGTY = OMEG(2)
      GOTO 100
      ELSEIF(FOMEG.LT.0.) THEN
      OMEG(2) = OMEG(2) + 0.5
      GOTO 5
      ENDIF
 
      I = 0
10    I = I+1
 
      OMEGTY = (OMEG(1)+OMEG(2))/2.
      FOMEG = RATIO - 0.5*FRSUM(-2.*OMEGTY)/FRSUM(-OMEGTY)
      IF(ABS(FOMEG).LT.0.001) GOTO 100
      IF(FOMEG.LT.0.)  OMEG(1) = OMEGTY
      IF(FOMEG.GT.0.)  OMEG(2) = OMEGTY
 
      GOTO 10
 
100   BETA = (-2.*PI/SGTOT)* FRSUM(-OMEGTY)
      AOP(4) = BETA/(0.197)**2
 
      AOP(3) = OMEGTY
 
C............Gaussian parameters....................................
 
      AOP(5) = 4.*SGEL/SGTOT
      AOP(6) = 2.*PI*AOP(5)/SGTOT/(0.197)**2
 
C.............GRAY DISK PARAMETERS................................
 
      AOP(8) = 4.* (SGINEL)*SGEL/SGTOT**2
      REC02 = SGINEL/(PI*AOP(8))
      AOP(7) = SQRT(REC02) *0.197
 
      return
 
      END
