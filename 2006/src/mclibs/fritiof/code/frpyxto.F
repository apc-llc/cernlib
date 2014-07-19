*
* $Id: frpyxto.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frpyxto.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE FRPYXTO
 
C...This routine is borrowed from PYTHIA's PYXTOT.  Modifications:
C... The Block and Cahn fit No.2 for slope parameters are changed according
C    to their paper in Physics Simulations at High Energy
C    (edited by V.Barger,etc), which gives better fit.
C...  MSTU(31)=6: Block-Cahn fit 8 for total xsection, and fit 1 for slope;
C...  The pion scaling factor is slightly modified to better accomodate
C...  the low energy data.
C...  Kaons added.
C
C...Parametrizes total, double diffractive, single diffractive and
C...elastic cross-sections for different energies and beams.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
 
      COMMON/FRTOPYXTO/KF0(2)
 
      SAVE /LUDAT1/,/PYPARS/,/PYINT1/,/PYINT5/,/FRTOPYXTO/
      DIMENSION BCS(5,8),BCB(2,5),BCC(3),SCALE(2)
 
C...The following data lines are coefficients needed in the
C...Block, Cahn parametrization of total cross-section and nuclear
C...slope parameter; see below.
      DATA ((BCS(I,J),J=1,8),I=1,5)/
     1 41.74, 0.66, 0.0000, 337.,  0.0, 0.0, -39.3, 0.48,
     2 41.66, 0.60, 0.0000, 306.,  0.0, 0.0, -34.6, 0.51,
     3 41.36, 0.63, 0.0000, 299.,  7.3, 0.5, -40.4, 0.47,
     4 41.68, 0.63, 0.0083, 330.,  0.0, 0.0, -39.0, 0.48,
     5 41.13, 0.59, 0.0074, 278., 10.5, 0.5, -41.2, 0.46/
      DATA ((BCB(I,J),J=1,5),I=1,2)/
     1 10.79, -0.049, 0.040, 21.5, 1.23,
     2  9.92, 0.27, 0.013, 18.9, 1.93/
CC   2  9.92, -0.027, 0.013, 18.9, 1.07/
      DATA BCC/2.0164346,-0.5590311,0.0376279/
 
C...Total cross-section and nuclear slope parameter for pp and p-pbar
      NFIT=MIN(5,MAX(1,MSTP(31)))
      SIGP=BCS(NFIT,1)+BCS(NFIT,2)*(-0.25*PARU(1)**2*
     &(1.-0.25*BCS(NFIT,3)*PARU(1)**2)+(1.+0.5*BCS(NFIT,3)*PARU(1)**2)*
     &(LOG(VINT(2)/BCS(NFIT,4)))**2+BCS(NFIT,3)*
     &(LOG(VINT(2)/BCS(NFIT,4)))**4)/
     &((1.-0.25*BCS(NFIT,3)*PARU(1)**2)**2+2.*BCS(NFIT,3)*
     &(1.+0.25*BCS(NFIT,3)*PARU(1)**2)*(LOG(VINT(2)/BCS(NFIT,4)))**2+
     &BCS(NFIT,3)**2*(LOG(VINT(2)/BCS(NFIT,4)))**4)+BCS(NFIT,5)*
     &VINT(2)**(BCS(NFIT,6)-1.)*SIN(0.5*PARU(1)*BCS(NFIT,6))
      SIGM=-BCS(NFIT,7)*VINT(2)**(BCS(NFIT,8)-1.)*
     &COS(0.5*PARU(1)*BCS(NFIT,8))
      REFP=BCS(NFIT,2)*PARU(1)*LOG(VINT(2)/BCS(NFIT,4))/
     &((1.-0.25*BCS(NFIT,3)*PARU(1)**2)**2+2.*BCS(NFIT,3)*
     &(1.+0.25*BCS(NFIT,3)*PARU(1)**2)+(LOG(VINT(2)/BCS(NFIT,4)))**2+
     &BCS(NFIT,3)**2*(LOG(VINT(2)/BCS(NFIT,4)))**4)-BCS(NFIT,5)*
     &VINT(2)**(BCS(NFIT,6)-1.)*COS(0.5*PARU(1)*BCS(NFIT,6))
      REFM=-BCS(NFIT,7)*VINT(2)**(BCS(NFIT,8)-1.)*
     &SIN(0.5*PARU(1)*BCS(NFIT,8))
      SIGMA=SIGP-ISIGN(1,MINT(11)*MINT(12))*SIGM
      RHO=(REFP-ISIGN(1,MINT(11)*MINT(12))*REFM)/SIGMA
 
C...Nuclear slope parameter B, curvature C:
      NFIT=1
      IF(MSTP(31).GE.4.and.MSTP(31).LE.5) NFIT=2
      BP=BCB(NFIT,1)+BCB(NFIT,2)*LOG(VINT(2))+
     &BCB(NFIT,3)*(LOG(VINT(2)))**2
      BM=BCB(NFIT,4)+BCB(NFIT,5)*LOG(VINT(2))
      B=BP-ISIGN(1,MINT(11)*MINT(12))*SIGM/SIGP*(BM-BP)
      VINT(121)=B
      C=-0.5*BCC(2)/BCC(3)*(1.-SQRT(MAX(0.,1.+4.*BCC(3)/BCC(2)**2*
     &(1.E-03*VINT(1)-BCC(1)))))
      VINT(122)=C
 
C...Elastic scattering cross-section (fixed by sigma-tot, rho and B).
      SIGEL=SIGMA**2*(1.+RHO**2)/(16.*PARU(1)*PARU(5)*B)
 
C...Single diffractive scattering cross-section from Goulianos:
      SIGSD=2.*0.68*(1.+36./VINT(2))*LOG(0.6+0.1*VINT(2))
 
C...Double diffractive scattering cross-section (essentially fixed by
C...sigma-sd and sigma-el).
      SIGDD=SIGSD**2/(3.*SIGEL)
 
C...Total non-elastic, non-diffractive cross-section.
      SIGND=SIGMA-SIGDD-SIGSD-SIGEL
 
C...Rescale for MESONS.
      SCALPI = (2./3.-1.13/VINT(1))
      SCALK = (2./3.-3.27/VINT(1))
 
      DO 110 LO = 1, 2
      IF(IABS(KF0(LO)).EQ.321) THEN
      SCALE(LO) = SCALK
      ELSEIF(IABS(KF0(LO)).LE.999) THEN
      SCALE(LO) = SCALPI
      ELSE
      SCALE(LO) = 1.0
      ENDIF
110   CONTINUE
 
        SIGMA=SIGMA *SCALE(1)*SCALE(2)
        SIGDD=SIGDD *SCALE(1)*SCALE(2)
        SIGSD=SIGSD *SCALE(1)*SCALE(2)
        SIGEL=SIGEL *SCALE(1)*SCALE(2)
        SIGND=SIGND *SCALE(1)*SCALE(2)
 
C...Save cross-sections in common block PYPARA.
      VINT(101)=SIGMA
      VINT(102)=SIGEL
      VINT(103)=SIGSD
      VINT(104)=SIGDD
      VINT(106)=SIGND
      XSEC(95,1)=SIGND
 
      RETURN
      END
