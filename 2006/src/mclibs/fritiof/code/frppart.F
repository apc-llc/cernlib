*
* $Id: frppart.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frppart.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C******************************** END FRINSET ********************************
 
C****************************** FRPPART *********************************
 
      SUBROUTINE FRPPART(L,PPSR,DPV1,DPV2)
 
C........If a system has a lightcone-momenta PPSR(4), it is partitioned
C........into two momenta corresponding to the quark (DPV1) and diquark(DPV2)
C........end.  Note the input PPSR is light-cone: Px,Py,P-,P+, and the output
C........DPVs are normal 4-vector: Px,Py,Pz,E.
 
      PARAMETER (KSZ1=20,PI = 3.1415926)
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION PPSR(4),DPV1(4),DPV2(4)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      SAVE /FRPARA1/
 
      PPST2 = PPSR(1)**2+PPSR(2)**2
      WT2 = PPSR(3)*PPSR(4)
      SM = WT2 - PPST2
      GP2MX = 0.25* (SQRT(WT2)-SQRT(PPST2))**2
 
      NTRY= 0
10    NTRY=NTRY+1
      IF(NTRY.GT.200) CALL FRMGOUT(0,1,'NTRY runaway loop:',
     >      FLOAT(NTRY),0.,0.,0.,0.)
 
C.... ....TO GENERATE A GAUSSIAN PT FOR THE DIQUARK ..............
 
      CALL FRGAUSS(GP2, VFR(7), GP2MX)
        Gpt = FRSQR(GP2, 'PT2HGF')
        phi= 2.*PI*rlu(0)
        DPV2(1) = DBLE(Gpt*cos(phi))
        DPV2(2) = DBLE(Gpt*sin(phi))
 
      DPV1(1) = -DPV2(1) + DBLE(PPSR(1))
      DPV1(2) = -DPV2(2) + DBLE(PPSR(2))
 
C........DPV1 CORRES. TO THE QUARK END, DPV2 CORRES. TO THE DIQUARK END....
 
      DGPT1 = DPV1(1)**2 + DPV1(2)**2
      DGPT2 = DPV2(1)**2 + DPV2(2)**2
      DTM1 =DBLE(PPSR(3))+ (DGPT2-DGPT1)/DBLE(PPSR(4))
      DTM2=DTM1**2-4.D0*DGPT2*DBLE(PPSR(3))/DBLE(PPSR(4))
      IF(DTM2.LT.-0.1) CALL FRMGOUT(0,1,'CHECK DTM2',WT2,PPST2,
     >    REAL(DGPT1),REAL(DGPT2),GP2MX)
      DTM2=DFRSQR(DMAX1(0.D0,DTM2),'DTM2$')
 
      DGPV2M = (DTM1-(-1)**(L-1)*DTM2)/2.D0
      DGPV1M = DBLE(PPSR(3))-DGPV2M
      DGPV1P = DGPT1/DGPV1M
      DGPV2P = DBLE(PPSR(4))-DGPV1P
CC      DGPV1P = (DBLE(PPSR(4))*DGPV2M-DGPT2+DGPT1)/DBLE(PPSR(3))
 
      DPV1(3) = 0.5D0*(DGPV1P-DGPV1M)
      DPV1(4) = 0.5D0*(DGPV1P+DGPV1M)
      DPV2(3) = 0.5D0*(DGPV2P-DGPV2M)
      DPV2(4) = 0.5D0*(DGPV2P+DGPV2M)
 
      RETURN
      END
