*
* $Id: frpsoft.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frpsoft.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRRINGO ***************************
 
 
C********************************* FRPSOFT *******************************
 
      SUBROUTINE FRPSOFT(I,PJK,DP,DN,KFEL)
 
C.....To generate momenta for the soft remnents
C.....  IQ=0: generated according to dQ/Q
C.....    =1: dQ/Q+k
C.....  kfel>0: no phase space for the collision
 
      PARAMETER (KSZ1=20,KSZ2=300)
      IMPLICIT DOUBLE PRECISION (D)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      DIMENSION DP(2,5),DN(2,5),PJK(2,4)
      SAVE /FRPARA1/,/FRINTN3/,/FRINTN0/,/FRINTN1/,/FRJETS/
 
      KFEL=0
      IK9=0
      IQ = 1
      IF(IOP(15).GE.2) IQ =0
      NU1 = NUC(1,I)
      NU2 = NUC(2,I)
 
      DWP = DP(1,4)+DP(2,4)+PPH(1,NU1,4)+PPH(2,NU2,4)
      DWM = DP(1,3)+DP(2,3)+PPH(1,NU1,3)+PPH(2,NU2,3)
      DA =(DN(1,1)+PPH(1,NU1,1)+DBLE(IQ*PJK(1,1)))**2+
     >    (DN(1,2)+PPH(1,NU1,2)+DBLE(IQ*PJK(1,2)))**2
      DB =(DN(2,1)+PPH(2,NU2,1)+DBLE(IQ*PJK(2,1)))**2+
     >    (DN(2,2)+PPH(2,NU2,2)+DBLE(IQ*PJK(2,2)))**2
      DA = DA + AOP(9)**2
      DB = DB + AOP(10)**2
 
10    CALL FRPLIMT(DWP,DWM,DA,DB,DPLO3,DPHI3,DPLO4,DPHI4,KFEL)
      IF(KFEL.GT.0) RETURN
 
      DPLO3 = DMAX1(DPLO3, DP(1,3)+PPH(1,NU1,3))
      DPLO4 = DMAX1(DPLO4, DP(2,4)+PPH(2,NU2,4))
      DPHI3 = DMIN1(DPHI3, DP(1,4)+PPH(1,NU1,4))
      DPHI4 = DMIN1(DPHI4, DP(2,3)+PPH(2,NU2,3))
      IF(IQ.EQ.1) THEN
      DPLO3 = DMAX1(DPLO3, DBLE(PJK(1,3)+PPH(1,NU1,3)) )
      DPLO4 = DMAX1(DPLO4, DBLE(PJK(2,4)+PPH(2,NU2,4)) )
      ENDIF
 
      P0 = 0.
 
      IF(rlu(0).LT.0.500) THEN
 
            IF(DPHI3.LT.DPLO3) GOTO 99
      P0 = -PPH(1,NU1,3)
      DN(1,3) = DFRDPQ(DPLO3,DPHI3,P0)
 
      DPLO4 = DMAX1(DPLO4,DB/(DWM-DN(1,3)))
      DPHI4 = DMIN1(DPHI4, DWP - DA/DN(1,3) )
            IF(DPHI4.LT.DPLO4) GOTO 99
      P0 = -PPH(2,NU2,4)
      DN(2,4) = DFRDPQ(DPLO4,DPHI4,P0)
 
      ELSE
 
            IF(DPHI4.LT.DPLO4) GOTO 99
      P0 = -PPH(2,NU2,4)
      DN(2,4) = DFRDPQ(DPLO4,DPHI4,P0)
 
      DPLO3 = DMAX1(DPLO3,DA/(DWP-DN(2,4)))
      DPHI3 = DMIN1(DPHI3, DWM - DB/DN(2,4) )
           IF(DPHI3.LT.DPLO3) GOTO 99
      P0 = -PPH(1,NU1,3)
      DN(1,3) = DFRDPQ(DPLO3,DPHI3,P0)
 
      ENDIF
 
      DN(1,4) = DWP - DN(2,4)
      DN(2,3) = DWM - DN(1,3)
 
      DO 70 L=1,2
      DO 70 LO=3,4
      DN(L,LO) = DN(L,LO) - DBLE(PPH(L,NUC(L,I),LO))
      IF(IQ.EQ.1) DN(L,LO) = DN(L,LO) - DBLE(PJK(L,LO))
        IF(DN(L,LO).LE.0.D0) THEN
      IK9=IK9+1
      IF(IK9.LT.10) GOTO 10
      KFEL=9
        ENDIF
70    CONTINUE
 
      GOTO 100
99    KFEL=2
100   RETURN
      END
