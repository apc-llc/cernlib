*
* $Id: frhardp.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frhardp.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRQPROB **************************
 
 
C********************************* FRHARDP *******************************
 
      SUBROUTINE FRHARDP(KFI,KFT,W,IHAV,IQ)
 
C....GIVEN PARTICLE KF CODES FOR THE PROJECTILE KFI AND THE TARGET KFT,
C....AND TOTAL CMS ENERGY W, THIS ROUTINE WILL GENERATE PARTON-PARTON
C....PROCESSES (INCLUDING QCD 2->2 PROCESSES, VECTOR BOSONS OR HIGGS
C....PRODUCTIONS, HEAVY QUARKS ECT).  THE GENERATED COLORED OBJECTS
C....ARE TRANSFERED FROM LUJETS TO BLOCK FRJETS, AND THE COLOR-NEUTRAL
C....PARTICLES ARE STORED IN BLOCK FRCNUT. AFTERWARDS THE N IN LUJETS
C....IS RESET TO ZERO.
C......Ihav=0 - non-hard events; Ihav=1, event containing hard process.
C......IQ=-1 OR 1: PYINIT - PYTHIA INITIALIZATION IS MADE;
C......IQ=OTHERS: NO PYTHIA INITIALIZATION
C......FOR IQ<0, Only PYEVENT IS CALLED, FREDIPY will not be callled.
 
      PARAMETER (KSZJ=4000,KSZ1=20)
      CHARACTER*6 BEAM(2), PARCDE7(15)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/FRPICKJ/NH,KP(100,5),PP(100,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
 
      COMMON/FRTOPYXTO/KF0(2)
 
      DIMENSION KCD7(15),KF(2),IKF(2)
      SAVE BEAM
      SAVE /LUJETS/,/PYSUBS/,/PYPARS/,/FRINTN0/,/FRPARA1/,/FRCNUT/,
     >     /FRJETS/,/FRPICKJ/,/FRTOPYXTO/,/LUDAT1/
 
C....The following is a list of particles available in PYTHIA 5.5.
C....Note for generating hard scattering, all mesons not on the list
C....will be reduced to corresponding pions, and baryons to protons or
C....neutrons. Obviously this can only be a good approximation
C....at high energies when the gluon distribution dominents.
 
        DATA PARCDE7/
     >  'p+    ','p~-   ','n0    ','n~0   ','pi+   ','pi-   ',
     >  'e-    ','e+    ','nu_e  ','nu_e~ ','mu-   ','mu+   ',
     >  'nu_mu ','nu_mu~','gamma '/
        DATA KCD7/2212,-2212,2112,-2112,211,-211,
     >            11,  -11,  12,  -12,  13, -13,
     >            14,  -14,  22/
 
      N=0
C...            Identify the beam particles:
      KF(1) = KFI
      KF(2) = KFT
 
      DO 20 L=1, 2
                     IF( KF(L).NE.KF0(L) ) THEN
      KF0(L) = KF(L)
      IKF(L) = 0
 
 5    DO 12 I=1, 15
      IF(KF(L).EQ.KCD7(I)) THEN
      BEAM(L) = PARCDE7(I)
      IF(IKF(L).EQ.0) IKF(L) = 1
      GO TO 14
      ENDIF
12    CONTINUE
14      IF(IKF(L).EQ.0) THEN
        KFV = IABS(KF(L))
           IF(KFV.GT.1000.AND.KFV.LT.9999) THEN
        KCG=0
        K1 = KFV/1000
          K1R = 2 - MOD(K1,2)
        K2 = (KFV-K1*1000)/100
          K2R = 2 - MOD(K2,2)
        K3 = (KFV-K1*1000-K2*100)/10
          K3R = 2 - MOD(K3,2)
        KLG= MAX(K1R,MAX(K2R,K3R))
        KSM= MIN(K1R,MIN(K2R,K3R))
        KME= K1R+K2R+K3R -KLG-KSM
        KFVR= 1000*KLG+ 100*KME+ 10*KSM+ 2
        IF(KFVR.NE.2212.AND.KFVR.NE.2112) KFVR= 2212
      KF(L) = KFVR *(KF(L)/KFV)
           ELSEIF(KFV.GT.100.AND.KFV.LT.999) THEN
        K1= KFV/100
        K2= (KFV-K1*100)/10
        K3= (KFV-K1*100-K2*10)
        K1R= 2 - MOD(K1,2)
        K2R= 2 - MOD(K2,2)
        KF(L)=(MAX(K1R,K2R)*100+MIN(K2R,K1R)*10+K3)*(-1)**MAX(K1R,K2R)
        IF(K1R.LT.K2R) KF(L)= -KF(L)
      IF(ABS(KF(L)).NE.211) KF(L) = (KF(L)/IABS(KF(L)))*211
           ENDIF
        IKF(L) = -1
        GOTO 5
        ENDIF
 
      IF(IKF(L).EQ.0) THEN
        CALL FRMGOUT(0,1,'PYTHIA unrecognized particle',
     >        FLOAT(L),FLOAT(KF(1)),FLOAT(KF(2)),0.,0.)
      ELSEIF(IKF(L).EQ.-1) THEN
        WRITE(MSTU(11),1010)
        IF(L.EQ.1) WRITE(MSTU(11),1012) 'The projectile',
     >                     KF0(L), BEAM(L),KF(L)
        IF(L.EQ.2) WRITE(MSTU(11),1012) 'The target    ',
     >                     KF0(L), BEAM(L),KF(L)
      ENDIF
                     ENDIF
20    CONTINUE
 
C................................
      NJ = 0
      NH = 0
      MSTI(2) = 0
      MSTI(3) = 0
      MSTI(31) = 0
      IF(ABS(IQ).EQ.1) CALL FRPYINI('USER',BEAM(1),BEAM(2),W)
      CALL PYEVNT
 
      IF(MSTI(1).GE.91.AND.MSTI(1).LE.95) THEN
      IHAV = 0
      ELSE
      IHAV = 1
      ENDIF
 
      if(IQ.LT.0.OR.IHAV.EQ.0) RETURN
 
      CALL FREDIPY
 
      DO 200 LO = 1, NH
      NJ = NJ+1
200   CALL FRVECRC(NJ,LO,1)
 
1010  FORMAT(/,6x,
     >'For the purpose of interfacing with PYTHIA:')
1012  FORMAT(8x,A14,' (code=',I5,')',' is treated as ',A6,
     >       '(code=',I5,')',/ )
 
      RETURN
      END
