*
* $Id: fratleo.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: fratleo.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRANGUR **************************
 
C********************************* FRATLEO ******************************
 
      SUBROUTINE FRATLEO(L,J)
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300,MAXSTR=100)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/ARDAT1/PARA(40),MSTA(40)
      COMMON/ARSTRS/IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
 
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN2/NHP(2),IHQP(2,KSZ2),KHP(2,KSZ2,100,5),
     >               PHP(2,KSZ2,100,5)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRINTN4/KFEND(2,KSZ2,2)
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
 
      COMMON/FRATLE1/NA1,KA1(KSZ2,5),PA1(KSZ2,5)
 
      DIMENSION DPV1(4),DPV2(4),PPSR(4),RFA(2,2),XPQ(-25:25)
     >        ,PGL(2)
 
      SAVE RFA, J1M,IFL1M,IFL2M
      SAVE /LUJETS/,/LUDAT1/,/PYPARS/,/ARDAT1/,/ARSTRS/,/FRPARA1/,
     >   /FRINTN0/,/FRINTN1/,/FRINTN2/,/FRINTN3/,/FRINTN4/,
     >   /FRJETS/,/FRATLE1/
 
C-----------------------------------------------------------------------
C to set parton codes and momenta for the J-th hadron string, with ends
C flavours IFL1 and IFL2.  Calls ARIADNE for dipole shower.
C For baryons, IFL2 always corresponds to the diquark end.
C This routine is entered twice (for each collision), once in test mode
C    and once for the final gluon radiation.
C
C Codes used for partons:
C   K(J,5)=111 for a hard valence quark and its accompaning gluon kink;
C         =221 for a sea quark that has converted into a gluon.
C         =222 for the soft gluon kink accompaning a hard gluon.
C         =100 for the hard gluon.
C
C L =1 FOR PROJECTILE; AND L=2 FOR TARGET.
C    IOP(15)=0 normal mode, arrange the partons and do bremsstrahlung
C    IOP(15)=1 test mode, look for valence quarks and testing if RPS is
C              drowned. Output flag:
C           =1+L: gluon on L drowned;
C PHP(L,J,K,5) will be used to memorize the fractions.
C KHP(L,J,1,5)=I   labels the valence quark
C-----------------------------------------------------------------------
 
      IF(L.EQ.1) J1M= 0
                 J2M= 0
      IFL1= KFEND(L,J,1)
      IFL2= KFEND(L,J,2)
 
      IF(IOP(15).GT.0) THEN
      IFL2A = IFL2
      IFL2B = 0
      SPIN0 = 0.
      IF(ABS(IFL2).GT.1000) THEN
      IFL2A = (IFL2/ABS(IFL2))* (ABS(IFL2)/1000)
      IFL2B = (IFL2/ABS(IFL2))* ((ABS(IFL2)-ABS(IFL2A)*1000)/100)
      SPIN0 = FLOAT((ABS(IFL2)-ABS(IFL2A)*1000-ABS(IFL2B)*100)/2)
      ENDIF
        ENDIF
 
      IQQK = 0
      IQGL = 0
      NA1 = 0
 
      IF(IHQP(L,J).LE.0) GOTO 150
 
      I0 = KHP(L,J,IHQP(L,J),4)+1
 
      DO 1000 I = I0, IHQP(L,J)
 
      KHP(L,J,I,4) = 0
 
C   Check if a quark is possibly a valence:
C   The processes in which quark flavorS changed are treated as sea quarks:
C   Note since PYTHIA only has pions and protons, only u,d can be valence.
 
      IF(IOP(15).GT.0.AND.NJ.GT.0
     >   .AND.KHP(L,J,1,5).eq.0.AND.KHP(L,J,I,2).NE.21
     >   .AND.(MSTI(1).NE.12.AND.MSTI(1).NE.53) ) THEN
 
      IU = KHP(L,J,I,2)
      IUSN = (IU/IABS(IU))
 
        IF(IU.EQ.IFL1.OR.IUSN*(IABS(IU)+2).EQ.IFL1.OR.
     >     IU.EQ.IFL2A.OR.IUSN*(IABS(IU)+2).EQ.IFL2A.OR.
     >     IU.EQ.IFL2B.OR.IUSN*(IABS(IU)+2).EQ.IFL2B) THEN
C                          weighted by the structure functions
        CALL PYSTFU(MSTI(11+L-1),PARI(33+L-1),PARI(21)**2,XPQ)
        RVAL = ABS(XPQ(IU)-XPQ(-IU))/MAX(XPQ(IU),XPQ(-IU))
 
          IF(RLU(0).LE.RVAL) THEN
          KHP(L,J,1,5) = I
           IF(IU.EQ.IFL1.OR.IUSN*(IABS(IU)+2).EQ.IFL1) THEN
             KHP(L,J,I,2) = IFL1
           ELSEIF(IU.EQ.IFL2A.OR.IUSN*(IABS(IU)+2).EQ.IFL2A) THEN
             KHP(L,J,I,2) = IFL2A
             IF(IFL2B.EQ.0) THEN
               IFL2 = IFL1
               IFL1 = IFL2A
             ELSE
               SPIN = SPIN0
               IF(IFL1.EQ.IFL2B) SPIN = 1.
               IFL2 = IFRKFC(IFL1,IFL2B,0,SPIN)
               IFL1 = IFL2A
           ENDIF
           ELSEIF(IU.EQ.IFL2B.OR.IUSN*(IABS(IU)+2).EQ.IFL2B) THEN
             KHP(L,J,I,2) = IFL2B
             SPIN = SPIN0
             IF(IFL1.EQ.IFL2A) SPIN = 1.
             IFL2 = IFRKFC(IFL1,IFL2A,0,SPIN)
             IFL1 = IFL2B
           ELSE
             KHP(L,J,1,5) = 0
           ENDIF
          ENDIF
        ENDIF
      ENDIF
 
      IF(I.EQ.KHP(L,J,1,5).AND.IQQK.EQ.0) THEN
      DO 131 LO=1,4
      KA1(1,LO) = KHP(L,J,I,LO)
131   PA1(1,LO) = PHP(L,J,I,LO)
      PA1(1,5) = 0.0
        KA1(1,1)= 2
        KA1(1,3)= 0
        KA1(1,4)= 0
        KA1(1,5)= 111
      IQQK=1
 
C..       Save the end configurations:
        IF(IOP(15).GT.0) THEN
          IF(L.EQ.1) THEN
          J1M= J
          IFL1M= IFL1
          IFL2M= IFL2
          ELSE
          J2M= J
        ENDIF
      ENDIF
 
      ELSE
C         GLUON entry started at NA1=2
      NA1 = MAX(NA1+1,2)
      DO 138 LO=1,4
      PA1(NA1,LO) = PHP(L,J,I,LO)
138   KA1(NA1,LO) = KHP(L,J,I,LO)
      PA1(NA1,5) = 0.0
      KA1(NA1,1)=2
      KA1(NA1,5)=0
      IF(KA1(NA1,2).NE.21) KA1(NA1,5) = 221
      KA1(NA1,2)=21
      KA1(NA1,3)=0
      KA1(NA1,4)=0
      KA1(NA1,5)=0
      IQGL= IQGL + 1
 
      ENDIF
 
1000  CONTINUE
 
 
C...ORDER THE GLUONS ACCORDING TO PT..............
      IF(NA1.GT.2) CALL FRORDER(L,2,NA1)
 
C...DIPOLE RADIATION.......................
 
150   IOP(17) = N+1
 
      IF(IQQK.EQ.1) THEN
      N = N+1
      DO 190 LO=1,5
      K(N,LO) = KA1(1,LO)
190   P(N,LO) = PA1(1,LO)
      P(N,5) = 0.0
      P(N,4) = SQRT(P(N,1)**2+P(N,2)**2+P(N,3)**2)
      ENDIF
 
C...............Set up the two fractions:
 
      RFA(L,1) = 1.0
      IF(KFR(10).NE.0.AND.IQGL.GT.0) THEN
        IF(PHP(L,J,1,5).GT.0) THEN
        RFA(L,1) = PHP(L,J,1,5)
        ELSE
          IF(KFR(10).EQ.1) THEN
          RFA(L,1) = VFR(16)
          ELSEIF(KFR(10).EQ.2) THEN
          RFA(L,1) = RLU(0)
          ENDIF
        PHP(L,J,1,5)= RFA(L,1)
        ENDIF
      ENDIF
      IF(RFA(L,1).GT.1.0) CALL FRMGOUT(0,0,'VFR(16)>1 NOT ALLOWED!',
     >   VFR(16),RFA(L,1),0.,0.,0.)
 
      RFA(L,2) = 1.0
      IF(KFR(8).GE.1.AND.IQGL.GT.0) THEN
        IF(PHP(L,J,2,5).GT.0) THEN
        RFA(L,2) = PHP(L,J,2,5)
        ELSE
      PGL(1) = PA1(IQGL+1,4)-PA1(IQGL+1,3)
      PGL(2) = PA1(IQGL+1,4)+PA1(IQGL+1,3)
      AP = SQRT( PGL(L)/PPSY(L,J,2+L))
C       Rarely PGL can become zero due to inaccuracy when Pz is very large.
      RKK = 0.
      RKKMX= 1.0-(PPS(L,J,1)**2+PPS(L,J,2)**2)/(PPS(L,J,3)*PPS(L,J,4))
      RKKMX = 0.99*RKKMX
593   IF(AP.GT.0.) RKK=AP*((RKKMX+AP)/AP)**RLU(0)-AP
      IF(RKK.LE.0..OR.RKK.GE.RKKMX) GOTO 593
      RFA(L,2) = 1.- RKK
      PHP(L,J,2,5)= RFA(L,2)
        ENDIF
      ENDIF
 
      DO 588 LO=1,4
588   PPSR(LO) = PPS(L,J,LO)
 
C...for the kink:
      IF(RFA(L,2).GT.0.and.RFA(L,2).LT.1.0) THEN
      PGL(L) = (1.-RFA(L,2))* PPS(L,J,2+L)
      PA1(1,1)= 0.0
      PA1(1,2)= 0.0
      PGL(3-L) = 0.0
      PA1(1,3)=0.5*(PGL(2)-PGL(1))
      PA1(1,4)=0.5*(PGL(2)+PGL(1))
      KA1(1,1)=2
      KA1(1,2)=21
      KA1(1,4)=3
      KA1(1,5)=222
      PPSR(3) = PPSR(3) - PGL(1)
      PPSR(4) = PPSR(4) - PGL(2)
      ENDIF
 
590   CALL FRPPART(L,PPSR,DPV1,DPV2)
 
      N = N+1
      DO 599 LO=1,4
599   P(N,LO) = DPV1(LO)
      P(N,5) = 0.0
      P(N,4) = SQRT(P(N,1)**2+P(N,2)**2+P(N,3)**2)
      K(N,1) = 2
      K(N,3) = 0
      K(N,4) = 1
      IF(IQQK.EQ.0) THEN
      K(N,2) = IFL1
      K(N,5) = 0
      ELSEIF(IQQK.EQ.1) THEN
      K(N,2) = 21
      K(N,5) = 111
      ENDIF
 
      N = N+1
      DO 600 LO=1,4
600   P(N,LO) = DPV2(LO)
      P(N,5) = 0.0
      P(N,4) = SQRT(P(N,1)**2+P(N,2)**2+P(N,3)**2)
      K(N,1) = 1
      K(N,2) = IFL2
      K(N,3) = 0
      K(N,4) = 2
      K(N,5) = 0
 
C...  Check mass and remove negative mass arising from numerical
C...  imprecisions.
      RMS0= ppsr(3)*ppsr(4)- ppsr(1)**2-ppsr(2)**2
      JN=N
      IF(P(N-1,4).LT.P(N,4)) JN=N-1
      PP2=(P(N,3)+P(N-1,3))**2+(P(N,2)+P(N-1,2))**2
     >     +(P(N,1)+P(N-1,1))**2
      XRMS20=(P(N,4)+P(N-1,4))**2-PP2 -RMS0
 
      IF(XRMS20.LT.0.) THEN
      XADD0=0.0
      XADD1=0.0
700   XADD1=XADD1+0.1
      PJNV= P(JN,4)+XADD1
      XRMS2=(P(2*N-1-JN,4)+PJNV)**2-PP2-RMS0
      IF(XRMS2*XRMS20.GT.0.) GOTO 700
 
      NTRY=0
710   XADD= (XADD0+XADD1)/2.0
       XRMS2M=XRMS2
       PJNV= P(JN,4)+XADD
       XRMS2=(P(2*N-1-JN,4)+PJNV)**2-PP2-RMS0
       IF(XRMS2*XRMS20.GT.0.) THEN
        XADD0=XADD
       ELSE
        XADD1=XADD
       ENDIF
       IF(XRMS2.EQ.XRMS2M) THEN
         NTRY=NTRY+1
       ELSE
         NTRY=0
       ENDIF
 
       IF(NTRY.GE.5) THEN
         IF(XRMS2.LE.-0.5*RMS0) XADD=XADD1
         GOTO 720
       ENDIF
 
       GOTO 710
 
720    P(JN,4) = P(JN,4)+ XADD
      ENDIF
 
      MSTA(11) = 0
 
C..Test hard partons against bremsstrahlung
      IF(KFR(9).NE.0.AND.IOP(15).EQ.1.AND.IQGL+IQQK.GT.0) THEN
        IF(IQGL.GT.0.AND.RFA(L,1).LT.1.0) THEN
        PARA(11) = VFR(7+L)/(RFA(L,1))
        PARA(12) = VFR(7+L)/(1.-RFA(L,1))
        ENDIF
        CALL FRTESTG(L,IQQK,IQGL,IOK,RFA)
        IF(IOK.EQ.0) THEN
        IOP(15) = IOP(15) +L
          IF(J1M.GT.0) KHP(1,J1M,1,5)= 0
          IF(J2M.GT.0) KHP(2,J2M,1,5)= 0
        GOTO 999
        ELSEIF(L.EQ.2.AND.IQQK+J1M.GT.0) THEN
C..            If valence quarks survived, keep the new ends:
        KFEND(L,J,1) = IFL1
        KFEND(L,J,2) = IFL2
          IF(J1M.GT.0) THEN
        KFEND(1,J1M,1) = IFL1M
        KFEND(1,J1M,2) = IFL2M
          ENDIF
        ENDIF
      ENDIF
 
C.. Insert hard gluons one by one, and do emission.  The Pt of emission is
C.. restricted such that   Pt_next gluon < Pt < Previous emmision.
 
                         IF(IOP(15).EQ.0) THEN
      PARA(11) = VFR(7+L)
      PARA(12) = PARA(11)
      MSTA(11) = 0
      IF(RFA(L,1).LT.1.0) THEN
       PARA(11) = VFR(7+L)/(RFA(L,1))
       PARA(12) = VFR(7+L)/(1.-RFA(L,1))
      ENDIF
 
      IARI = 0
      PARA3=PARA(3)
      PARA6=PARA(6)
      DO 900 I = 1, MAX(IQGL,1)
 
        INUP= 0
        IF(I.EQ.1) INUP= 0
        CALL FRMXGPT(IOP(17),INUP,IMX,VRPTNX,1)
        IF(IMX.GT.0) THEN
          CALL FRINSET(IMX,IOP(17),N,NOG,1)
          IF(I.EQ.1.AND.KFR(8).GT.0) THEN
            CALL FRINSKK(RFA(L,2),NKK)
            IF(NKK.GT.0) THEN
            PARA(13) = PARA(11)/(1.-RFA(L,2))
            PARA(11) = PARA(11)/RFA(L,2)
            ENDIF
          ENDIF
        ENDIF
 
        IF(KFR(2).EQ.1) THEN
        PARA(3) = MAX(PARA3,VRPTNX)
        IF(IARI.GT.0) PARA(6) = FRSQR(PT2LST,'PT2LSTAR')
        NMEM = N
        CALL FRARIAD
        IARI= N-NMEM
        ENDIF
 
900   CONTINUE
      PARA(3)=PARA3
      PARA(6)=PARA6
                           ENDIF
 
999   RETURN
      END
