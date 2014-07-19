 
C*********************************************************************
 
C...PYSTRF
C...Handles the fragmentation of an arbitrary colour singlet
C...jet system according to the Lund string fragmentation model.
 
      SUBROUTINE PYSTRF(IP)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /PYJETS/,/PYDAT1/,/PYDAT2/
C...Local arrays. All MOPS variables ends with MO
      DIMENSION DPS(5),KFL(3),PMQ(3),PX(3),PY(3),GAM(3),IE(2),PR(2),
     &IN(9),DHM(4),DHG(4),DP(5,5),IRANK(2),MJU(4),IJU(3),PJU(5,5),
     &TJU(5),KFJH(2),NJS(2),KFJS(2),PJS(4,5),MSTU9T(8),PARU9T(8),
     &INMO(9),PM2QMO(2),XTMO(2),EJSTR(2)
 
C...Function: four-product of two vectors.
      FOUR(I,J)=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)
      DFOUR(I,J)=DP(I,4)*DP(J,4)-DP(I,1)*DP(J,1)-DP(I,2)*DP(J,2)-
     &DP(I,3)*DP(J,3)
 
C...Reset counters.
      MSTJ(91)=0
      NSAV=N
      MSTU90=MSTU(90)
      NP=0
      KQSUM=0
      DO 100 J=1,5
        DPS(J)=0D0
  100 CONTINUE
      MJU(1)=0
      MJU(2)=0
      NTRYFN=0
 
C...Identify parton system.
      I=IP-1
  110 I=I+1
      IF(I.GT.MIN(N,MSTU(4)-MSTU(32))) THEN
        CALL PYERRM(12,'(PYSTRF:) failed to reconstruct jet system')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      IF(K(I,1).NE.1.AND.K(I,1).NE.2.AND.K(I,1).NE.41) GOTO 110
      KC=PYCOMP(K(I,2))
      IF(KC.EQ.0) GOTO 110
      KQ=KCHG(KC,2)*ISIGN(1,K(I,2))
      IF(KQ.EQ.0.AND.K(I,1).NE.41) GOTO 110
      IF(N+5*NP+11.GT.MSTU(4)-MSTU(32)-5) THEN
        CALL PYERRM(11,'(PYSTRF:) no more memory left in PYJETS')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
 
C...Take copy of partons to be considered. Check flavour sum.
      NP=NP+1
      DO 120 J=1,5
        K(N+NP,J)=K(I,J)
        P(N+NP,J)=P(I,J)
        IF(J.NE.4) DPS(J)=DPS(J)+P(I,J)
  120 CONTINUE
      DPS(4)=DPS(4)+SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2+P(I,5)**2)
      K(N+NP,3)=I
      IF(KQ.NE.2) KQSUM=KQSUM+KQ
      IF(K(I,1).EQ.41) THEN
        IF(MOD(KQSUM,2).EQ.0.AND.MJU(1).EQ.0) THEN
          MJU(1)=N+NP
        ELSE
          MJU(2)=N+NP
        ENDIF
      ENDIF
      IF(K(I,1).EQ.2.OR.K(I,1).EQ.41) GOTO 110
      IF(MOD(KQSUM,3).NE.0) THEN
        CALL PYERRM(12,'(PYSTRF:) unphysical flavour combination')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      IF(MJU(1).GT.0.OR.MJU(2).GT.0) MSTU(29)=1
 
C...Boost copied system to CM frame (for better numerical precision).
      IF(ABS(DPS(3)).LT.0.99D0*DPS(4)) THEN
        MBST=0
        MSTU(33)=1
        CALL PYROBO(N+1,N+NP,0D0,0D0,-DPS(1)/DPS(4),-DPS(2)/DPS(4),
     &  -DPS(3)/DPS(4))
      ELSE
        MBST=1
        HHBZ=SQRT(MAX(1D-6,DPS(4)+DPS(3))/MAX(1D-6,DPS(4)-DPS(3)))
        DO 130 I=N+1,N+NP
          HHPMT=P(I,1)**2+P(I,2)**2+P(I,5)**2
          IF(P(I,3).GT.0D0) THEN
            HHPEZ=MAX(1D-10,(P(I,4)+P(I,3))/HHBZ)
            P(I,3)=0.5D0*(HHPEZ-HHPMT/HHPEZ)
            P(I,4)=0.5D0*(HHPEZ+HHPMT/HHPEZ)
          ELSE
            HHPEZ=MAX(1D-10,(P(I,4)-P(I,3))*HHBZ)
            P(I,3)=-0.5D0*(HHPEZ-HHPMT/HHPEZ)
            P(I,4)=0.5D0*(HHPEZ+HHPMT/HHPEZ)
          ENDIF
  130   CONTINUE
      ENDIF
 
C...Search for very nearby partons that may be recombined.
      NTRYR=0
      NTRYWR=0
      PARU12=PARU(12)
      PARU13=PARU(13)
      MJU(3)=MJU(1)
      MJU(4)=MJU(2)
      NR=NP
  140 IF(NR.GE.3) THEN
        PDRMIN=2D0*PARU12
        DO 150 I=N+1,N+NR
          IF(I.EQ.N+NR.AND.IABS(K(N+1,2)).NE.21) GOTO 150
          I1=I+1
          IF(I.EQ.N+NR) I1=N+1
          IF(K(I,1).EQ.41.OR.K(I1,1).EQ.41) GOTO 150
          IF(MJU(1).NE.0.AND.I1.LT.MJU(1).AND.IABS(K(I1,2)).NE.21)
     &    GOTO 150
          IF(MJU(2).NE.0.AND.I.GT.MJU(2).AND.IABS(K(I,2)).NE.21)
     &    GOTO 150
          PAP=SQRT((P(I,1)**2+P(I,2)**2+P(I,3)**2)*(P(I1,1)**2+
     &    P(I1,2)**2+P(I1,3)**2))
          PVP=P(I,1)*P(I1,1)+P(I,2)*P(I1,2)+P(I,3)*P(I1,3)
          PDR=4D0*(PAP-PVP)**2/MAX(1D-6,PARU13**2*PAP+2D0*(PAP-PVP))
          IF(PDR.LT.PDRMIN) THEN
            IR=I
            PDRMIN=PDR
          ENDIF
  150   CONTINUE
 
C...Recombine very nearby partons to avoid machine precision problems.
        IF(PDRMIN.LT.PARU12.AND.IR.EQ.N+NR) THEN
          DO 160 J=1,4
            P(N+1,J)=P(N+1,J)+P(N+NR,J)
  160     CONTINUE
          P(N+1,5)=SQRT(MAX(0D0,P(N+1,4)**2-P(N+1,1)**2-P(N+1,2)**2-
     &    P(N+1,3)**2))
          NR=NR-1
          GOTO 140
        ELSEIF(PDRMIN.LT.PARU12) THEN
          DO 170 J=1,4
            P(IR,J)=P(IR,J)+P(IR+1,J)
  170     CONTINUE
          P(IR,5)=SQRT(MAX(0D0,P(IR,4)**2-P(IR,1)**2-P(IR,2)**2-
     &    P(IR,3)**2))
          IF(MJU(2).NE.0.AND.IR.GT.MJU(2)) K(IR,2)=K(IR+1,2)
          DO 190 I=IR+1,N+NR-1
            K(I,1)=K(I+1,1)
            K(I,2)=K(I+1,2)
            DO 180 J=1,5
              P(I,J)=P(I+1,J)
  180       CONTINUE
  190     CONTINUE
          IF(IR.EQ.N+NR-1) K(IR,2)=K(N+NR,2)
          NR=NR-1
          IF(MJU(1).GT.IR) MJU(1)=MJU(1)-1
          IF(MJU(2).GT.IR) MJU(2)=MJU(2)-1
          GOTO 140
        ENDIF
      ENDIF
      NTRYR=NTRYR+1
 
C...Reset particle counter. Skip ahead if no junctions are present;
C...this is usually the case!
      NRS=MAX(5*NR+11,NP)
      NTRY=0
  200 NTRY=NTRY+1
      IF(NTRY.GT.100.AND.NTRYR.LE.8) THEN
        PARU12=4D0*PARU12
        PARU13=2D0*PARU13
        GOTO 140
      ELSEIF(NTRY.GT.100) THEN
        CALL PYERRM(14,'(PYSTRF:) caught in infinite loop')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      I=N+NRS
      MSTU(90)=MSTU90
      IF(MJU(1).EQ.0.AND.MJU(2).EQ.0) GOTO 620
      IF(MSTJ(12).GE.4) CALL PYERRM(29,'(PYSTRF:) sorry,'//
     &     ' junction strings not handled by MSTJ(12)>3 options')
      DO 610 JT=1,2
        NJS(JT)=0
        IF(MJU(JT).EQ.0) GOTO 610
        JS=3-2*JT
 
C...Find and sum up momentum on three sides of junction.
        DO 220 IU=1,3
          IJU(IU)=0
          DO 210 J=1,5
            PJU(IU,J)=0D0
  210     CONTINUE
  220   CONTINUE
        IU=0
        DO 240 I1=N+1+(JT-1)*(NR-1),N+NR+(JT-1)*(1-NR),JS
          IF(K(I1,2).NE.21.AND.IU.LE.2) THEN
            IU=IU+1
            IJU(IU)=I1
          ENDIF
          DO 230 J=1,4
            PJU(IU,J)=PJU(IU,J)+P(I1,J)
  230     CONTINUE
  240   CONTINUE
        DO 250 IU=1,3
          PJU(IU,5)=SQRT(PJU(IU,1)**2+PJU(IU,2)**2+PJU(IU,3)**2)
  250   CONTINUE
 
C...Calculate (approximate) boost to rest frame of junction.
        T12=(PJU(1,1)*PJU(2,1)+PJU(1,2)*PJU(2,2)+PJU(1,3)*PJU(2,3))/
     &  (PJU(1,5)*PJU(2,5))
        T13=(PJU(1,1)*PJU(3,1)+PJU(1,2)*PJU(3,2)+PJU(1,3)*PJU(3,3))/
     &  (PJU(1,5)*PJU(3,5))
        T23=(PJU(2,1)*PJU(3,1)+PJU(2,2)*PJU(3,2)+PJU(2,3)*PJU(3,3))/
     &  (PJU(2,5)*PJU(3,5))
        T11=SQRT((2D0/3D0)*(1D0-T12)*(1D0-T13)/(1D0-T23))
        T22=SQRT((2D0/3D0)*(1D0-T12)*(1D0-T23)/(1D0-T13))
        TSQ=SQRT((2D0*T11*T22+T12-1D0)*(1D0+T12))
        T1F=(TSQ-T22*(1D0+T12))/(1D0-T12**2)
        T2F=(TSQ-T11*(1D0+T12))/(1D0-T12**2)
        DO 260 J=1,3
          TJU(J)=-(T1F*PJU(1,J)/PJU(1,5)+T2F*PJU(2,J)/PJU(2,5))
  260   CONTINUE
        TJU(4)=SQRT(1D0+TJU(1)**2+TJU(2)**2+TJU(3)**2)
 
C...Restrict junction motion to be below velocity of qq subsystem.
C...(Opposite possible only by massless approximation breakdown.)
        PMJU12=SQRT(MAX(0D0,(PJU(1,4)+PJU(2,4))**2-(PJU(1,1)+
     &  PJU(2,1))**2-(PJU(1,2)+PJU(2,2))**2-(PJU(1,3)+PJU(2,3))**2))
        TJUMAX=(PJU(1,4)+PJU(2,4))/PMJU12
        IF(TJU(4).GT.TJUMAX) THEN
          TJURND=1D0+SQRT(PYR(0))*(TJUMAX-1D0)
          FJURE=SQRT((TJURND**2-1D0)/(TJU(4)**2-1D0))
          DO 270 J=1,3
            TJU(J)=FJURE*TJU(J)
  270     CONTINUE
          TJU(4)=TJURND
        ENDIF
 
C...Calculate string piece energies in junction rest frame.
        DO 280 IU=1,3
          PJU(IU,5)=TJU(4)*PJU(IU,4)-TJU(1)*PJU(IU,1)-TJU(2)*PJU(IU,2)-
     &    TJU(3)*PJU(IU,3)
  280   CONTINUE
 
C...Put junction at rest if motion could give inconsistencies.
        IF(PJU(1,5)+PJU(2,5).GT.PJU(1,4)+PJU(2,4)) THEN
          DO 290 J=1,3
            TJU(J)=0D0
  290     CONTINUE
          TJU(4)=1D0
          PJU(1,5)=PJU(1,4)
          PJU(2,5)=PJU(2,4)
          PJU(3,5)=PJU(3,4)
        ENDIF
 
C...Start preparing for fragmentation of two strings from junction.
        ISTA=I
        NTRYER=0
  300   NTRYER=NTRYER+1
        I=ISTA
        DO 590 IU=1,2
          NS=IABS(IJU(IU+1)-IJU(IU))
 
C...Junction strings: find longitudinal string directions.
          DO 330 IS=1,NS
            IS1=IJU(IU)+JS*(IS-1)
            IS2=IJU(IU)+JS*IS
            DO 310 J=1,5
              DP(1,J)=0.5D0*P(IS1,J)
              IF(IS.EQ.1) DP(1,J)=P(IS1,J)
              DP(2,J)=0.5D0*P(IS2,J)
              IF(IS.EQ.NS) DP(2,J)=-PJU(IU,J)+2D0*PJU(IU,5)*TJU(J)
  310       CONTINUE
            IF(IS.EQ.NS) DP(2,5)=SQRT(MAX(0D0,PJU(IU,4)**2-
     &      PJU(IU,1)**2-PJU(IU,2)**2-PJU(IU,3)**2))
            DP(3,5)=DFOUR(1,1)
            DP(4,5)=DFOUR(2,2)
            DHKC=DFOUR(1,2)
            IF(DP(3,5)+2D0*DHKC+DP(4,5).LE.0D0) THEN
              DP(1,4)=SQRT(DP(1,1)**2+DP(1,2)**2+DP(1,3)**2)
              DP(2,4)=SQRT(DP(2,1)**2+DP(2,2)**2+DP(2,3)**2)
              DP(3,5)=0D0
              DP(4,5)=0D0
              DHKC=DFOUR(1,2)
            ENDIF
            DHKS=SQRT(DHKC**2-DP(3,5)*DP(4,5))
            DHK1=0.5D0*((DP(4,5)+DHKC)/DHKS-1D0)
            DHK2=0.5D0*((DP(3,5)+DHKC)/DHKS-1D0)
            IN1=N+NR+4*IS-3
            P(IN1,5)=SQRT(DP(3,5)+2D0*DHKC+DP(4,5))
            DO 320 J=1,4
              P(IN1,J)=(1D0+DHK1)*DP(1,J)-DHK2*DP(2,J)
              P(IN1+1,J)=(1D0+DHK2)*DP(2,J)-DHK1*DP(1,J)
  320       CONTINUE
  330     CONTINUE
 
C...Junction strings: initialize flavour, momentum and starting pos.
          ISAV=I
          MSTU91=MSTU(90)
  340     NTRY=NTRY+1
          IF(NTRY.GT.100.AND.NTRYR.LE.8) THEN
            PARU12=4D0*PARU12
            PARU13=2D0*PARU13
            GOTO 140
          ELSEIF(NTRY.GT.100) THEN
            CALL PYERRM(14,'(PYSTRF:) caught in infinite loop')
            IF(MSTU(21).GE.1) RETURN
          ENDIF
          I=ISAV
          MSTU(90)=MSTU91
          IRANKJ=0
          IE(1)=K(N+1+(JT/2)*(NP-1),3)
          IN(4)=N+NR+1
          IN(5)=IN(4)+1
          IN(6)=N+NR+4*NS+1
          DO 360 JQ=1,2
            DO 350 IN1=N+NR+2+JQ,N+NR+4*NS-2+JQ,4
              P(IN1,1)=2-JQ
              P(IN1,2)=JQ-1
              P(IN1,3)=1D0
  350       CONTINUE
  360     CONTINUE
          KFL(1)=K(IJU(IU),2)
          PX(1)=0D0
          PY(1)=0D0
          GAM(1)=0D0
          DO 370 J=1,5
            PJU(IU+3,J)=0D0
  370     CONTINUE
 
C...Junction strings: find initial transverse directions.
          DO 380 J=1,4
            DP(1,J)=P(IN(4),J)
            DP(2,J)=P(IN(4)+1,J)
            DP(3,J)=0D0
            DP(4,J)=0D0
  380     CONTINUE
          DP(1,4)=SQRT(DP(1,1)**2+DP(1,2)**2+DP(1,3)**2)
          DP(2,4)=SQRT(DP(2,1)**2+DP(2,2)**2+DP(2,3)**2)
          DP(5,1)=DP(1,1)/DP(1,4)-DP(2,1)/DP(2,4)
          DP(5,2)=DP(1,2)/DP(1,4)-DP(2,2)/DP(2,4)
          DP(5,3)=DP(1,3)/DP(1,4)-DP(2,3)/DP(2,4)
          IF(DP(5,1)**2.LE.DP(5,2)**2+DP(5,3)**2) DP(3,1)=1D0
          IF(DP(5,1)**2.GT.DP(5,2)**2+DP(5,3)**2) DP(3,3)=1D0
          IF(DP(5,2)**2.LE.DP(5,1)**2+DP(5,3)**2) DP(4,2)=1D0
          IF(DP(5,2)**2.GT.DP(5,1)**2+DP(5,3)**2) DP(4,3)=1D0
          DHC12=DFOUR(1,2)
          DHCX1=DFOUR(3,1)/DHC12
          DHCX2=DFOUR(3,2)/DHC12
          DHCXX=1D0/SQRT(1D0+2D0*DHCX1*DHCX2*DHC12)
          DHCY1=DFOUR(4,1)/DHC12
          DHCY2=DFOUR(4,2)/DHC12
          DHCYX=DHCXX*(DHCX1*DHCY2+DHCX2*DHCY1)*DHC12
          DHCYY=1D0/SQRT(1D0+2D0*DHCY1*DHCY2*DHC12-DHCYX**2)
          DO 390 J=1,4
            DP(3,J)=DHCXX*(DP(3,J)-DHCX2*DP(1,J)-DHCX1*DP(2,J))
            P(IN(6),J)=DP(3,J)
            P(IN(6)+1,J)=DHCYY*(DP(4,J)-DHCY2*DP(1,J)-DHCY1*DP(2,J)-
     &      DHCYX*DP(3,J))
  390     CONTINUE
 
C...Junction strings: produce new particle, origin.
  400     I=I+1
          IF(2*I-NSAV.GE.MSTU(4)-MSTU(32)-5) THEN
            CALL PYERRM(11,'(PYSTRF:) no more memory left in PYJETS')
            IF(MSTU(21).GE.1) RETURN
          ENDIF
          IRANKJ=IRANKJ+1
          K(I,1)=1
          K(I,3)=IE(1)
          K(I,4)=0
          K(I,5)=0
 
C...Junction strings: generate flavour, hadron, pT, z and Gamma.
  410     CALL PYKFDI(KFL(1),0,KFL(3),K(I,2))
          IF(K(I,2).EQ.0) GOTO 340
          IF(IRANKJ.EQ.1.AND.IABS(KFL(1)).LE.10.AND.
     &    IABS(KFL(3)).GT.10) THEN
            IF(PYR(0).GT.PARJ(19)) GOTO 410
          ENDIF
          P(I,5)=PYMASS(K(I,2))
          CALL PYPTDI(KFL(1),PX(3),PY(3))
          PR(1)=P(I,5)**2+(PX(1)+PX(3))**2+(PY(1)+PY(3))**2
          CALL PYZDIS(KFL(1),KFL(3),PR(1),Z)
          IF(IABS(KFL(1)).GE.4.AND.IABS(KFL(1)).LE.8.AND.
     &    MSTU(90).LT.8) THEN
            MSTU(90)=MSTU(90)+1
            MSTU(90+MSTU(90))=I
            PARU(90+MSTU(90))=Z
          ENDIF
          GAM(3)=(1D0-Z)*(GAM(1)+PR(1)/Z)
          DO 420 J=1,3
            IN(J)=IN(3+J)
  420     CONTINUE
 
C...Junction strings: stepping within 'low' string region.
          IF(IN(1)+1.EQ.IN(2).AND.Z*P(IN(1)+2,3)*P(IN(2)+2,3)*
     &    P(IN(1),5)**2.GE.PR(1)) THEN
            P(IN(1)+2,4)=Z*P(IN(1)+2,3)
            P(IN(2)+2,4)=PR(1)/(P(IN(1)+2,4)*P(IN(1),5)**2)
            DO 430 J=1,4
              P(I,J)=(PX(1)+PX(3))*P(IN(3),J)+(PY(1)+PY(3))*P(IN(3)+1,J)
  430       CONTINUE
            GOTO 530
C...Has used up energy of junction string, i.e. no more hadrons in it.
          ELSEIF(IN(1)+1.EQ.IN(2).AND.IN(1).EQ.N+NR+4*NS-3) THEN
            DO 440 J=1,5
              P(I,J)=0D0
  440       CONTINUE
            GOTO 570
C...Stepping from 'low' string region
          ELSEIF(IN(1)+1.EQ.IN(2)) THEN
            P(IN(2)+2,4)=P(IN(2)+2,3)
            P(IN(2)+2,1)=1D0
            IN(2)=IN(2)+4
            IF(IN(2).GT.N+NR+4*NS) GOTO 340
            IF(FOUR(IN(1),IN(2)).LE.1D-2) THEN
              P(IN(1)+2,4)=P(IN(1)+2,3)
              P(IN(1)+2,1)=0D0
              IN(1)=IN(1)+4
            ENDIF
          ENDIF
 
C...Junction strings: find new transverse directions.
  450     IF(IN(1).GT.N+NR+4*NS.OR.IN(2).GT.N+NR+4*NS.OR.
     &    IN(1).GT.IN(2)) GOTO 340
          IF(IN(1).NE.IN(4).OR.IN(2).NE.IN(5)) THEN
            DO 460 J=1,4
              DP(1,J)=P(IN(1),J)
              DP(2,J)=P(IN(2),J)
              DP(3,J)=0D0
              DP(4,J)=0D0
  460       CONTINUE
            DP(1,4)=SQRT(DP(1,1)**2+DP(1,2)**2+DP(1,3)**2)
            DP(2,4)=SQRT(DP(2,1)**2+DP(2,2)**2+DP(2,3)**2)
            DHC12=DFOUR(1,2)
            IF(DHC12.LE.1D-2) THEN
              P(IN(1)+2,4)=P(IN(1)+2,3)
              P(IN(1)+2,1)=0D0
              IN(1)=IN(1)+4
              GOTO 450
            ENDIF
            IN(3)=N+NR+4*NS+5
            DP(5,1)=DP(1,1)/DP(1,4)-DP(2,1)/DP(2,4)
            DP(5,2)=DP(1,2)/DP(1,4)-DP(2,2)/DP(2,4)
            DP(5,3)=DP(1,3)/DP(1,4)-DP(2,3)/DP(2,4)
            IF(DP(5,1)**2.LE.DP(5,2)**2+DP(5,3)**2) DP(3,1)=1D0
            IF(DP(5,1)**2.GT.DP(5,2)**2+DP(5,3)**2) DP(3,3)=1D0
            IF(DP(5,2)**2.LE.DP(5,1)**2+DP(5,3)**2) DP(4,2)=1D0
            IF(DP(5,2)**2.GT.DP(5,1)**2+DP(5,3)**2) DP(4,3)=1D0
            DHCX1=DFOUR(3,1)/DHC12
            DHCX2=DFOUR(3,2)/DHC12
            DHCXX=1D0/SQRT(1D0+2D0*DHCX1*DHCX2*DHC12)
            DHCY1=DFOUR(4,1)/DHC12
            DHCY2=DFOUR(4,2)/DHC12
            DHCYX=DHCXX*(DHCX1*DHCY2+DHCX2*DHCY1)*DHC12
            DHCYY=1D0/SQRT(1D0+2D0*DHCY1*DHCY2*DHC12-DHCYX**2)
            DO 470 J=1,4
              DP(3,J)=DHCXX*(DP(3,J)-DHCX2*DP(1,J)-DHCX1*DP(2,J))
              P(IN(3),J)=DP(3,J)
              P(IN(3)+1,J)=DHCYY*(DP(4,J)-DHCY2*DP(1,J)-DHCY1*DP(2,J)-
     &        DHCYX*DP(3,J))
  470       CONTINUE
C...Express pT with respect to new axes, if sensible.
            PXP=-(PX(3)*FOUR(IN(6),IN(3))+PY(3)*FOUR(IN(6)+1,IN(3)))
            PYP=-(PX(3)*FOUR(IN(6),IN(3)+1)+PY(3)*FOUR(IN(6)+1,IN(3)+1))
            IF(ABS(PXP**2+PYP**2-PX(3)**2-PY(3)**2).LT.0.01D0) THEN
              PX(3)=PXP
              PY(3)=PYP
            ENDIF
          ENDIF
 
C...Junction strings: sum up known four-momentum, coefficients for m2.
          DO 500 J=1,4
            DHG(J)=0D0
            P(I,J)=PX(1)*P(IN(6),J)+PY(1)*P(IN(6)+1,J)+PX(3)*P(IN(3),J)+
     &      PY(3)*P(IN(3)+1,J)
            DO 480 IN1=IN(4),IN(1)-4,4
              P(I,J)=P(I,J)+P(IN1+2,3)*P(IN1,J)
  480       CONTINUE
            DO 490 IN2=IN(5),IN(2)-4,4
              P(I,J)=P(I,J)+P(IN2+2,3)*P(IN2,J)
  490       CONTINUE
  500     CONTINUE
          DHM(1)=FOUR(I,I)
          DHM(2)=2D0*FOUR(I,IN(1))
          DHM(3)=2D0*FOUR(I,IN(2))
          DHM(4)=2D0*FOUR(IN(1),IN(2))
 
C...Junction strings: find coefficients for Gamma expression.
          DO 520 IN2=IN(1)+1,IN(2),4
            DO 510 IN1=IN(1),IN2-1,4
              DHC=2D0*FOUR(IN1,IN2)
              DHG(1)=DHG(1)+P(IN1+2,1)*P(IN2+2,1)*DHC
              IF(IN1.EQ.IN(1)) DHG(2)=DHG(2)-P(IN2+2,1)*DHC
              IF(IN2.EQ.IN(2)) DHG(3)=DHG(3)+P(IN1+2,1)*DHC
              IF(IN1.EQ.IN(1).AND.IN2.EQ.IN(2)) DHG(4)=DHG(4)-DHC
  510       CONTINUE
  520     CONTINUE
 
C...Junction strings: solve (m2, Gamma) equation system for energies.
          DHS1=DHM(3)*DHG(4)-DHM(4)*DHG(3)
          IF(ABS(DHS1).LT.1D-4) GOTO 340
          DHS2=DHM(4)*(GAM(3)-DHG(1))-DHM(2)*DHG(3)-DHG(4)*
     &    (P(I,5)**2-DHM(1))+DHG(2)*DHM(3)
          DHS3=DHM(2)*(GAM(3)-DHG(1))-DHG(2)*(P(I,5)**2-DHM(1))
          P(IN(2)+2,4)=0.5D0*(SQRT(MAX(0D0,DHS2**2-4D0*DHS1*DHS3))/
     &    ABS(DHS1)-DHS2/DHS1)
          IF(DHM(2)+DHM(4)*P(IN(2)+2,4).LE.0D0) GOTO 340
          P(IN(1)+2,4)=(P(I,5)**2-DHM(1)-DHM(3)*P(IN(2)+2,4))/
     &    (DHM(2)+DHM(4)*P(IN(2)+2,4))
 
C...Junction strings: step to new region if necessary.
          IF(P(IN(2)+2,4).GT.P(IN(2)+2,3)) THEN
            P(IN(2)+2,4)=P(IN(2)+2,3)
            P(IN(2)+2,1)=1D0
            IN(2)=IN(2)+4
            IF(IN(2).GT.N+NR+4*NS) GOTO 340
            IF(FOUR(IN(1),IN(2)).LE.1D-2) THEN
              P(IN(1)+2,4)=P(IN(1)+2,3)
              P(IN(1)+2,1)=0D0
              IN(1)=IN(1)+4
            ENDIF
            GOTO 450
          ELSEIF(P(IN(1)+2,4).GT.P(IN(1)+2,3)) THEN
            P(IN(1)+2,4)=P(IN(1)+2,3)
            P(IN(1)+2,1)=0D0
            IN(1)=IN(1)+4
            GOTO 450
          ENDIF
 
C...Junction strings: particle four-momentum, remainder, loop back.
  530     DO 540 J=1,4
            P(I,J)=P(I,J)+P(IN(1)+2,4)*P(IN(1),J)+
     &      P(IN(2)+2,4)*P(IN(2),J)
            PJU(IU+3,J)=PJU(IU+3,J)+P(I,J)
  540     CONTINUE
          IF(P(I,4).LT.P(I,5)) GOTO 340
          PJU(IU+3,5)=TJU(4)*PJU(IU+3,4)-TJU(1)*PJU(IU+3,1)-
     &    TJU(2)*PJU(IU+3,2)-TJU(3)*PJU(IU+3,3)
          IF(PJU(IU+3,5).LT.PJU(IU,5)) THEN
            KFL(1)=-KFL(3)
            PX(1)=-PX(3)
            PY(1)=-PY(3)
            GAM(1)=GAM(3)
            IF(IN(3).NE.IN(6)) THEN
              DO 550 J=1,4
                P(IN(6),J)=P(IN(3),J)
                P(IN(6)+1,J)=P(IN(3)+1,J)
  550         CONTINUE
            ENDIF
            DO 560 JQ=1,2
              IN(3+JQ)=IN(JQ)
              P(IN(JQ)+2,3)=P(IN(JQ)+2,3)-P(IN(JQ)+2,4)
              P(IN(JQ)+2,1)=P(IN(JQ)+2,1)-(3-2*JQ)*P(IN(JQ)+2,4)
  560       CONTINUE
            GOTO 400
          ENDIF
 
C...Junction strings: save quantities left after each string.
          IF(IABS(KFL(1)).GT.10) GOTO 340
  570     I=I-1
          KFJH(IU)=KFL(1)
          DO 580 J=1,4
            PJU(IU+3,J)=PJU(IU+3,J)-P(I+1,J)
  580     CONTINUE
 
C...Junction strings: loopback if much unused energy in both strings.
          PJU(IU+3,5)=TJU(4)*PJU(IU+3,4)-TJU(1)*PJU(IU+3,1)-
     &    TJU(2)*PJU(IU+3,2)-TJU(3)*PJU(IU+3,3)
          EJSTR(IU)=PJU(IU,5)-PJU(IU+3,5)
  590   CONTINUE
        IF((MIN(EJSTR(1),EJSTR(2)).GT.PARJ(49).OR.
     &  EJSTR(1).GT.PARJ(49)+PYR(0)*PARJ(50).OR.
     &  EJSTR(2).GT.PARJ(49)+PYR(0)*PARJ(50))
     &  .AND.NTRYER.LT.10) GOTO 300
 
C...Junction strings: put together to new effective string endpoint.
        NJS(JT)=I-ISTA
        KFLS=2*INT(PYR(0)+3D0*PARJ(4)/(1D0+3D0*PARJ(4)))+1
        IF(KFJH(1).EQ.KFJH(2)) KFLS=3
        KFJS(JT)=ISIGN(1000*MAX(IABS(KFJH(1)),IABS(KFJH(2)))+
     &  100*MIN(IABS(KFJH(1)),IABS(KFJH(2)))+KFLS,KFJH(1))
        DO 600 J=1,4
          PJS(JT,J)=PJU(1,J)+PJU(2,J)+P(MJU(JT),J)
          PJS(JT+2,J)=PJU(4,J)+PJU(5,J)
  600   CONTINUE
        PJS(JT,5)=SQRT(MAX(0D0,PJS(JT,4)**2-PJS(JT,1)**2-PJS(JT,2)**2-
     &  PJS(JT,3)**2))
        PJS(JT+2,5)=0D0
  610 CONTINUE
 
C...Open versus closed strings. Choose breakup region for latter.
  620 IF(MJU(1).NE.0.AND.MJU(2).NE.0) THEN
        NS=MJU(2)-MJU(1)
        NB=MJU(1)-N
      ELSEIF(MJU(1).NE.0) THEN
        NS=N+NR-MJU(1)
        NB=MJU(1)-N
      ELSEIF(MJU(2).NE.0) THEN
        NS=MJU(2)-N
        NB=1
      ELSEIF(IABS(K(N+1,2)).NE.21) THEN
        NS=NR-1
        NB=1
      ELSE
        NS=NR+1
        W2SUM=0D0
        DO 630 IS=1,NR
          P(N+NR+IS,1)=0.5D0*FOUR(N+IS,N+IS+1-NR*(IS/NR))
          W2SUM=W2SUM+P(N+NR+IS,1)
  630   CONTINUE
        W2RAN=PYR(0)*W2SUM
        NB=0
  640   NB=NB+1
        W2SUM=W2SUM-P(N+NR+NB,1)
        IF(W2SUM.GT.W2RAN.AND.NB.LT.NR) GOTO 640
      ENDIF
 
C...Find longitudinal string directions (i.e. lightlike four-vectors).
      DO 670 IS=1,NS
        IS1=N+IS+NB-1-NR*((IS+NB-2)/NR)
        IS2=N+IS+NB-NR*((IS+NB-1)/NR)
        DO 650 J=1,5
          DP(1,J)=P(IS1,J)
          IF(IABS(K(IS1,2)).EQ.21) DP(1,J)=0.5D0*DP(1,J)
          IF(IS1.EQ.MJU(1)) DP(1,J)=PJS(1,J)-PJS(3,J)
          DP(2,J)=P(IS2,J)
          IF(IABS(K(IS2,2)).EQ.21) DP(2,J)=0.5D0*DP(2,J)
          IF(IS2.EQ.MJU(2)) DP(2,J)=PJS(2,J)-PJS(4,J)
  650   CONTINUE
        IF(IS1.EQ.MJU(1)) DP(1,5)=SQRT(MAX(0D0,DP(1,4)**2-DP(1,1)**2-
     &  DP(1,2)**2-DP(1,3)**2))
        IF(IS2.EQ.MJU(2)) DP(2,5)=SQRT(MAX(0D0,DP(2,4)**2-DP(2,1)**2-
     &  DP(2,2)**2-DP(2,3)**2))
        DP(3,5)=DFOUR(1,1)
        DP(4,5)=DFOUR(2,2)
        DHKC=DFOUR(1,2)
        IF(DP(3,5)+2D0*DHKC+DP(4,5).LE.0D0) GOTO 200
        DHKS=SQRT(DHKC**2-DP(3,5)*DP(4,5))
        DHK1=0.5D0*((DP(4,5)+DHKC)/DHKS-1D0)
        DHK2=0.5D0*((DP(3,5)+DHKC)/DHKS-1D0)
        IN1=N+NR+4*IS-3
        P(IN1,5)=SQRT(DP(3,5)+2D0*DHKC+DP(4,5))
        DO 660 J=1,4
          P(IN1,J)=(1D0+DHK1)*DP(1,J)-DHK2*DP(2,J)
          P(IN1+1,J)=(1D0+DHK2)*DP(2,J)-DHK1*DP(1,J)
  660   CONTINUE
  670 CONTINUE
 
C...Begin initialization: sum up energy, set starting position.
      ISAV=I
      MSTU91=MSTU(90)
  680 NTRY=NTRY+1
      IF(NTRY.GT.100.AND.NTRYR.LE.8) THEN
        PARU12=4D0*PARU12
        PARU13=2D0*PARU13
        GOTO 140
      ELSEIF(NTRY.GT.100) THEN
        CALL PYERRM(14,'(PYSTRF:) caught in infinite loop')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      I=ISAV
      MSTU(90)=MSTU91
      DO 700 J=1,4
        P(N+NRS,J)=0D0
        DO 690 IS=1,NR
          P(N+NRS,J)=P(N+NRS,J)+P(N+IS,J)
  690   CONTINUE
  700 CONTINUE
      DO 720 JT=1,2
        IRANK(JT)=0
        IF(MJU(JT).NE.0) IRANK(JT)=NJS(JT)
        IF(NS.GT.NR) IRANK(JT)=1
        IE(JT)=K(N+1+(JT/2)*(NP-1),3)
        IN(3*JT+1)=N+NR+1+4*(JT/2)*(NS-1)
        IN(3*JT+2)=IN(3*JT+1)+1
        IN(3*JT+3)=N+NR+4*NS+2*JT-1
        DO 710 IN1=N+NR+2+JT,N+NR+4*NS-2+JT,4
          P(IN1,1)=2-JT
          P(IN1,2)=JT-1
          P(IN1,3)=1D0
  710   CONTINUE
  720 CONTINUE
 
C.. MOPS variables and switches
      NRVMO=0
      XBMO=1D0
      MSTU(121)=0
      MSTU(122)=0
 
C...Initialize flavour and pT variables for open string.
      IF(NS.LT.NR) THEN
        PX(1)=0D0
        PY(1)=0D0
        IF(NS.EQ.1.AND.MJU(1)+MJU(2).EQ.0) CALL PYPTDI(0,PX(1),PY(1))
        PX(2)=-PX(1)
        PY(2)=-PY(1)
        DO 730 JT=1,2
          KFL(JT)=K(IE(JT),2)
          IF(MJU(JT).NE.0) KFL(JT)=KFJS(JT)
          MSTJ(93)=1
          PMQ(JT)=PYMASS(KFL(JT))
          GAM(JT)=0D0
  730   CONTINUE
 
C...Closed string: random initial breakup flavour, pT and vertex.
      ELSE
        KFL(3)=INT(1D0+(2D0+PARJ(2))*PYR(0))*(-1)**INT(PYR(0)+0.5D0)
        IBMO=0
  740   CALL PYKFDI(KFL(3),0,KFL(1),KDUMP)
C.. Closed string: first vertex diq attempt => enforced second
C.. vertex diq
        IF(IABS(KFL(1)).GT.10)THEN
           IBMO=1
           MSTU(121)=0
           GOTO 740
        ENDIF
        IF(IBMO.EQ.1) MSTU(121)=-1
        KFL(2)=-KFL(1)
        CALL PYPTDI(KFL(1),PX(1),PY(1))
        PX(2)=-PX(1)
        PY(2)=-PY(1)
        PR3=MIN(25D0,0.1D0*P(N+NR+1,5)**2)
  750   CALL PYZDIS(KFL(1),KFL(2),PR3,Z)
        ZR=PR3/(Z*P(N+NR+1,5)**2)
        IF(ZR.GE.1D0) GOTO 750
        DO 760 JT=1,2
          MSTJ(93)=1
          PMQ(JT)=PYMASS(KFL(JT))
          GAM(JT)=PR3*(1D0-Z)/Z
          IN1=N+NR+3+4*(JT/2)*(NS-1)
          P(IN1,JT)=1D0-Z
          P(IN1,3-JT)=JT-1
          P(IN1,3)=(2-JT)*(1D0-Z)+(JT-1)*Z
          P(IN1+1,JT)=ZR
          P(IN1+1,3-JT)=2-JT
          P(IN1+1,3)=(2-JT)*(1D0-ZR)+(JT-1)*ZR
  760   CONTINUE
      ENDIF
C.. MOPS variables
      DO 770 JT=1,2
         XTMO(JT)=1D0
         PM2QMO(JT)=PMQ(JT)**2
         IF(IABS(KFL(JT)).GT.10) PM2QMO(JT)=0D0
  770 CONTINUE
 
C...Find initial transverse directions (i.e. spacelike four-vectors).
      DO 810 JT=1,2
        IF(JT.EQ.1.OR.NS.EQ.NR-1.OR.MJU(1)+MJU(2).NE.0) THEN
          IN1=IN(3*JT+1)
          IN3=IN(3*JT+3)
          DO 780 J=1,4
            DP(1,J)=P(IN1,J)
            DP(2,J)=P(IN1+1,J)
            DP(3,J)=0D0
            DP(4,J)=0D0
  780     CONTINUE
          DP(1,4)=SQRT(DP(1,1)**2+DP(1,2)**2+DP(1,3)**2)
          DP(2,4)=SQRT(DP(2,1)**2+DP(2,2)**2+DP(2,3)**2)
          DP(5,1)=DP(1,1)/DP(1,4)-DP(2,1)/DP(2,4)
          DP(5,2)=DP(1,2)/DP(1,4)-DP(2,2)/DP(2,4)
          DP(5,3)=DP(1,3)/DP(1,4)-DP(2,3)/DP(2,4)
          IF(DP(5,1)**2.LE.DP(5,2)**2+DP(5,3)**2) DP(3,1)=1D0
          IF(DP(5,1)**2.GT.DP(5,2)**2+DP(5,3)**2) DP(3,3)=1D0
          IF(DP(5,2)**2.LE.DP(5,1)**2+DP(5,3)**2) DP(4,2)=1D0
          IF(DP(5,2)**2.GT.DP(5,1)**2+DP(5,3)**2) DP(4,3)=1D0
          DHC12=DFOUR(1,2)
          DHCX1=DFOUR(3,1)/DHC12
          DHCX2=DFOUR(3,2)/DHC12
          DHCXX=1D0/SQRT(1D0+2D0*DHCX1*DHCX2*DHC12)
          DHCY1=DFOUR(4,1)/DHC12
          DHCY2=DFOUR(4,2)/DHC12
          DHCYX=DHCXX*(DHCX1*DHCY2+DHCX2*DHCY1)*DHC12
          DHCYY=1D0/SQRT(1D0+2D0*DHCY1*DHCY2*DHC12-DHCYX**2)
          DO 790 J=1,4
            DP(3,J)=DHCXX*(DP(3,J)-DHCX2*DP(1,J)-DHCX1*DP(2,J))
            P(IN3,J)=DP(3,J)
            P(IN3+1,J)=DHCYY*(DP(4,J)-DHCY2*DP(1,J)-DHCY1*DP(2,J)-
     &      DHCYX*DP(3,J))
  790     CONTINUE
        ELSE
          DO 800 J=1,4
            P(IN3+2,J)=P(IN3,J)
            P(IN3+3,J)=P(IN3+1,J)
  800     CONTINUE
        ENDIF
  810 CONTINUE
 
C...Remove energy used up in junction string fragmentation.
      IF(MJU(1)+MJU(2).GT.0) THEN
        DO 830 JT=1,2
          IF(NJS(JT).EQ.0) GOTO 830
          DO 820 J=1,4
            P(N+NRS,J)=P(N+NRS,J)-PJS(JT+2,J)
  820     CONTINUE
  830   CONTINUE
        PARJST=PARJ(33)
        IF(MSTJ(11).EQ.2) PARJST=PARJ(34)
        WMIN=PARJST+PMQ(1)+PMQ(2)
        WREM2=FOUR(N+NRS,N+NRS)
        IF(P(N+NRS,4).LT.0D0.OR.WREM2.LT.WMIN**2) THEN
          NTRYWR=NTRYWR+1
          IF(MOD(NTRYWR,20).NE.0) NTRYR=NTRYR-1
          GOTO 140
        ENDIF
      ENDIF
 
C...Produce new particle: side, origin.
  840 I=I+1
      IF(2*I-NSAV.GE.MSTU(4)-MSTU(32)-5) THEN
        CALL PYERRM(11,'(PYSTRF:) no more memory left in PYJETS')
        IF(MSTU(21).GE.1) RETURN
      ENDIF
C.. New side priority for popcorn systems
      IF(MSTU(121).LE.0)THEN
         JT=1.5D0+PYR(0)
         IF(IABS(KFL(3-JT)).GT.10) JT=3-JT
         IF(IABS(KFL(3-JT)).GE.4.AND.IABS(KFL(3-JT)).LE.8) JT=3-JT
      ENDIF
      JR=3-JT
      JS=3-2*JT
      IRANK(JT)=IRANK(JT)+1
      K(I,1)=1
      K(I,3)=IE(JT)
      K(I,4)=0
      K(I,5)=0
 
C...Generate flavour, hadron and pT.
  850 CONTINUE
      CALL PYKFDI(KFL(JT),0,KFL(3),K(I,2))
      IF(K(I,2).EQ.0) GOTO 680
      MU90MO=MSTU(90)
      IF(MSTU(121).EQ.-1) GOTO 880
      IF(IRANK(JT).EQ.1.AND.IABS(KFL(JT)).LE.10.AND.
     &IABS(KFL(3)).GT.10) THEN
        IF(PYR(0).GT.PARJ(19)) GOTO 850
      ENDIF
      P(I,5)=PYMASS(K(I,2))
      CALL PYPTDI(KFL(JT),PX(3),PY(3))
      PR(JT)=P(I,5)**2+(PX(JT)+PX(3))**2+(PY(JT)+PY(3))**2
 
C...Final hadrons for small invariant mass.
      MSTJ(93)=1
      PMQ(3)=PYMASS(KFL(3))
      PARJST=PARJ(33)
      IF(MSTJ(11).EQ.2) PARJST=PARJ(34)
      WMIN=PARJST+PMQ(1)+PMQ(2)+PARJ(36)*PMQ(3)
      IF(IABS(KFL(JT)).GT.10.AND.IABS(KFL(3)).GT.10) WMIN=
     &WMIN-0.5D0*PARJ(36)*PMQ(3)
      WREM2=FOUR(N+NRS,N+NRS)
      IF(WREM2.LT.0.10D0) GOTO 680
      IF(WREM2.LT.MAX(WMIN*(1D0+(2D0*PYR(0)-1D0)*PARJ(37)),
     &PARJ(32)+PMQ(1)+PMQ(2))**2) GOTO 1050
 
C...Choose z, which gives Gamma. Shift z for heavy flavours.
      CALL PYZDIS(KFL(JT),KFL(3),PR(JT),Z)
      IF(IABS(KFL(JT)).GE.4.AND.IABS(KFL(JT)).LE.8.AND.
     &MSTU(90).LT.8) THEN
        MSTU(90)=MSTU(90)+1
        MSTU(90+MSTU(90))=I
        PARU(90+MSTU(90))=Z
      ENDIF
      KFL1A=IABS(KFL(1))
      KFL2A=IABS(KFL(2))
      IF(MAX(MOD(KFL1A,10),MOD(KFL1A/1000,10),MOD(KFL2A,10),
     &MOD(KFL2A/1000,10)).GE.4) THEN
        PR(JR)=(PMQ(JR)+PMQ(3))**2+(PX(JR)-PX(3))**2+(PY(JR)-PY(3))**2
        PW12=SQRT(MAX(0D0,(WREM2-PR(1)-PR(2))**2-4D0*PR(1)*PR(2)))
        Z=(WREM2+PR(JT)-PR(JR)+PW12*(2D0*Z-1D0))/(2D0*WREM2)
        PR(JR)=(PMQ(JR)+PARJST)**2+(PX(JR)-PX(3))**2+(PY(JR)-PY(3))**2
        IF((1D0-Z)*(WREM2-PR(JT)/Z).LT.PR(JR)) GOTO 1050
      ENDIF
      GAM(3)=(1D0-Z)*(GAM(JT)+PR(JT)/Z)
 
C.. MOPS baryon model modification
      XTMO3=(1D0-Z)*XTMO(JT)
      IF(IABS(KFL(3)).LE.10) NRVMO=0
      IF(IABS(KFL(3)).GT.10.AND.MSTJ(12).GE.4) THEN
         GTSTMO=1D0
         PTSTMO=1D0
         RTSTMO=PYR(0)
         IF(IABS(KFL(JT)).LE.10)THEN
            XBMO=MIN(XTMO3,1D0-(2D-10))
            GBMO=GAM(3)
            PMMO=0D0
            PGMO=GBMO+LOG(1D0-XBMO)*PM2QMO(JT)
            GTSTMO=1D0-PARF(192)**PGMO
         ELSE
            IF(IRANK(JT).EQ.1) THEN
               GBMO=GAM(JT)
               PMMO=0D0
               XBMO=1D0
            ENDIF
            IF(XBMO.LT.1D0-(1D-10))THEN
               PGNMO=GBMO*XTMO3/XBMO+PM2QMO(JT)*LOG(1D0-XTMO3)
               GTSTMO=(1D0-PARF(192)**PGNMO)/(1D0-PARF(192)**PGMO)
               PGMO=PGNMO
            ENDIF
            IF(MSTJ(12).GE.5)THEN
               PMNMO=SQRT((XBMO-XTMO3)*(GAM(3)/XTMO3-GBMO/XBMO))
               PMMO=PMMO+PMAS(PYCOMP(K(I,2)),1)-PMAS(PYCOMP(K(I,2)),3)
               PTSTMO=EXP((PMMO-PMNMO)*PARF(193))
               PMMO=PMNMO
            ENDIF
         ENDIF
 
C.. MOPS Accepting popcorn system hadron.
         IF(PTSTMO*GTSTMO.GT.RTSTMO) THEN
            IF(IRANK(JT).EQ.1.OR.IABS(KFL(JT)).LE.10) THEN
               NRVMO=I-N-NR
               IF(I+NRVMO.GT.MSTU(4)-MSTU(32)-5) THEN
                  CALL PYERRM(11,
     &                 '(PYSTRF:) no more memory left in PYJETS')
                  IF(MSTU(21).GE.1) RETURN
               ENDIF
               IMO=I
               KFLMO=KFL(JT)
               PMQMO=PMQ(JT)
               PXMO=PX(JT)
               PYMO=PY(JT)
               GAMMO=GAM(JT)
               IRMO=IRANK(JT)
               XMO=XTMO(JT)
               DO 870 J=1,9
                  IF(J.LE.5) THEN
                     DO 860 LINE=1,I-N-NR
                        P(MSTU(4)-MSTU(32)-LINE,J)=P(N+NR+LINE,J)
                        K(MSTU(4)-MSTU(32)-LINE,J)=K(N+NR+LINE,J)
  860                CONTINUE
                  ENDIF
                  INMO(J)=IN(J)
  870          CONTINUE
            ENDIF
         ELSE
C..Reject popcorn system, flag=-1 if enforcing new one
            MSTU(121)=-1
            IF(PTSTMO.GT.RTSTMO) MSTU(121)=-2
         ENDIF
      ENDIF
 
 
C..Lift restoring string outside MOPS block
  880 IF(MSTU(121).LT.0) THEN
         IF(MSTU(121).EQ.-2) MSTU(121)=0
         MSTU(90)=MU90MO
         NRVMO=0
         IF(IRANK(JT).EQ.1.OR.IABS(KFL(JT)).LE.10) GOTO 850
         I=IMO
         KFL(JT)=KFLMO
         PMQ(JT)=PMQMO
         PX(JT)=PXMO
         PY(JT)=PYMO
         GAM(JT)=GAMMO
         IRANK(JT)=IRMO
         XTMO(JT)=XMO
         DO 900 J=1,9
            IF(J.LE.5) THEN
               DO 890 LINE=1,I-N-NR
                  P(N+NR+LINE,J)=P(MSTU(4)-MSTU(32)-LINE,J)
                  K(N+NR+LINE,J)=K(MSTU(4)-MSTU(32)-LINE,J)
  890          CONTINUE
            ENDIF
            IN(J)=INMO(J)
  900    CONTINUE
         GOTO 850
      ENDIF
      XTMO(JT)=XTMO3
C.. MOPS end of modification
 
      DO 910 J=1,3
        IN(J)=IN(3*JT+J)
  910 CONTINUE
 
C...Stepping within or from 'low' string region easy.
      IF(IN(1)+1.EQ.IN(2).AND.Z*P(IN(1)+2,3)*P(IN(2)+2,3)*
     &P(IN(1),5)**2.GE.PR(JT)) THEN
        P(IN(JT)+2,4)=Z*P(IN(JT)+2,3)
        P(IN(JR)+2,4)=PR(JT)/(P(IN(JT)+2,4)*P(IN(1),5)**2)
        DO 920 J=1,4
          P(I,J)=(PX(JT)+PX(3))*P(IN(3),J)+(PY(JT)+PY(3))*P(IN(3)+1,J)
  920   CONTINUE
        GOTO 1010
      ELSEIF(IN(1)+1.EQ.IN(2)) THEN
        P(IN(JR)+2,4)=P(IN(JR)+2,3)
        P(IN(JR)+2,JT)=1D0
        IN(JR)=IN(JR)+4*JS
        IF(JS*IN(JR).GT.JS*IN(4*JR)) GOTO 680
        IF(FOUR(IN(1),IN(2)).LE.1D-2) THEN
          P(IN(JT)+2,4)=P(IN(JT)+2,3)
          P(IN(JT)+2,JT)=0D0
          IN(JT)=IN(JT)+4*JS
        ENDIF
      ENDIF
 
C...Find new transverse directions (i.e. spacelike string vectors).
  930 IF(JS*IN(1).GT.JS*IN(3*JR+1).OR.JS*IN(2).GT.JS*IN(3*JR+2).OR.
     &IN(1).GT.IN(2)) GOTO 680
      IF(IN(1).NE.IN(3*JT+1).OR.IN(2).NE.IN(3*JT+2)) THEN
        DO 940 J=1,4
          DP(1,J)=P(IN(1),J)
          DP(2,J)=P(IN(2),J)
          DP(3,J)=0D0
          DP(4,J)=0D0
  940   CONTINUE
        DP(1,4)=SQRT(DP(1,1)**2+DP(1,2)**2+DP(1,3)**2)
        DP(2,4)=SQRT(DP(2,1)**2+DP(2,2)**2+DP(2,3)**2)
        DHC12=DFOUR(1,2)
        IF(DHC12.LE.1D-2) THEN
          P(IN(JT)+2,4)=P(IN(JT)+2,3)
          P(IN(JT)+2,JT)=0D0
          IN(JT)=IN(JT)+4*JS
          GOTO 930
        ENDIF
        IN(3)=N+NR+4*NS+5
        DP(5,1)=DP(1,1)/DP(1,4)-DP(2,1)/DP(2,4)
        DP(5,2)=DP(1,2)/DP(1,4)-DP(2,2)/DP(2,4)
        DP(5,3)=DP(1,3)/DP(1,4)-DP(2,3)/DP(2,4)
        IF(DP(5,1)**2.LE.DP(5,2)**2+DP(5,3)**2) DP(3,1)=1D0
        IF(DP(5,1)**2.GT.DP(5,2)**2+DP(5,3)**2) DP(3,3)=1D0
        IF(DP(5,2)**2.LE.DP(5,1)**2+DP(5,3)**2) DP(4,2)=1D0
        IF(DP(5,2)**2.GT.DP(5,1)**2+DP(5,3)**2) DP(4,3)=1D0
        DHCX1=DFOUR(3,1)/DHC12
        DHCX2=DFOUR(3,2)/DHC12
        DHCXX=1D0/SQRT(1D0+2D0*DHCX1*DHCX2*DHC12)
        DHCY1=DFOUR(4,1)/DHC12
        DHCY2=DFOUR(4,2)/DHC12
        DHCYX=DHCXX*(DHCX1*DHCY2+DHCX2*DHCY1)*DHC12
        DHCYY=1D0/SQRT(1D0+2D0*DHCY1*DHCY2*DHC12-DHCYX**2)
        DO 950 J=1,4
          DP(3,J)=DHCXX*(DP(3,J)-DHCX2*DP(1,J)-DHCX1*DP(2,J))
          P(IN(3),J)=DP(3,J)
          P(IN(3)+1,J)=DHCYY*(DP(4,J)-DHCY2*DP(1,J)-DHCY1*DP(2,J)-
     &    DHCYX*DP(3,J))
  950   CONTINUE
C...Express pT with respect to new axes, if sensible.
        PXP=-(PX(3)*FOUR(IN(3*JT+3),IN(3))+PY(3)*
     &  FOUR(IN(3*JT+3)+1,IN(3)))
        PYP=-(PX(3)*FOUR(IN(3*JT+3),IN(3)+1)+PY(3)*
     &  FOUR(IN(3*JT+3)+1,IN(3)+1))
        IF(ABS(PXP**2+PYP**2-PX(3)**2-PY(3)**2).LT.0.01D0) THEN
          PX(3)=PXP
          PY(3)=PYP
        ENDIF
      ENDIF
 
C...Sum up known four-momentum. Gives coefficients for m2 expression.
      DO 980 J=1,4
        DHG(J)=0D0
        P(I,J)=PX(JT)*P(IN(3*JT+3),J)+PY(JT)*P(IN(3*JT+3)+1,J)+
     &  PX(3)*P(IN(3),J)+PY(3)*P(IN(3)+1,J)
        DO 960 IN1=IN(3*JT+1),IN(1)-4*JS,4*JS
          P(I,J)=P(I,J)+P(IN1+2,3)*P(IN1,J)
  960   CONTINUE
        DO 970 IN2=IN(3*JT+2),IN(2)-4*JS,4*JS
          P(I,J)=P(I,J)+P(IN2+2,3)*P(IN2,J)
  970   CONTINUE
  980 CONTINUE
      DHM(1)=FOUR(I,I)
      DHM(2)=2D0*FOUR(I,IN(1))
      DHM(3)=2D0*FOUR(I,IN(2))
      DHM(4)=2D0*FOUR(IN(1),IN(2))
 
C...Find coefficients for Gamma expression.
      DO 1000 IN2=IN(1)+1,IN(2),4
        DO 990 IN1=IN(1),IN2-1,4
          DHC=2D0*FOUR(IN1,IN2)
          DHG(1)=DHG(1)+P(IN1+2,JT)*P(IN2+2,JT)*DHC
          IF(IN1.EQ.IN(1)) DHG(2)=DHG(2)-JS*P(IN2+2,JT)*DHC
          IF(IN2.EQ.IN(2)) DHG(3)=DHG(3)+JS*P(IN1+2,JT)*DHC
          IF(IN1.EQ.IN(1).AND.IN2.EQ.IN(2)) DHG(4)=DHG(4)-DHC
  990   CONTINUE
 1000 CONTINUE
 
C...Solve (m2, Gamma) equation system for energies taken.
      DHS1=DHM(JR+1)*DHG(4)-DHM(4)*DHG(JR+1)
      IF(ABS(DHS1).LT.1D-4) GOTO 680
      DHS2=DHM(4)*(GAM(3)-DHG(1))-DHM(JT+1)*DHG(JR+1)-DHG(4)*
     &(P(I,5)**2-DHM(1))+DHG(JT+1)*DHM(JR+1)
      DHS3=DHM(JT+1)*(GAM(3)-DHG(1))-DHG(JT+1)*(P(I,5)**2-DHM(1))
      P(IN(JR)+2,4)=0.5D0*(SQRT(MAX(0D0,DHS2**2-4D0*DHS1*DHS3))/
     &ABS(DHS1)-DHS2/DHS1)
      IF(DHM(JT+1)+DHM(4)*P(IN(JR)+2,4).LE.0D0) GOTO 680
      P(IN(JT)+2,4)=(P(I,5)**2-DHM(1)-DHM(JR+1)*P(IN(JR)+2,4))/
     &(DHM(JT+1)+DHM(4)*P(IN(JR)+2,4))
 
C...Step to new region if necessary.
      IF(P(IN(JR)+2,4).GT.P(IN(JR)+2,3)) THEN
        P(IN(JR)+2,4)=P(IN(JR)+2,3)
        P(IN(JR)+2,JT)=1D0
        IN(JR)=IN(JR)+4*JS
        IF(JS*IN(JR).GT.JS*IN(4*JR)) GOTO 680
        IF(FOUR(IN(1),IN(2)).LE.1D-2) THEN
          P(IN(JT)+2,4)=P(IN(JT)+2,3)
          P(IN(JT)+2,JT)=0D0
          IN(JT)=IN(JT)+4*JS
        ENDIF
        GOTO 930
      ELSEIF(P(IN(JT)+2,4).GT.P(IN(JT)+2,3)) THEN
        P(IN(JT)+2,4)=P(IN(JT)+2,3)
        P(IN(JT)+2,JT)=0D0
        IN(JT)=IN(JT)+4*JS
        GOTO 930
      ENDIF
 
C...Four-momentum of particle. Remaining quantities. Loop back.
 1010 DO 1020 J=1,4
        P(I,J)=P(I,J)+P(IN(1)+2,4)*P(IN(1),J)+P(IN(2)+2,4)*P(IN(2),J)
        P(N+NRS,J)=P(N+NRS,J)-P(I,J)
 1020 CONTINUE
      IF(P(I,4).LT.P(I,5)) GOTO 680
      KFL(JT)=-KFL(3)
      PMQ(JT)=PMQ(3)
      PX(JT)=-PX(3)
      PY(JT)=-PY(3)
      GAM(JT)=GAM(3)
      IF(IN(3).NE.IN(3*JT+3)) THEN
        DO 1030 J=1,4
          P(IN(3*JT+3),J)=P(IN(3),J)
          P(IN(3*JT+3)+1,J)=P(IN(3)+1,J)
 1030   CONTINUE
      ENDIF
      DO 1040 JQ=1,2
        IN(3*JT+JQ)=IN(JQ)
        P(IN(JQ)+2,3)=P(IN(JQ)+2,3)-P(IN(JQ)+2,4)
        P(IN(JQ)+2,JT)=P(IN(JQ)+2,JT)-JS*(3-2*JQ)*P(IN(JQ)+2,4)
 1040 CONTINUE
      GOTO 840
 
C...Final hadron: side, flavour, hadron, mass.
 1050 I=I+1
      K(I,1)=1
      K(I,3)=IE(JR)
      K(I,4)=0
      K(I,5)=0
      CALL PYKFDI(KFL(JR),-KFL(3),KFLDMP,K(I,2))
      IF(K(I,2).EQ.0) GOTO 680
      P(I,5)=PYMASS(K(I,2))
      PR(JR)=P(I,5)**2+(PX(JR)-PX(3))**2+(PY(JR)-PY(3))**2
 
C...Final two hadrons: find common setup of four-vectors.
      JQ=1
      IF(P(IN(4)+2,3)*P(IN(5)+2,3)*FOUR(IN(4),IN(5)).LT.
     &P(IN(7)+2,3)*P(IN(8)+2,3)*FOUR(IN(7),IN(8))) JQ=2
      DHC12=FOUR(IN(3*JQ+1),IN(3*JQ+2))
      DHR1=FOUR(N+NRS,IN(3*JQ+2))/DHC12
      DHR2=FOUR(N+NRS,IN(3*JQ+1))/DHC12
      IF(IN(4).NE.IN(7).OR.IN(5).NE.IN(8)) THEN
        PX(3-JQ)=-FOUR(N+NRS,IN(3*JQ+3))-PX(JQ)
        PY(3-JQ)=-FOUR(N+NRS,IN(3*JQ+3)+1)-PY(JQ)
        PR(3-JQ)=P(I+(JT+JQ-3)**2-1,5)**2+(PX(3-JQ)+(2*JQ-3)*JS*
     &  PX(3))**2+(PY(3-JQ)+(2*JQ-3)*JS*PY(3))**2
      ENDIF
 
C...Solve kinematics for final two hadrons, if possible.
      WREM2=2D0*DHR1*DHR2*DHC12
      FD=(SQRT(PR(1))+SQRT(PR(2)))/SQRT(WREM2)
      IF(MJU(1)+MJU(2).NE.0.AND.I.EQ.ISAV+2.AND.FD.GE.1D0) GOTO 200
      IF(FD.GE.1D0) GOTO 680
      FA=WREM2+PR(JT)-PR(JR)
      FB=SQRT(MAX(0D0,FA**2-4D0*WREM2*PR(JT)))
      PREVCF=PARJ(42)
      IF(MSTJ(11).EQ.2) PREVCF=PARJ(39)
      PREV=1D0/(1D0+EXP(MIN(50D0,PREVCF*FB)))
      FB=SIGN(FB,JS*(PYR(0)-PREV))
      KFL1A=IABS(KFL(1))
      KFL2A=IABS(KFL(2))
      IF(MAX(MOD(KFL1A,10),MOD(KFL1A/1000,10),MOD(KFL2A,10),
     &MOD(KFL2A/1000,10)).GE.6) FB=SIGN(SQRT(MAX(0D0,FA**2-
     &4D0*WREM2*PR(JT))),DBLE(JS))
      DO 1060 J=1,4
        P(I-1,J)=(PX(JT)+PX(3))*P(IN(3*JQ+3),J)+(PY(JT)+PY(3))*
     &  P(IN(3*JQ+3)+1,J)+0.5D0*(DHR1*(FA+FB)*P(IN(3*JQ+1),J)+
     &  DHR2*(FA-FB)*P(IN(3*JQ+2),J))/WREM2
        P(I,J)=P(N+NRS,J)-P(I-1,J)
 1060 CONTINUE
      IF(P(I-1,4).LT.P(I-1,5).OR.P(I,4).LT.P(I,5)) GOTO 680
      DM2F1=P(I-1,4)**2-P(I-1,1)**2-P(I-1,2)**2-P(I-1,3)**2-P(I-1,5)**2
      DM2F2=P(I,4)**2-P(I,1)**2-P(I,2)**2-P(I,3)**2-P(I,5)**2
      IF(DM2F1.GT.1D-10*P(I-1,4)**2.OR.DM2F2.GT.1D-10*P(I,4)**2) THEN
        NTRYFN=NTRYFN+1
        IF(NTRYFN.LT.100) GOTO 140
        CALL PYERRM(13,'(PYSTRF:) bad energies for final two hadrons')
      ENDIF
 
C...Mark jets as fragmented and give daughter pointers.
      N=I-NRS+1
      DO 1070 I=NSAV+1,NSAV+NP
        IM=K(I,3)
        K(IM,1)=K(IM,1)+10
        IF(MSTU(16).NE.2) THEN
          K(IM,4)=NSAV+1
          K(IM,5)=NSAV+1
        ELSE
          K(IM,4)=NSAV+2
          K(IM,5)=N
        ENDIF
 1070 CONTINUE
 
C...Document string system. Move up particles.
      NSAV=NSAV+1
      K(NSAV,1)=11
      K(NSAV,2)=92
      K(NSAV,3)=IP
      K(NSAV,4)=NSAV+1
      K(NSAV,5)=N
      DO 1080 J=1,4
        P(NSAV,J)=DPS(J)
        V(NSAV,J)=V(IP,J)
 1080 CONTINUE
      P(NSAV,5)=SQRT(MAX(0D0,DPS(4)**2-DPS(1)**2-DPS(2)**2-DPS(3)**2))
      V(NSAV,5)=0D0
      DO 1100 I=NSAV+1,N
        DO 1090 J=1,5
          K(I,J)=K(I+NRS-1,J)
          P(I,J)=P(I+NRS-1,J)
          V(I,J)=0D0
 1090   CONTINUE
 1100 CONTINUE
      MSTU91=MSTU(90)
      DO 1110 IZ=MSTU90+1,MSTU91
        MSTU9T(IZ)=MSTU(90+IZ)-NRS+1-NSAV+N
        PARU9T(IZ)=PARU(90+IZ)
 1110 CONTINUE
      MSTU(90)=MSTU90
 
C...Order particles in rank along the chain. Update mother pointer.
      DO 1130 I=NSAV+1,N
        DO 1120 J=1,5
          K(I-NSAV+N,J)=K(I,J)
          P(I-NSAV+N,J)=P(I,J)
 1120   CONTINUE
 1130 CONTINUE
      I1=NSAV
      DO 1160 I=N+1,2*N-NSAV
        IF(K(I,3).NE.IE(1)) GOTO 1160
        I1=I1+1
        DO 1140 J=1,5
          K(I1,J)=K(I,J)
          P(I1,J)=P(I,J)
 1140   CONTINUE
        IF(MSTU(16).NE.2) K(I1,3)=NSAV
        DO 1150 IZ=MSTU90+1,MSTU91
          IF(MSTU9T(IZ).EQ.I) THEN
            MSTU(90)=MSTU(90)+1
            MSTU(90+MSTU(90))=I1
            PARU(90+MSTU(90))=PARU9T(IZ)
          ENDIF
 1150   CONTINUE
 1160 CONTINUE
      DO 1190 I=2*N-NSAV,N+1,-1
        IF(K(I,3).EQ.IE(1)) GOTO 1190
        I1=I1+1
        DO 1170 J=1,5
          K(I1,J)=K(I,J)
          P(I1,J)=P(I,J)
 1170   CONTINUE
        IF(MSTU(16).NE.2) K(I1,3)=NSAV
        DO 1180 IZ=MSTU90+1,MSTU91
          IF(MSTU9T(IZ).EQ.I) THEN
            MSTU(90)=MSTU(90)+1
            MSTU(90+MSTU(90))=I1
            PARU(90+MSTU(90))=PARU9T(IZ)
          ENDIF
 1180   CONTINUE
 1190 CONTINUE
 
C...Boost back particle system. Set production vertices.
      IF(MBST.EQ.0) THEN
        MSTU(33)=1
        CALL PYROBO(NSAV+1,N,0D0,0D0,DPS(1)/DPS(4),DPS(2)/DPS(4),
     &  DPS(3)/DPS(4))
      ELSE
        DO 1200 I=NSAV+1,N
          HHPMT=P(I,1)**2+P(I,2)**2+P(I,5)**2
          IF(P(I,3).GT.0D0) THEN
            HHPEZ=(P(I,4)+P(I,3))*HHBZ
            P(I,3)=0.5D0*(HHPEZ-HHPMT/HHPEZ)
            P(I,4)=0.5D0*(HHPEZ+HHPMT/HHPEZ)
          ELSE
            HHPEZ=(P(I,4)-P(I,3))/HHBZ
            P(I,3)=-0.5D0*(HHPEZ-HHPMT/HHPEZ)
            P(I,4)=0.5D0*(HHPEZ+HHPMT/HHPEZ)
          ENDIF
 1200   CONTINUE
      ENDIF
      DO 1220 I=NSAV+1,N
        DO 1210 J=1,4
          V(I,J)=V(IP,J)
 1210   CONTINUE
 1220 CONTINUE
 
      RETURN
      END
