 
C*********************************************************************
 
C...PYPREP
C...Rearranges partons along strings.
C...Allows small systems to collapse into one or two particles.
C...Checks flavours and colour singlet invariant masses.
 
      SUBROUTINE PYPREP(IP)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYDAT3/
C...Local arrays.
      DIMENSION DPS(5),DPC(5),UE(3),PG(5),E1(3),E2(3),E3(3),E4(3),
     &ECL(3),IJUNC(10,0:4),IPIECE(30,0:4),KFEND(4),KFQ(4),
     &IJUR(4),PJU(4,6),IRNG(4,2)
 
C...Function to give four-product.
      FOUR(I,J)=P(I,4)*P(J,4)-P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)
 
C...Rearrange parton shower product listing along strings: begin loop.
      NOLD=N
      I1=N
      NJUNC=0
      NPIECE=0
      NJJSTR=0
      MSTU32=MSTU(32)+1
      DO 170 MQGST=1,3
        DO 160 I=MAX(1,IP),N
 
C...Special treatment for junctions
          IF(K(I,1).EQ.42) THEN
C...First, just store positions
            IF (MQGST.EQ.1) THEN
              NJUNC=NJUNC+1
              IJUNC(NJUNC,0)=I
              IJUNC(NJUNC,4)=0
C...Then look for junction-junction strings (not detected in the
C...main search below).
            ELSE IF (MQGST.EQ.2.AND.NPIECE.NE.3*NJUNC) THEN
              IF (NJJSTR.EQ.0) THEN
                NJJSTR = (3*NJUNC-NPIECE)/2
              ENDIF
C...Check how many already identified strings end on this junction
              ILC=0
              DO 100 J=1,NPIECE
                IF (IPIECE(J,4).EQ.I) ILC=ILC+1
  100         CONTINUE
C...If only 2, third one must be to another junction
              IF (ILC.EQ.2) THEN
C...The colour information in the junction is unreadable for the
C...colour space search further down in this routine, so we must
C...start on the colour mother of this junction and then "artificially"
C...prevent the colour mother from connecting here again.
                IA=MOD(K(I,4),MSTU(5))
                KCS=4
                IF (MOD(MOD(K(I,4)/MSTU(5),MSTU(5)),2).EQ.1) KCS=5
                K(IA,KCS)   = K(IA,KCS) + MSTU(5)**2
                K(I,KCS)    = K(I,KCS) + 2*MSTU(5)**2
                I1BEG = I1
                NSTP = 0
                GOTO 150
              ELSE IF (ILC.NE.3) THEN
C...This could happen if 2 legs of a junction connect to other
C...junctions.
                CALL PYERRM(12,
     &          '(PYPREP:) Too many junction-junction strings.')
              ENDIF
            ENDIF
          ENDIF
 
C...Look for coloured string endpoint, or (later) leftover gluon.
          IF(K(I,1).NE.3) GOTO 160
          KC=PYCOMP(K(I,2))
          IF(KC.EQ.0) GOTO 160
          KQ=KCHG(KC,2)
          IF(KQ.EQ.0.OR.(MQGST.LE.2.AND.KQ.EQ.2)) GOTO 160
 
C...Pick up loose string end.
          KCS=4
          IF(KQ*ISIGN(1,K(I,2)).LT.0) KCS=5
          IA=I
          IB=I
          I1BEG=I1
          NSTP=0
  110     NSTP=NSTP+1
          IF(NSTP.GT.4*N) THEN
            CALL PYERRM(14,'(PYPREP:) caught in infinite loop')
            RETURN
          ENDIF
 
C...Copy undecayed parton. Finished if reached string endpoint.
          IF(K(IA,1).EQ.3) THEN
            IF(I1.GE.MSTU(4)-MSTU32-5) THEN
              CALL PYERRM(11,'(PYPREP:) no more memory left in PYJETS')
              RETURN
            ENDIF
            I1=I1+1
            K(I1,1)=2
            IF(NSTP.GE.2.AND.KCHG(PYCOMP(K(IA,2)),2).NE.2) K(I1,1)=1
            K(I1,2)=K(IA,2)
            K(I1,3)=IA
            K(I1,4)=0
            K(I1,5)=0
            DO 120 J=1,5
              P(I1,J)=P(IA,J)
              V(I1,J)=V(IA,J)
  120       CONTINUE
            K(IA,1)=K(IA,1)+10
            IF(K(I1,1).EQ.1) GOTO 160
          ENDIF
 
C...Also finished (for now) if reached junction; then copy to end.
          IF(K(IA,1).EQ.42) THEN
            NCOPY=I1-I1BEG
            IF(I1.GE.MSTU(4)-MSTU32-NCOPY-5) THEN
              CALL PYERRM(11,'(PYPREP:) no more memory left in PYJETS')
              RETURN
            ENDIF
            IF (MQGST.LE.2.AND.NCOPY.NE.0) THEN
              DO 140 ICOPY=1,NCOPY
                DO 130 J=1,5
                  K(MSTU(4)-MSTU32-ICOPY,J)=K(I1BEG+ICOPY,J)
                  P(MSTU(4)-MSTU32-ICOPY,J)=P(I1BEG+ICOPY,J)
                  V(MSTU(4)-MSTU32-ICOPY,J)=V(I1BEG+ICOPY,J)
  130           CONTINUE
  140         CONTINUE
            ENDIF
            NPIECE=NPIECE+1
            IPIECE(NPIECE,0)=I
            IPIECE(NPIECE,1)=MSTU32+1
            IPIECE(NPIECE,2)=MSTU32+NCOPY
            IPIECE(NPIECE,3)=IB
            IPIECE(NPIECE,4)=IA
            MSTU32=MSTU32+NCOPY
            I1=I1BEG
            GOTO 160
          ENDIF
 
C...GOTO next parton in colour space.
  150     IB=IA
          IF(MOD(K(IB,KCS)/MSTU(5)**2,2).EQ.0.AND.MOD(K(IB,KCS),MSTU(5))
     &    .NE.0) THEN
            IA=MOD(K(IB,KCS),MSTU(5))
            K(IB,KCS)=K(IB,KCS)+MSTU(5)**2
            MREV=0
          ELSE
            IF(K(IB,KCS).GE.2*MSTU(5)**2.OR.MOD(K(IB,KCS)/MSTU(5),
     &      MSTU(5)).EQ.0) KCS=9-KCS
            IA=MOD(K(IB,KCS)/MSTU(5),MSTU(5))
            K(IB,KCS)=K(IB,KCS)+2*MSTU(5)**2
            MREV=1
          ENDIF
          IF(IA.LE.0.OR.IA.GT.N) THEN
            CALL PYERRM(12,'(PYPREP:) colour rearrangement failed')
            RETURN
          ENDIF
          IF(MOD(K(IA,4)/MSTU(5),MSTU(5)).EQ.IB.OR.MOD(K(IA,5)/MSTU(5),
     &    MSTU(5)).EQ.IB) THEN
            IF(MREV.EQ.1) KCS=9-KCS
            IF(MOD(K(IA,KCS)/MSTU(5),MSTU(5)).NE.IB) KCS=9-KCS
            K(IA,KCS)=K(IA,KCS)+2*MSTU(5)**2
          ELSE
            IF(MREV.EQ.0) KCS=9-KCS
            IF(MOD(K(IA,KCS),MSTU(5)).NE.IB) KCS=9-KCS
            K(IA,KCS)=K(IA,KCS)+MSTU(5)**2
          ENDIF
          IF(IA.NE.I) GOTO 110
          K(I1,1)=1
  160   CONTINUE
  170 CONTINUE
 
C...Junction systems remain.
      IJU=0
      IJUS=0
      IJUCNT=0
      MREV=0
      IJJSTR=0
  180 IJUCNT=IJUCNT+1
      IF (IJUCNT.LE.NJUNC) THEN
C...If we are not processing a j-j string, treat this junction as new.
        IF (IJJSTR.EQ.0) THEN
          IJU=IJUNC(IJUCNT,0)
          MREV=0
C...If junction has already been read, ignore it.
          IF (IJUNC(IJUCNT,4).EQ.1) GOTO 180
C...If we are on a j-j string, goto second j-j junction.
        ELSE
          IJUCNT=IJUCNT-1
          IJU=IJUS
        ENDIF
C...Mark selected junction read.
        DO 190 J=1,NJUNC
          IF (IJUNC(J,0).EQ.IJU) IJUNC(J,4)=1
  190   CONTINUE
 
C...Determine junction type
        ITJUNC = MOD(K(IJU,4)/MSTU(5),MSTU(5))
C...Type 1 and 2 junctions: ~chi -> q q q, ~chi -> qbar,qbar,qbar
C...Type 3 and 4 junctions: ~qbar -> q q , ~q -> qbar qbar
C...Type 5 and 6 junctions: ~g -> q q q, ~g -> qbar qbar qbar
        IF (ITJUNC.GE.1.AND.ITJUNC.LE.6) THEN
          IHK=0
  200     IHK=IHK+1
C...Find which quarks belong to given junction.
          IF(IHK.EQ.1) IEND=MOD(K(IJU,5),MSTU(5))
          IF(IHK.EQ.2) IEND=MOD(K(IJU,5)/MSTU(5),MSTU(5))
C...IHK = 3 is special. Either normal string piece, or j-j string.
          IF(IHK.EQ.3) THEN
            IEND=MOD(K(IJU,4),MSTU(5))
            IF (MREV.NE.1) THEN
              DO 210 IPC=1,NPIECE
C...If there is a j-j string starting on the present junction which has
C...zero length, insert next junction immediately.
                IF (IPIECE(IPC,0).EQ.IJU.AND.K(IPIECE(IPC,4),1)
     &          .EQ.42.AND.IPIECE(IPC,1)-1-IPIECE(IPC,2).EQ.0) THEN
                  IJJSTR = 1
                  GOTO 250
                ENDIF
  210         CONTINUE
              MREV = 1
C...If MREV is 1 and IHK is 3 we are finished with this system.
            ELSE
              MREV=0
              GOTO 180
            ENDIF
          ENDIF
 
C...If we've gotten this far, then either IHK < 3, or
C...an interjunction string exists, or just a third normal string.
          IJUNC(IJUCNT,IHK)=0
          IJJSTR = 0
C..Order pieces belonging to this junction. Also look for j-j.
          DO 220 IPC=1,NPIECE
            IF (IPIECE(IPC,3).EQ.IEND) IJUNC(IJUCNT,IHK)=IPC
            IF (IHK.EQ.3.AND.IPIECE(IPC,0).EQ.IJUNC(IJUCNT,0)
     &      .AND.K(IPIECE(IPC,4),1).EQ.42) THEN
              IJUNC(IJUCNT,IHK)=IPC
              IJJSTR = 1
              MREV = 0
            ENDIF
  220     CONTINUE
C...Copy back chains in proper order. MREV=0/1 : descending/ascending
          IPC=IJUNC(IJUCNT,IHK)
          DO 240 ICP=IPIECE(IPC,1+MREV),IPIECE(IPC,2-MREV),1-2*MREV
            I1=I1+1
            DO 230 J=1,5
              K(I1,J)=K(MSTU(4)-ICP,J)
              P(I1,J)=P(MSTU(4)-ICP,J)
              V(I1,J)=V(MSTU(4)-ICP,J)
  230       CONTINUE
  240     CONTINUE
          K(I1,1)=2
C...Mark last quark.
          IF (MREV.EQ.1.AND.IHK.GE.2) K(I1,1)=1
C...Do not insert junctions at wrong places.
          IF(IHK.LT.2.OR.MREV.NE.0) GOTO 270
C...Insert junction.
  250     IJUS = IJU
          IF (IHK.EQ.3) THEN
C...Shift to end junction if a j-j string has been processed.
            IF (IJJSTR.NE.0) IJUS = IPIECE(IPC,4)
            MREV= 1
          ENDIF
          I1=I1+1
          DO 260 J=1,5
            K(I1,J)=0
            P(I1,J)=0.
            V(I1,J)=0.
  260     CONTINUE
          K(I1,1)=41
          K(IJUS,1)=K(IJUS,1)+10
          K(I1,2)=K(IJUS,2)
          K(I1,3)=K(IJUS,3)
  270     IF (IHK.LT.3) GOTO 200
        ELSE
          CALL PYERRM(12,'(PYPREP:) Unknown junction type')
        ENDIF
        IF (IJUCNT.NE.NJUNC) GOTO 180
      ENDIF
      N=I1
 
C...Rearrange three strings from junction, e.g. in case one has been
C...shortened by shower, so the last is the largest-energy one.
      IF(NJUNC.GE.1) THEN
C...Find systems with exactly one junction.
        MJUN1=0
        NBEG=NOLD+1
        DO 380 I=NOLD+1,N
          IF(K(I,1).NE.1.AND.K(I,1).NE.41) THEN
          ELSEIF(K(I,1).EQ.41) THEN
            MJUN1=MJUN1+1
          ELSEIF(K(I,1).EQ.1.AND.MJUN1.NE.1) THEN
            MJUN1=0
            NBEG=I+1
          ELSE
            NEND=I
C...Sum up energy-momentum in each junction string.
            DO 280 J=1,5
              PJU(1,J)=0D0
              PJU(2,J)=0D0
              PJU(3,J)=0D0
  280       CONTINUE
            NJU=0
            DO 300 I1=NBEG,NEND
              IF(K(I1,2).NE.21) THEN
                NJU=NJU+1
                IJUR(NJU)=I1
              ENDIF
              DO 290 J=1,5
                PJU(MIN(NJU,3),J)=PJU(MIN(NJU,3),J)+P(I1,J)
  290         CONTINUE
  300       CONTINUE
C...Find which of them has highest energy (minus mass) in rest frame.
            DO 310 J=1,5
              PJU(4,J)=PJU(1,J)+PJU(2,J)+PJU(3,J)
  310       CONTINUE
            PMJU=SQRT(MAX(0D0,PJU(4,4)**2-PJU(4,1)**2-PJU(4,2)**2-
     &      PJU(4,3)**2))
            DO 320 I2=1,3
              PJU(I2,6)=(PJU(4,4)*PJU(I2,4)-PJU(4,1)*PJU(I2,1)-
     &        PJU(4,2)*PJU(I2,2)-PJU(4,3)*PJU(I2,3))/PMJU-PJU(I2,5)
  320       CONTINUE
            IF(PJU(3,6).LT.MIN(PJU(1,6),PJU(2,6))) THEN
C...Decide how to rearrange so that new last has highest energy.
              IF(PJU(1,6).LT.PJU(2,6)) THEN
                IRNG(1,1)=IJUR(1)
                IRNG(1,2)=IJUR(2)-1
                IRNG(2,1)=IJUR(4)
                IRNG(2,2)=IJUR(3)+1
                IRNG(4,1)=IJUR(3)-1
                IRNG(4,2)=IJUR(2)
              ELSE
                IRNG(1,1)=IJUR(4)
                IRNG(1,2)=IJUR(3)+1
                IRNG(2,1)=IJUR(2)
                IRNG(2,2)=IJUR(3)-1
                IRNG(4,1)=IJUR(2)-1
                IRNG(4,2)=IJUR(1)
              ENDIF
              IRNG(3,1)=IJUR(3)
              IRNG(3,2)=IJUR(3)
C...Copy in correct order below bottom of current event record.
              I2=N
              DO 350 II=1,4
                DO 340 I1=IRNG(II,1),IRNG(II,2),
     &          ISIGN(1,IRNG(II,2)-IRNG(II,1))
                  I2=I2+1
                  DO 330 J=1,5
                    K(I2,J)=K(I1,J)
                    P(I2,J)=P(I1,J)
                    V(I2,J)=V(I1,J)
  330             CONTINUE
                  IF(K(I2,1).EQ.1) K(I2,1)=2
  340           CONTINUE
  350         CONTINUE
              K(I2,1)=1
C...Copy back up, overwriting but now in correct order.
              DO 370 I1=NBEG,NEND
                I2=I1-NBEG+N+1
                DO 360 J=1,5
                  K(I1,J)=K(I2,J)
                  P(I1,J)=P(I2,J)
                  V(I1,J)=V(I2,J)
  360           CONTINUE
  370         CONTINUE
            ENDIF
            MJUN1=0
            NBEG=I+1
          ENDIF
  380   CONTINUE
      ENDIF
 
C...Done if no checks on small-mass systems.
      IF(MSTJ(14).LT.0) RETURN
      IF(MSTJ(14).EQ.0) GOTO 820
 
C...Find lowest-mass colour singlet jet system.
      NS=N
  390 NSIN=N-NS
      PDMIN=1D0+PARJ(32)
      IC=0
      DO 450 I=MAX(1,IP),N
        IF(K(I,1).NE.1.AND.K(I,1).NE.2) THEN
        ELSEIF(K(I,1).EQ.2.AND.IC.EQ.0) THEN
          NSIN=NSIN+1
          IC=I
          DO 400 J=1,4
            DPS(J)=P(I,J)
  400     CONTINUE
          MSTJ(93)=1
          DPS(5)=PYMASS(K(I,2))
        ELSEIF(K(I,1).EQ.2.AND.K(I,2).NE.21) THEN
          DO 410 J=1,4
            DPS(J)=DPS(J)+P(I,J)
  410     CONTINUE
          MSTJ(93)=1
          DPS(5)=DPS(5)+PYMASS(K(I,2))
        ELSEIF(K(I,1).EQ.2) THEN
          DO 420 J=1,4
            DPS(J)=DPS(J)+P(I,J)
  420     CONTINUE
        ELSEIF(IC.NE.0.AND.KCHG(PYCOMP(K(I,2)),2).NE.0) THEN
          DO 430 J=1,4
            DPS(J)=DPS(J)+P(I,J)
  430     CONTINUE
          MSTJ(93)=1
          DPS(5)=DPS(5)+PYMASS(K(I,2))
          PD=SQRT(MAX(0D0,DPS(4)**2-DPS(1)**2-DPS(2)**2-DPS(3)**2))-
     &    DPS(5)
          IF(PD.LT.PDMIN) THEN
            PDMIN=PD
            DO 440 J=1,5
              DPC(J)=DPS(J)
  440       CONTINUE
            IC1=IC
            IC2=I
          ENDIF
          IC=0
        ELSE
          NSIN=NSIN+1
        ENDIF
  450 CONTINUE
 
C...Done if lowest-mass system above threshold for string frag.
      IF(PDMIN.GE.PARJ(32)) GOTO 820
 
C...Fill small-mass system as cluster.
      NSAV=N
      PECM=SQRT(MAX(0D0,DPC(4)**2-DPC(1)**2-DPC(2)**2-DPC(3)**2))
      K(N+1,1)=11
      K(N+1,2)=91
      K(N+1,3)=IC1
      P(N+1,1)=DPC(1)
      P(N+1,2)=DPC(2)
      P(N+1,3)=DPC(3)
      P(N+1,4)=DPC(4)
      P(N+1,5)=PECM
 
C...Set up history, assuming cluster -> 2 hadrons.
      NBODY=2
      K(N+1,4)=N+2
      K(N+1,5)=N+3
      K(N+2,1)=1
      K(N+3,1)=1
      IF(MSTU(16).NE.2) THEN
        K(N+2,3)=N+1
        K(N+3,3)=N+1
      ELSE
        K(N+2,3)=IC1
        K(N+3,3)=IC2
      ENDIF
      K(N+2,4)=0
      K(N+3,4)=0
      K(N+2,5)=0
      K(N+3,5)=0
      V(N+1,5)=0D0
      V(N+2,5)=0D0
      V(N+3,5)=0D0
 
C...Find total flavour content - complicated by presence of junctions.
      NQ=0
      NDIQ=0
      DO 460 I=IC1,IC2
        IF((K(I,1).EQ.1.OR.K(I,1).EQ.2).AND.K(I,2).NE.21) THEN
          NQ=NQ+1
          KFQ(NQ)=K(I,2)
          IF(IABS(K(I,2)).GT.1000) NDIQ=NDIQ+1
        ENDIF
  460 CONTINUE
 
C...If several diquarks, split up one to give even number of flavours.
      IF(NQ.EQ.3.AND.NDIQ.GE.2) THEN
        I1=3
        IF(IABS(KFQ(3)).LT.1000) I1=1
        KFQ(4)=ISIGN(MOD(IABS(KFQ(I1))/100,10),KFQ(I1))
        KFQ(I1)=KFQ(I1)/1000
        NQ=4
        NDIQ=NDIQ-1
      ENDIF
 
C...If four quark ends, join two to diquark.
      IF(NQ.EQ.4.AND.NDIQ.EQ.0) THEN
        I1=1
        I2=2
        IF(KFQ(I1)*KFQ(I2).LT.0) I2=3
        IF(I2.EQ.3.AND.KFQ(I1)*KFQ(I2).LT.0) I2=4
        KFLS=2*INT(PYR(0)+3D0*PARJ(4)/(1D0+3D0*PARJ(4)))+1
        IF(KFQ(I1).EQ.KFQ(I2)) KFLS=3
        KFQ(I1)=ISIGN(1000*MAX(IABS(KFQ(I1)),IABS(KFQ(I2)))+
     &  100*MIN(IABS(KFQ(I1)),IABS(KFQ(I2)))+KFLS,KFQ(I1))
        KFQ(I2)=KFQ(4)
        NQ=3
        NDIQ=1
      ENDIF
 
C...If two quark ends, plus quark or diquark, join quarks to diquark.
      IF(NQ.EQ.3) THEN
        I1=1
        I2=2
        IF(IABS(KFQ(I1)).GT.1000) I1=3
        IF(IABS(KFQ(I2)).GT.1000) I2=3
        KFLS=2*INT(PYR(0)+3D0*PARJ(4)/(1D0+3D0*PARJ(4)))+1
        IF(KFQ(I1).EQ.KFQ(I2)) KFLS=3
        KFQ(I1)=ISIGN(1000*MAX(IABS(KFQ(I1)),IABS(KFQ(I2)))+
     &  100*MIN(IABS(KFQ(I1)),IABS(KFQ(I2)))+KFLS,KFQ(I1))
        KFQ(I2)=KFQ(3)
        NQ=2
        NDIQ=NDIQ+1
      ENDIF
 
C...Form two particles from flavours of lowest-mass system, if feasible.
      NTRY = 0
  470 NTRY = NTRY + 1
 
C...Open string with two specified endpoint flavours.
      IF(NQ.EQ.2) THEN
        KC1=PYCOMP(KFQ(1))
        KC2=PYCOMP(KFQ(2))
        IF(KC1.EQ.0.OR.KC2.EQ.0) GOTO 820
        KQ1=KCHG(KC1,2)*ISIGN(1,KFQ(1))
        KQ2=KCHG(KC2,2)*ISIGN(1,KFQ(2))
        IF(KQ1+KQ2.NE.0) GOTO 820
C...Start with qq, if there is one. Only allow for rank 1 popcorn meson
  480   K1=KFQ(1)
        IF(IABS(KFQ(2)).GT.1000) K1=KFQ(2)
        MSTU(125)=0
        CALL PYDCYK(K1,0,KFLN,K(N+2,2))
        CALL PYDCYK(KFQ(1)+KFQ(2)-K1,-KFLN,KFLDMP,K(N+3,2))
        IF(K(N+2,2).EQ.0.OR.K(N+3,2).EQ.0) GOTO 480
 
C...Open string with four specified flavours.
      ELSEIF(NQ.EQ.4) THEN
        KC1=PYCOMP(KFQ(1))
        KC2=PYCOMP(KFQ(2))
        KC3=PYCOMP(KFQ(3))
        KC4=PYCOMP(KFQ(4))
        IF(KC1.EQ.0.OR.KC2.EQ.0.OR.KC3.EQ.0.OR.KC4.EQ.0) GOTO 820
        KQ1=KCHG(KC1,2)*ISIGN(1,KFQ(1))
        KQ2=KCHG(KC2,2)*ISIGN(1,KFQ(2))
        KQ3=KCHG(KC3,2)*ISIGN(1,KFQ(3))
        KQ4=KCHG(KC4,2)*ISIGN(1,KFQ(4))
        IF(KQ1+KQ2+KQ3+KQ4.NE.0) GOTO 820
C...Combine flavours pairwise to form two hadrons.
  490   I1=1
        I2=2
        IF(KQ1*KQ2.GT.0.OR.(IABS(KFQ(1)).GT.1000.AND.
     &  IABS(KFQ(2)).GT.1000)) I2=3
        IF(I2.EQ.3.AND.(KQ1*KQ3.GT.0.OR.(IABS(KFQ(1)).GT.1000.AND.
     &  IABS(KFQ(3)).GT.1000))) I2=4
        I3=3
        IF(I2.EQ.3) I3=2
        I4=10-I1-I2-I3
        CALL PYDCYK(KFQ(I1),KFQ(I2),KFLDMP,K(N+2,2))
        CALL PYDCYK(KFQ(I3),KFQ(I4),KFLDMP,K(N+3,2))
        IF(K(N+2,2).EQ.0.OR.K(N+3,2).EQ.0) GOTO 490
 
C...Closed string.
      ELSE
        IF(IABS(K(IC2,2)).NE.21) GOTO 820
C...No room for popcorn mesons in closed string -> 2 hadrons.
        MSTU(125)=0
  500   CALL PYDCYK(1+INT((2D0+PARJ(2))*PYR(0)),0,KFLN,KFDMP)
        CALL PYDCYK(KFLN,0,KFLM,K(N+2,2))
        CALL PYDCYK(-KFLN,-KFLM,KFLDMP,K(N+3,2))
        IF(K(N+2,2).EQ.0.OR.K(N+3,2).EQ.0) GOTO 500
      ENDIF
      P(N+2,5)=PYMASS(K(N+2,2))
      P(N+3,5)=PYMASS(K(N+3,2))
 
C...If it does not work: try again (a number of times), give up (if no
C...place to shuffle momentum or too many flavours), or form one hadron.
      IF(P(N+2,5)+P(N+3,5)+PARJ(64).GE.PECM) THEN
        IF(NTRY.LT.MSTJ(17).OR.(NQ.EQ.4.AND.NTRY.LT.5*MSTJ(17))) THEN
          GOTO 470
        ELSEIF(NSIN.EQ.1.OR.NQ.EQ.4) THEN
          GOTO 820
        ELSE
          GOTO 570
        END IF
      END IF
 
C...Perform two-particle decay of jet system.
C...First step: find reference axis in decaying system rest frame.
C...(Borrow slot N+2 for temporary direction.)
      DO 510 J=1,4
        P(N+2,J)=P(IC1,J)
  510 CONTINUE
      DO 530 I=IC1+1,IC2-1
        IF((K(I,1).EQ.1.OR.K(I,1).EQ.2).AND.
     &  KCHG(PYCOMP(K(I,2)),2).NE.0) THEN
          FRAC1=FOUR(IC2,I)/(FOUR(IC1,I)+FOUR(IC2,I))
          DO 520 J=1,4
            P(N+2,J)=P(N+2,J)+FRAC1*P(I,J)
  520     CONTINUE
        ENDIF
  530 CONTINUE
      CALL PYROBO(N+2,N+2,0D0,0D0,-DPC(1)/DPC(4),-DPC(2)/DPC(4),
     &-DPC(3)/DPC(4))
      THE1=PYANGL(P(N+2,3),SQRT(P(N+2,1)**2+P(N+2,2)**2))
      PHI1=PYANGL(P(N+2,1),P(N+2,2))
 
C...Second step: generate isotropic/anisotropic decay.
      PA=SQRT((PECM**2-(P(N+2,5)+P(N+3,5))**2)*(PECM**2-
     &(P(N+2,5)-P(N+3,5))**2))/(2D0*PECM)
  540 UE(3)=PYR(0)
      IF(PARJ(21).LE.0.01D0) UE(3)=1D0
      PT2=(1D0-UE(3)**2)*PA**2
      IF(MSTJ(16).LE.0) THEN
        PREV=0.5D0
      ELSE
        IF(EXP(-PT2/(2D0*MAX(0.01D0,PARJ(21))**2)).LT.PYR(0)) GOTO 540
        PR1=P(N+2,5)**2+PT2
        PR2=P(N+3,5)**2+PT2
        ALAMBD=SQRT(MAX(0D0,(PECM**2-PR1-PR2)**2-4D0*PR1*PR2))
        PREVCF=PARJ(42)
        IF(MSTJ(11).EQ.2) PREVCF=PARJ(39)
        PREV=1D0/(1D0+EXP(MIN(50D0,PREVCF*ALAMBD)))
      ENDIF
      IF(PYR(0).LT.PREV) UE(3)=-UE(3)
      PHI=PARU(2)*PYR(0)
      UE(1)=SQRT(1D0-UE(3)**2)*COS(PHI)
      UE(2)=SQRT(1D0-UE(3)**2)*SIN(PHI)
      DO 550 J=1,3
        P(N+2,J)=PA*UE(J)
        P(N+3,J)=-PA*UE(J)
  550 CONTINUE
      P(N+2,4)=SQRT(PA**2+P(N+2,5)**2)
      P(N+3,4)=SQRT(PA**2+P(N+3,5)**2)
 
C...Third step: move back to event frame and set production vertex.
      CALL PYROBO(N+2,N+3,THE1,PHI1,DPC(1)/DPC(4),DPC(2)/DPC(4),
     &DPC(3)/DPC(4))
      DO 560 J=1,4
        V(N+1,J)=V(IC1,J)
        V(N+2,J)=V(IC1,J)
        V(N+3,J)=V(IC2,J)
  560 CONTINUE
      N=N+3
      GOTO 800
 
C...Else form one particle, if possible.
  570 NBODY=1
      K(N+1,5)=N+2
      DO 580 J=1,4
        V(N+1,J)=V(IC1,J)
        V(N+2,J)=V(IC1,J)
  580 CONTINUE
 
C...Select hadron flavour from available quark flavours.
  590 IF(NQ.EQ.2.AND.IABS(KFQ(1)).GT.100.AND.IABS(KFQ(2)).GT.100) THEN
        GOTO 820
      ELSEIF(NQ.EQ.2) THEN
        CALL PYKFDI(KFQ(1),KFQ(2),KFLDMP,K(N+2,2))
      ELSE
        KFLN=1+INT((2D0+PARJ(2))*PYR(0))
        CALL PYKFDI(KFLN,-KFLN,KFLDMP,K(N+2,2))
      ENDIF
      IF(K(N+2,2).EQ.0) GOTO 590
      P(N+2,5)=PYMASS(K(N+2,2))
 
C...Use old algorithm for E/p conservation? (EN)
      IF (MSTJ(16).LE.0) GOTO 760
 
C...Find the string piece closest to the cluster by a loop
C...over the undecayed partons not in present cluster. (EN)
      DGLOMI=1D30
      IBEG=0
      I0=0
      NJUNC=0
      DO 620 I1=MAX(1,IP),N-1
        IF(K(I,1).EQ.1) NJUNC=0
        IF(K(I,1).EQ.41) NJUNC=NJUNC+1
        IF(I1.GE.IC1-1.AND.I1.LE.IC2) THEN
          I0=0
        ELSEIF(K(I1,1).EQ.2) THEN
          IF(I0.EQ.0) I0=I1
          I2=I1
  600     I2=I2+1
          IF(K(I2,1).EQ.41) GOTO 620
          IF(K(I2,1).GT.10) GOTO 600
          IF(KCHG(PYCOMP(K(I2,2)),2).EQ.0) GOTO 600
          IF(K(I1,2).EQ.21.AND.K(I2,2).NE.21.AND.K(I2,1).NE.1.AND.
     &    NJUNC.EQ.0) GOTO 620
          IF(K(I1,2).NE.21.AND.K(I2,2).EQ.21.AND.NJUNC.NE.0) GOTO 620
 
C...Define velocity vectors e1, e2, ecl and differences e3, e4.
          DO 610 J=1,3
            E1(J)=P(I1,J)/P(I1,4)
            E2(J)=P(I2,J)/P(I2,4)
            ECL(J)=P(N+1,J)/P(N+1,4)
            E3(J)=E2(J)-E1(J)
            E4(J)=ECL(J)-E1(J)
  610     CONTINUE
 
C...Calculate minimal D=(e4-alpha*e3)**2 for 0<alpha<1.
          E3S=E3(1)**2+E3(2)**2+E3(3)**2
          E4S=E4(1)**2+E4(2)**2+E4(3)**2
          E34=E3(1)*E4(1)+E3(2)*E4(2)+E3(3)*E4(3)
          IF(E34.LE.0D0) THEN
            DDMIN=E4S
          ELSEIF(E34.LT.E3S) THEN
            DDMIN=E4S-E34**2/E3S
          ELSE
            DDMIN=E4S-2D0*E34+E3S
          ENDIF
 
C...Is this the smallest so far?
          IF(DDMIN.LT.DGLOMI) THEN
            DGLOMI=DDMIN
            IBEG=I0
            IPCS=I1
          ENDIF
        ELSEIF(K(I1,1).EQ.1.AND.KCHG(PYCOMP(K(I1,2)),2).NE.0) THEN
          I0=0
        ENDIF
  620 CONTINUE
 
C... Check if there are any strings to connect to the new gluon. (EN)
      IF (IBEG.EQ.0) GOTO 760
 
C...Delta_m = m_clus - m_had > 0: emit a 'gluon' (EN)
      IF (P(N+1,5).GE.P(N+2,5)) THEN
 
C...Construct 'gluon' that is needed to put hadron on the mass shell.
        FRAC=P(N+2,5)/P(N+1,5)
        DO 630 J=1,5
          P(N+2,J)=FRAC*P(N+1,J)
          PG(J)=(1D0-FRAC)*P(N+1,J)
  630   CONTINUE
 
C... Copy string with new gluon put in.
        N=N+2
        I=IBEG-1
  640   I=I+1
        IF(K(I,1).NE.1.AND.K(I,1).NE.2.AND.K(I,1).NE.41) GOTO 640
        IF(KCHG(PYCOMP(K(I,2)),2).EQ.0.AND.K(I,1).NE.41) GOTO 640
        N=N+1
        DO 650 J=1,5
          K(N,J)=K(I,J)
          P(N,J)=P(I,J)
          V(N,J)=V(I,J)
  650   CONTINUE
        K(I,1)=K(I,1)+10
        K(I,4)=N
        K(I,5)=N
        K(N,3)=I
        IF(I.EQ.IPCS) THEN
          N=N+1
          DO 660 J=1,5
            K(N,J)=K(N-1,J)
            P(N,J)=PG(J)
            V(N,J)=V(N-1,J)
  660     CONTINUE
          K(N,2)=21
          K(N,3)=NSAV+1
        ENDIF
        IF(K(I,1).EQ.12.OR.K(I,1).EQ.51) GOTO 640
        GOTO 800
 
C...Delta_m = m_clus - m_had < 0: have to absorb a 'gluon' instead,
C...from string piece endpoints.
      ELSE
 
C...Begin by copying string that should give energy to cluster.
        N=N+2
        I=IBEG-1
  670   I=I+1
        IF(K(I,1).NE.1.AND.K(I,1).NE.2.AND.K(I,1).NE.41) GOTO 670
        IF(KCHG(PYCOMP(K(I,2)),2).EQ.0.AND.K(I,1).NE.41) GOTO 670
        N=N+1
        DO 680 J=1,5
          K(N,J)=K(I,J)
          P(N,J)=P(I,J)
          V(N,J)=V(I,J)
  680   CONTINUE
        K(I,1)=K(I,1)+10
        K(I,4)=N
        K(I,5)=N
        K(N,3)=I
        IF(I.EQ.IPCS) I1=N
        IF(K(I,1).EQ.12.OR.K(I,1).EQ.51) GOTO 670
        I2=I1+1
 
C...Set initial Phad.
        DO 690 J=1,4
          P(NSAV+2,J)=P(NSAV+1,J)
  690   CONTINUE
 
C...Calculate Pg, a part of which will be added to Phad later. (EN)
  700   IF(MSTJ(16).EQ.1) THEN
          ALPHA=1D0
          BETA=1D0
        ELSE
          ALPHA=FOUR(NSAV+1,I2)/FOUR(I1,I2)
          BETA=FOUR(NSAV+1,I1)/FOUR(I1,I2)
        ENDIF
        DO 710 J=1,4
          PG(J)=ALPHA*P(I1,J)+BETA*P(I2,J)
  710   CONTINUE
        PG(5)=SQRT(MAX(1D-20,PG(4)**2-PG(1)**2-PG(2)**2-PG(3)**2))
 
C..Solve 2nd order equation, use the best (smallest) solution. (EN)
        PMSCOL=P(NSAV+2,4)**2-P(NSAV+2,1)**2-P(NSAV+2,2)**2-
     &  P(NSAV+2,3)**2
        PCLPG=(P(NSAV+2,4)*PG(4)-P(NSAV+2,1)*PG(1)-
     &  P(NSAV+2,2)*PG(2)-P(NSAV+2,3)*PG(3))/PG(5)**2
        DELTA=SQRT(PCLPG**2+(P(NSAV+2,5)**2-PMSCOL)/PG(5)**2)-PCLPG
 
C...If all gluon energy eaten, zero it and take a step back.
        ITER=0
        IF(DELTA*ALPHA.GT.1D0.AND.I1.GT.NSAV+3) THEN
          ITER=1
          DO 720 J=1,4
            P(NSAV+2,J)=P(NSAV+2,J)+P(I1,J)
            P(I1,J)=0D0
  720     CONTINUE
          P(I1,5)=0D0
          K(I1,1)=K(I1,1)+10
          I1=I1-1
          IF(K(I1,1).EQ.41) ITER=-1
        ENDIF
        IF(DELTA*BETA.GT.1D0.AND.I2.LT.N) THEN
          ITER=1
          DO 730 J=1,4
            P(NSAV+2,J)=P(NSAV+2,J)+P(I2,J)
            P(I2,J)=0D0
  730     CONTINUE
          P(I2,5)=0D0
          K(I2,1)=K(I2,1)+10
          I2=I2+1
          IF(K(I2,1).EQ.41) ITER=-1
        ENDIF
        IF(ITER.EQ.1) GOTO 700
 
C...If also all endpoint energy eaten, revert to old procedure.
        IF((1D0-DELTA*ALPHA)*P(I1,4).LT.P(I1,5).OR.
     &  (1D0-DELTA*BETA)*P(I2,4).LT.P(I2,5).OR.ITER.EQ.-1) THEN
          DO 740 I=NSAV+3,N
            IM=K(I,3)
            K(IM,1)=K(IM,1)-10
            K(IM,4)=0
            K(IM,5)=0
  740     CONTINUE
          N=NSAV
          GOTO 760
        ENDIF
 
C... Construct the collapsed hadron and modified string partons.
        DO 750 J=1,4
          P(NSAV+2,J)=P(NSAV+2,J)+DELTA*PG(J)
          P(I1,J)=(1D0-DELTA*ALPHA)*P(I1,J)
          P(I2,J)=(1D0-DELTA*BETA)*P(I2,J)
  750   CONTINUE
          P(I1,5)=(1D0-DELTA*ALPHA)*P(I1,5)
          P(I2,5)=(1D0-DELTA*BETA)*P(I2,5)
 
C...Finished with string collapse in new scheme.
        GOTO 800
      ENDIF
 
C... Use old algorithm; by choice or when in trouble.
  760 CONTINUE
C...Find parton/particle which combines to largest extra mass.
      IR=0
      HA=0D0
      HSM=0D0
      DO 780 MCOMB=1,3
        IF(IR.NE.0) GOTO 780
        DO 770 I=MAX(1,IP),N
          IF(K(I,1).LE.0.OR.K(I,1).GT.10.OR.(I.GE.IC1.AND.I.LE.IC2
     &    .AND.K(I,1).GE.1.AND.K(I,1).LE.2)) GOTO 770
          IF(MCOMB.EQ.1) KCI=PYCOMP(K(I,2))
          IF(MCOMB.EQ.1.AND.KCI.EQ.0) GOTO 770
          IF(MCOMB.EQ.1.AND.KCHG(KCI,2).EQ.0.AND.I.LE.NS) GOTO 770
          IF(MCOMB.EQ.2.AND.IABS(K(I,2)).GT.10.AND.IABS(K(I,2)).LE.100)
     &    GOTO 770
          HCR=DPC(4)*P(I,4)-DPC(1)*P(I,1)-DPC(2)*P(I,2)-DPC(3)*P(I,3)
          HSR=2D0*HCR+PECM**2-P(N+2,5)**2-2D0*P(N+2,5)*P(I,5)
          IF(HSR.GT.HSM) THEN
            IR=I
            HA=HCR
            HSM=HSR
          ENDIF
  770   CONTINUE
  780 CONTINUE
 
C...Shuffle energy and momentum to put new particle on mass shell.
      IF(IR.NE.0) THEN
        HB=PECM**2+HA
        HC=P(N+2,5)**2+HA
        HD=P(IR,5)**2+HA
        HK2=0.5D0*(HB*SQRT(MAX(0D0,((HB+HC)**2-4D0*(HB+HD)*P(N+2,5)**2)/
     &  (HA**2-(PECM*P(IR,5))**2)))-(HB+HC))/(HB+HD)
        HK1=(0.5D0*(P(N+2,5)**2-PECM**2)+HD*HK2)/HB
        DO 790 J=1,4
          P(N+2,J)=(1D0+HK1)*DPC(J)-HK2*P(IR,J)
          P(IR,J)=(1D0+HK2)*P(IR,J)-HK1*DPC(J)
  790   CONTINUE
        N=N+2
      ELSE
        CALL PYERRM(3,'(PYPREP:) no match for collapsing cluster')
        RETURN
      ENDIF
 
C...Mark collapsed system and store daughter pointers. Iterate.
  800 DO 810 I=IC1,IC2
        IF((K(I,1).EQ.1.OR.K(I,1).EQ.2).AND.
     &  KCHG(PYCOMP(K(I,2)),2).NE.0) THEN
          K(I,1)=K(I,1)+10
          IF(MSTU(16).NE.2) THEN
            K(I,4)=NSAV+1
            K(I,5)=NSAV+1
          ELSE
            K(I,4)=NSAV+2
            K(I,5)=NSAV+1+NBODY
          ENDIF
        ENDIF
        IF(K(I,1).EQ.41) K(I,1)=K(I,1)+10
  810 CONTINUE
      IF(N.LT.MSTU(4)-MSTU(32)-5) GOTO 390
 
C...Check flavours and invariant masses in parton systems.
  820 NP=0
      KFN=0
      KQS=0
      NJU=0
      DO 830 J=1,5
        DPS(J)=0D0
  830 CONTINUE
      DO 860 I=MAX(1,IP),N
        IF(K(I,1).EQ.41) NJU=NJU+1
        IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 860
        KC=PYCOMP(K(I,2))
        IF(KC.EQ.0) GOTO 860
        KQ=KCHG(KC,2)*ISIGN(1,K(I,2))
        IF(KQ.EQ.0) GOTO 860
        NP=NP+1
        IF(KQ.NE.2) THEN
          KFN=KFN+1
          KQS=KQS+KQ
          MSTJ(93)=1
          DPS(5)=DPS(5)+PYMASS(K(I,2))
        ENDIF
        DO 840 J=1,4
          DPS(J)=DPS(J)+P(I,J)
  840   CONTINUE
        IF(K(I,1).EQ.1) THEN
          NFERR=0
          IF(NJU.EQ.0.AND.NP.NE.1) THEN
            IF(KFN.EQ.1.OR.KFN.GE.3.OR.KQS.NE.0) NFERR=1
          ELSEIF(NJU.EQ.1) THEN
            IF(KFN.NE.3.OR.IABS(KQS).NE.3) NFERR=1
          ELSEIF(NJU.EQ.2) THEN
            IF(KFN.NE.4.OR.KQS.NE.0) NFERR=1
          ELSEIF(NJU.GE.3) THEN
            NFERR=1
          ENDIF
          IF(NFERR.EQ.1) CALL
     &    PYERRM(2,'(PYPREP:) unphysical flavour combination')
          IF(NP.NE.1.AND.DPS(4)**2-DPS(1)**2-DPS(2)**2-DPS(3)**2.LT.
     &    (0.9D0*PARJ(32)+DPS(5))**2) CALL PYERRM(3,
     &    '(PYPREP:) too small mass in jet system')
          NP=0
          KFN=0
          KQS=0
          NJU=0
          DO 850 J=1,5
            DPS(J)=0D0
  850     CONTINUE
        ENDIF
  860 CONTINUE
 
      RETURN
      END
