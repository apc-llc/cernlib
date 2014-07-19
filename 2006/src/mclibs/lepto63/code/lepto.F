
C **********************************************************************

      SUBROUTINE LEPTO

C...Administer the generation of an event.
C...Note: if error flag LST(21) is non-zero, no proper event generated.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      DOUBLE PRECISION DTHETA,DPHI,DBETA,DETOT,DARI29,DARI30
      DIMENSION SPQ(17)
      DATA NUMMIS,NWARN/0,10/,DARI29,DARI30/2*0.D0/

      L17=0
    1 LST(21)=0
      DO 10 I=1,10
      DO 10 J=1,5
      K(I,J)=0
   10 V(I,J)=0.
      DO 15 I=1,4
      K(I,1)=21
   15 K(I,2)=KSAVE(I)
      K(4,1)=1
      N=2
      IF(LST(17).NE.0.AND.LST(2).GT.0) THEN
C...Lepton and/or nucleon energy may vary from event to event,
        IF(L17.EQ.0) THEN
C...Momentum vectors from P(i,j) i=1,2 j=1,2,3 on entry in LEPTO
          DO 20 I=1,2
          P(I,5)=ULMASS(K(I,2))
          P(I,4)=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2+P(I,5)**2)
          DO 20 J=1,5
   20     PSAVE(3,I,J)=P(I,J)
        ELSE
C...Momentum vectors from PSAVE if new try, i.e. jump back to 1
          DO 25 I=1,2
          DO 25 J=1,5
   25     P(I,J)=PSAVE(3,I,J)
        ENDIF
        L17=1
C...Transform to cms of incoming particles, lepton along +z axis.
        DO 30 J=1,3
   30   DBETA(1,J)=(DBLE(P(1,J))+DBLE(P(2,J)))/
     &             (DBLE(P(1,4))+DBLE(P(2,4)))
        CALL LUDBRB(0,0,0.,0.,-DBETA(1,1),-DBETA(1,2),-DBETA(1,3))
        SPHI(1)=ULANGL(P(1,1),P(1,2))
        CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
        STHETA(1)=ULANGL(P(1,3),P(1,1))
        CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        LST(28)=2
        PARL(21)=2.*(P(1,4)*P(2,4)-P(1,3)*P(2,3))
      ELSE
C...Initial state momenta fixed from LINIT call.
        DO 42 I=1,2
        DO 40 J=1,5
   40   P(I,J)=PSAVE(3,I,J)
   42   IF(PSAVE(3,1,3).LT.0.) P(I,3)=-PSAVE(3,I,3)
        LST(28)=3
      ENDIF

      CALL LEPTOX
C...Return if error or if no event to be generated.
      IF(LST(21).NE.0.OR.LST(2).LE.0.OR.LST(7).EQ.-1) RETURN

      IF(PARI(29).LT.0.5) THEN
C...For first call, reset double precision counters.
        DARI29=0.D0
        DARI30=0.D0
      ENDIF
      DARI29=DARI29+1.D0
      PARI(29)=DARI29

C     CALL GULIST(-3,2)
C...Scattered lepton and exchanged boson added to event record in LKINEM
C...Transform to lepton-nucleon cms if not made earlier
      IF(LST(17).EQ.0) THEN
        DO 46 I=3,4
        DO 45 J=1,5
   45   PSAVE(3,I,J)=P(I,J)
   46   IF(PSAVE(3,1,3).LT.0.) PSAVE(3,I,3)=-P(I,3)
        CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        LST(28)=2
      ENDIF
      DO 50 I=1,4
      DO 50 J=1,5
   50 PSAVE(2,I,J)=P(I,J)
C     CALL GULIST(-2,2)

C...Prepare for parton cascade.
      IF(LST(8).GE.2.AND.MOD(LST(8),10).NE.9) CALL LSHOWR(0)

C...Transform to hadronic cms, boost parameters in double precision.
      DETOT=DBLE(P(1,4))-DBLE(P(4,4))+DBLE(P(2,4))
      DBETA(2,1)=-DBLE(P(4,1))/DETOT
      DBETA(2,2)=-DBLE(P(4,2))/DETOT
      DBETA(2,3)=(DBLE(P(1,3))-DBLE(P(4,3))+DBLE(P(2,3)))/DETOT
      CALL LUDBRB(0,0,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      SPHI(2)=0.
      STHETA(2)=ULANGL(P(3,3),P(3,1))
      CALL LUDBRB(0,0,-STHETA(2),0.,0.D0,0.D0,0.D0)
      LST(28)=1
      DO 60 I=1,4
      DO 60 J=1,5
   60 PSAVE(1,I,J)=P(I,J)
C...Save momentum of exchanged boson (used in subroutine LFRAME).
      DO 70 J=1,5
   70 PB(J)=P(3,J)
C     CALL GULIST(-1,2)

   90 N=4
      MSTU(1)=N+1
      LST(26)=N+1
      LST(27)=0
      PARL(25)=ULALPS(Q2)
      IF(LST(8).EQ.1.OR.LST(8)/10.EQ.1.OR.MOD(LST(8),10).EQ.9) THEN
C...Probabilities for hard, first order QCD events.
CAE...Corrected what to do when LQGEV or LQQBEV fail. Now make LQEV.
        CALL LQCDPR(QG,QQB)
        
CAE        WRITE(6,*) 'Lepto:',X,Q2,PARL(27)
CAE        WRITE(6,*) 'weights:',1-QG-QQB,QG,QQB
        
        DO 100 I=1,17
  100   SPQ(I)=PQ(I)
  200   SRLU=RLU(0)
        IF(SRLU.GT.QQB+QG) THEN
          CALL LQEV
        ELSEIF(SRLU.GT.QQB) THEN
          IF(LST(8).EQ.9) THEN
            DO 211 I=1,17
  211         PQ(I)=SPQ(I)
            CALL LQEV
          ELSE
            CALL LQGEV
            DO 212 I=1,17
              PQ(I)=SPQ(I)
  212       CONTINUE
          ENDIF
        ELSE
          CALL LQQBEV
          DO 213 I=1,17
            PQ(I)=SPQ(I)
  213     CONTINUE
          IF(LST(8).EQ.9.AND.LST(21).EQ.0) THEN
            IF(PLU(5,11).LT.Q2*PARA(20)) THEN
              DO 220 I=1,17
  220         PQ(I)=SPQ(I)
              CALL LQEVAR(K(5,2),K(7,2))
            ENDIF
          ENDIF
        ENDIF
        IF(LST(21).NE.0) THEN
CAE          WRITE(6,*) 'error:',LST(21),LST(24)
  230     CALL LQEV
          IF(LST(21).NE.0) GOTO 230
        ENDIF   
      ELSE
C...QPM model without QCD corrections (cascade applied later).
  300   CALL LQEV
        IF(LST(21).NE.0) GOTO 300
      ENDIF

      NS=MSTU(1)
      MSTU(1)=0
C     CALL GULIST(-3,2)
C     WRITE(6,*) ' LST(24)=',LST(24)
CJR--       no preclustering of small systems
          MSTJ(14)=-1
CJR--            
      IF(LST(8).LE.1.OR.MOD(LST(8),10).EQ.9) THEN
C...No parton cascade, introduce primordial kt.
        IF(PARL(3).GT.1.E-03) THEN
          CALL LPRIKT(PARL(3),PT,PHI)
          CALL LUDBRB(NS,N,0.,-PHI,0.D0,0.D0,0.D0)
          CALL LUDBRB(NS,N,ATAN(2.*PT/SQRT(W2)),PHI,0.D0,0.D0,0.D0)
        ENDIF
        IF(MOD(LST(8),10).NE.9) THEN
C...Check system against fragmentation cuts.
          MSTU(24)=0
          CALL LUPREP(0)
          IF(MSTU(24).NE.0) THEN
            IF(LST(3).GE.1) WRITE(6,*)'LUPREP error MSTU(24)=',MSTU(24),
     &                                ', New event generated'
            LST(21)=1
            GOTO 1
          ENDIF
        ENDIF
      ELSEIF(LST(24).EQ.1) THEN
C...Include parton cascades (+ remnant & kt) on q-event
        CALL LSHOWR(1)
      ELSE
C...Include parton cascades (+ remnant & kt) on qg- or qqbar-event
        CALL LMEPS
      ENDIF
      IF(LST(21).NE.0) THEN
C        IF(LST(3).GE.1)
C     &     WRITE(6,*)'Cascade error LST(21)= ',LST(21),
C     &               ', New event generated'
        GOTO 1
      ENDIF

CJR--       Soft colour interactions
      IF(LST(34).EQ.1 .OR. LST(34).EQ.2) CALL LSCI(PARL(7))
      IF(LST(21).NE.0) GOTO 1
CJR--       take care of small systems
      CALL LSMALL
      IF(LST(21).NE.0) THEN
        IF(LST(3).GE.1) WRITE(6,*)' LSMALL error LST(21)= ',LST(21),
     &                            ', New event generated'
        GOTO 1
      ENDIF
      MSTJ(14)=1
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
         IF(LST(3).GE.1) WRITE(6,*)' LUPREP error MSTU(24)= ',MSTU(24),
     &                             ', New event generated'
         LST(21)=1
      ENDIF
CJR--            
      IF(LST(21).NE.0) GOTO 1

      DO 400 I=1,N
C...Correct energy-momentum-mass mismatch for real particle
      IF(P(I,5).LT.0.) GOTO 400
      ENERGY=SQRT(DBLE(P(I,5))**2+DBLE(P(I,1))**2+DBLE(P(I,2))**2+
     &DBLE(P(I,3))**2)
      P2=DBLE(P(I,4))**2-DBLE(P(I,1))**2-DBLE(P(I,2))**2-DBLE(P(I,3))**2
      IF(ABS(ENERGY-P(I,4))/(PSAVE(3,1,4)+PSAVE(3,2,4)).GT.PARU(11))THEN
        NUMMIS=NUMMIS+1
C...For testing purposes
C       IF(LST(3).GE.1.AND.NUMMIS.LE.NWARN) THEN
C         WRITE(6,1000) I,(K(I,J),J=1,2),(P(I,J),J=1,5),
C    &    SIGN(SQRT(ABS(P2)),P2),ENERGY,INT(DARI29),NWARN
C         IF(ABS(P2-P(I,5)**2).GT.400.) CALL LULIST(2)
C       ENDIF
CAE        WRITE(6,*) 'Energy mismatch',LST(24),PARL(28),PARL(29),NUMMIS
        GOTO 90
      ENDIF
      P(I,4)=ENERGY
  400 CONTINUE

      DARI30=DARI30+1.D0
      PARI(30)=DARI30
Ctest IF(LST(23).EQ.2) PARL(24)=PARL(24)*DARI30/DARI29

      DO 500 I=1,N
      DO 500 J=1,5
  500 V(I,J)=0.
      IF(LST(7).EQ.1) THEN
        CALL LUEXEC
        IF(MSTU(24).NE.0) THEN
          WRITE(6,*) ' Error from JETSET, new event made'
          GOTO 90
        ENDIF
      ENDIF

C     CALL GULIST(-1,2)
C...Transform to desired frame
C     LST(28)=1
      LST(29)=0
      PHIR=6.2832*RLU(0)
      IF(LST(17).EQ.0) THEN
        IF(LST(5).GE.2) CALL LFRAME(LST(5),0)
C...Restore momenta (e,p,boson,L) due to numerical errors from boosts
        DO 600 I=1,4
        DO 600 J=1,5
  600   P(I,J)=PSAVE(LST(28),I,J)
        IF(LST(6).EQ.1.AND.LST(28).GE.2) THEN
C...Random rotation in azimuthal angle
          CALL LUDBRB(0,0,0.,PHIR,0.D0,0.D0,0.D0)
          LST(29)=1
        ENDIF
      ELSE
        IF(LST(5).GE.2) CALL LFRAME(LST(5),LST(6))
      ENDIF
C...Deactivate scattered lepton
      IF(MOD(LST(4),10).EQ.0) K(4,1)=21
C     CALL GULIST(0,2)

      RETURN
 1000 FORMAT(' Warning: too large numerical mismatch in ',
     &'particle energy-momentum-mass',
     &/,3X,'I K(I,1) ..2)  P(I,1)  P(I,2)  P(I,3)',
     &'  P(I,4)  P(I,5)    mass  energy',/,I4,2I6,7F8.3,/,
     &' Event no.',I8,' regenerated. Only first',I5,' warnings printed')
      END
