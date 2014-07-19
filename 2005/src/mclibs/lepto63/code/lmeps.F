
C **********************************************************************

      SUBROUTINE LMEPS

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON /LYPROC/ ISUB,KFL(3,2),XPY(2),SH,TH,UH,Q2PY,XSEC(0:40)
      COMMON /LYINT1/ XQPY(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      DOUBLE PRECISION DTHETA,DPHI,DBETA
      DOUBLE PRECISION DPQ2,DPB(3),DPA(3),DCTHET,DROBO(5)
      DOUBLE PRECISION DELTAP(4),DPLONG,DBTOT,DGAMMA,DROOT
      DIMENSION KS(9,5),PS(9,5),ROBO(5),XPQ(-6:6)
      SAVE KS,PS

C     CALL GULIST(100,2)
C...Save event record in hadronic cms
      DO 10 I=1,7
      DO 10 J=1,5
      KS(I,J)=K(I,J)
   10 PS(I,J)=P(I,J)
C...Rearrange event record to PYSSPA standard
      IP2=6
      IF(LST(24).EQ.3) IP2=7
      DO 20 J=1,5
      K(3,J)=0.
      P(3,J)=0.
      K(4,J)=0
      P(4,J)=0.
      K(5,J)=KS(3,J)
      P(5,J)=PS(3,J)
      K(7,J)=KS(4,J)
      P(7,J)=PS(4,J)
      K(8,J)=KS(5,J)
      P(8,J)=PS(5,J)
      K(9,J)=KS(4,J)
      P(9,J)=PS(4,J)
      K(10,J)=KS(IP2,J)
   20 P(10,J)=PS(IP2,J)
      K(5,3)=3
      K(6,3)=4
      K(7,3)=5
      K(8,3)=6
      K(9,3)=5
      K(10,3)=6
      DO 30 I=5,10
   30 K(I,1)=21
      K(9,1)=0
C...Incoming parton = outgoing 2 parton - boson fourvectors
      DO 40 J=1,4
   40 P(6,J)=P(8,J)+P(10,J)-P(5,J)
      P(6,5)=0.
      K(6,2)=LST(25)
      IF(LST(24).EQ.3) K(6,2)=21
      N=10
C     CALL GULIST(101,2)

      XR=X
      DPQ2=DBLE(Q2)
C...Partons with colour information in hadronic cms frame.
      DO 120 I=11,27
      DO 120 J=1,5
      K(I,J)=0
      P(I,J)=0.
  120 V(I,J)=0.
      NS=20
      DO 130 J=1,5
      K(NS+1,J)=K(5,J)
      P(NS+1,J)=P(5,J)
      K(NS+3,J)=K(6,J)
      P(NS+3,J)=P(6,J)
      K(NS+5,J)=K(8,J)
      P(NS+5,J)=P(8,J)
      K(NS+6,J)=K(10,J)
  130 P(NS+6,J)=P(10,J)
C...Old standard continuation lines
      K(NS+2,1)=-1
      K(NS+2,3)=NS+1
      K(NS+4,1)=-1
      K(NS+4,3)=NS+3
      P(NS+4,3)=27
      P(NS+4,4)=27
C...Origin and colour info for incoming parton
      K(NS+3,1)=13
      K(NS+3,3)=2
      K(NS+3,4)=27
      K(NS+3,5)=27
C...Colour info for two outgoing partons
      K(NS+5,1)=3
      K(NS+6,1)=3
      IF(K(NS+6,2).EQ.21) THEN
C...qg-event
        IF(K(NS+5,2).GT.0) THEN
          K(NS+5,4)=(NS+6)*MSTU(5)
          K(NS+5,5)=(NS+7)*MSTU(5)
          K(NS+6,4)=(NS+7)*MSTU(5)
          K(NS+6,5)=(NS+5)*MSTU(5)
        ELSE
          K(NS+5,4)=(NS+7)*MSTU(5)
          K(NS+5,5)=(NS+6)*MSTU(5)
          K(NS+6,4)=(NS+5)*MSTU(5)
          K(NS+6,5)=(NS+7)*MSTU(5)
        ENDIF
      ELSE
C...qqbar-event
        K(NS+5,4)=(NS+7)*MSTU(5)
        K(NS+5,5)=(NS+7)*MSTU(5)
        K(NS+6,4)=(NS+7)*MSTU(5)
        K(NS+6,5)=(NS+7)*MSTU(5)
      ENDIF
C...Effective outgoing parton = sum of both outgoing partons
      K(NS+7,1)=14
      K(NS+7,3)=3
      IF(LST(24).EQ.2) THEN
        K(NS+7,2)=K(NS+5,2)
        IF(K(NS+7,2).EQ.21) WRITE(6,*) ' Warning: K(NS+7,2)=',K(NS+7,2)
        IF(K(NS+7,2).GT.0) THEN
          K(NS+7,4)=(NS+3)*MSTU(5)+26
          K(NS+7,5)=(NS+3)*MSTU(5)+25
        ELSE
          K(NS+7,4)=(NS+3)*MSTU(5)+25
          K(NS+7,5)=(NS+3)*MSTU(5)+26
        ENDIF
      ELSE
        K(NS+7,2)=21
        IF(K(NS+5,2).GT.0) THEN
          K(NS+7,4)=(NS+3)*MSTU(5)+25
          K(NS+7,5)=(NS+3)*MSTU(5)+26
        ELSE
          K(NS+7,4)=(NS+3)*MSTU(5)+26
          K(NS+7,5)=(NS+3)*MSTU(5)+25
        ENDIF
      ENDIF
      DO 140 J=1,4
  140 P(NS+7,J)=P(8,J)+P(10,J)

      IT=NS+7
      IF(ABS(P(IT,1)).GT.0.1.OR.ABS(P(IT,2)).GT.0.1) THEN
C       WRITE(6,*) 'Warning: non-zero pt on final shower initiator'
C       WRITE(6,*) '1:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
C       WRITE(6,*) '1:',8 ,K( 8,2),P( 8,1),P( 8,2),P( 8,3),P( 8,4),P( 8,5)
C       WRITE(6,*) '1:',10,K(10,2),P(10,1),P(10,2),P(10,3),P(10,4),P(10,5)
        LST(21)=12
        RETURN
      ENDIF
      P(IT,1)=0.
      P(IT,2)=0.

      P(NS+7,5)=SQRT(MAX(0.,P(NS+7,4)**2-P(NS+7,1)**2-P(NS+7,2)**2-
     &P(NS+7,3)**2))
      N=NS+7
C     CALL GULIST(103,2)

C...Scale for bremsstrahlung etc.
      Q2PY=Q2
      IPY(40)=10
      IPY(47)=N
C...Save quantities for later use.
      XPY(1)=1.
      XPY(2)=XR
      CALL LYSTFU(K(2,2),XR,Q2,XPQ)
      DO 160 IFL=-6,6
  160 XQPY(2,IFL)=XPQ(IFL)
      IF(LST(23).EQ.1) THEN
        ISUB=39
        IPY(11)=1
      ELSEIF(LST(23).EQ.3) THEN
        ISUB=39
        IPY(11)=2
      ELSEIF(LST(23).EQ.4) THEN
        ISUB=39
        IPY(11)=3
      ELSEIF(LST(23).EQ.2) THEN
        ISUB=40
      ENDIF
      KFL(2,1)=K(5,2)
      KFL(2,2)=K(6,2)
      KFL(1,1)=KFL(2,1)
      KFL(1,2)=KFL(2,2)
      IF(ISUB.EQ.39) KFL(3,1)=K(1,2)
      IF(ISUB.EQ.40) KFL(3,1)=K(1,2)+ISIGN(1,K(1,2))
      KFL(3,2)=K(27,2)
      PYVAR(2)=PARL(21)
      PYVAR(1)=SQRT(PYVAR(2))
      PYVAR(3)=P(1,5)
      PYVAR(4)=P(2,5)
      PYVAR(5)=PYVAR(1)/2.
      IPY(41)=K(1,2)
      IPY(42)=K(2,2)
      IPY(48)=0

C...Generate timelike parton shower (if required)
      IF(IPY(13).EQ.1) THEN
        CALL LSCALE(1,QMAX)
        CALL LUSHOW(25,26,QMAX)
      ENDIF
      IT=25
      IF(N.GE.27) IT=27
      NS=N
C     CALL GULIST(104,2)

C...Generate spacelike parton shower (if required)
      IPU1=0
      IPU2=23
      IF(XPY(2)*(1.+(P(IT,5)**2+PYPAR(22))/P(21,5)**2).GT.0.999) THEN
        LST(21)=13
        RETURN
      ENDIF
      IF(IPY(14).GE.1) THEN
        CALL LYSSPA(IPU1,IPU2)
        IF(LST(21).NE.0) RETURN
      ENDIF
      IF(N.EQ.NS) THEN 
        DO 220 I=NS+1,NS+4
        DO 220 J=1,5
        K(I,J)=0
        P(I,J)=0.
  220   V(I,J)=0.
        K(NS+1,1)=11
        K(NS+1,2)=KFL(2,1)
        K(NS+1,3)=21
        DO 230 J=1,5
  230   P(NS+1,J)=P(21,J)
        K(NS+2,1)=-1
        K(NS+2,3)=NS+1
        K(NS+3,1)=13
        K(NS+3,2)=KFL(2,2)
        K(NS+3,3)=23
        K(NS+3,4)=23
        K(NS+3,5)=23
        P(NS+3,3)=(P(IT,5)**2+Q2)*(P(21,4)-P(21,3))/(2.*Q2)
        P(NS+3,4)=-P(NS+3,3)
        K(NS+4,1)=-1
        K(NS+4,3)=NS+3
        P(NS+4,3)=23
        P(NS+4,4)=23
        P(24,1)=NS+3
        P(24,2)=NS+3
        K(23,4)=K(23,4)+(NS+3)*MSTU(5)
        K(23,5)=K(23,5)+(NS+3)*MSTU(5)
        IPU1=0
        IPU2=NS+3
        N=N+4
      ENDIF
C     CALL GULIST(105,2)

C...Rotate and boost outgoing parton shower
      IF(N.GT.31) THEN
        K(N+1,1)=0
        DO 210 J=1,4
  210   P(N+1,J)=P(NS+1,J)+P(NS+3,J)
        IF(P(N+1,4).LE.1.01*P(IT,5)) THEN
          LST(21)=14
          RETURN
        ENDIF
        ROBO(1)=ULANGL(P(IT,3),SQRT(P(IT,1)**2+P(IT,2)**2))
        ROBO(2)=ULANGL(P(IT,1),P(IT,2))
      IF(ABS(ROBO(1)).GT.0.001.OR.ABS(ROBO(2)).GT.0.001) THEN
      WRITE(6,*) '1:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
      WRITE(6,*) '   ROBO(1-2)=',ROBO(1),ROBO(2)
      ENDIF
        CALL LUDBRB(25,NS,0.,-ROBO(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(25,NS,-ROBO(1),0.,0.D0,0.D0,0.D0)
C...Replace old rotation method with x,y,z-boost to preserve QCD phi-dep
        DELTAP(1)=DBLE(P(N+1,1))
        DELTAP(2)=DBLE(P(N+1,2))
        DELTAP(3)=DBLE(P(N+1,3)) - DBLE(P(IT,3))
        DELTAP(4)=SQRT(DELTAP(1)**2+DELTAP(2)**2+DELTAP(3)**2)
        IF(DELTAP(4).LT.1.D-11) GOTO 410
        DPLONG=-(DBLE(P(IT,3))*DELTAP(3))/DELTAP(4)
        DROOT=MAX(0.D0,DBLE(P(N+1,4))**2-DBLE(P(IT,4))**2+DPLONG**2)
        DBTOT=-(DPLONG*DBLE(P(IT,4))-DBLE(P(N+1,4))*SQRT(DROOT))/
C    &  SQRT(DBLE(P(N+1,4))**2-DBLE(P(IT,4))**2+DPLONG**2))/
     &  (DPLONG**2+DBLE(P(N+1,4))**2)
        DGAMMA=1.D0/SQRT(1.D0-DBTOT**2)
        DO 400 I = 1,3
  400   DROBO(I+2)=DELTAP(I)/(DGAMMA/(DGAMMA+1.D0)*
     &  (DBLE(P(N+1,4))-DGAMMA*DBLE(P(IT,4)))+DGAMMA*DBLE(P(IT,4)))
        CALL LUDBRB(25,NS,0.,0.,DROBO(3),DROBO(4),DROBO(5))
  410   CONTINUE
C...End phi-correction
      ENDIF
C     CALL GULIST(106,2)

      Q2PY=Q2
C...Hadron remnant and primordial kt
      IPY(47)=N
      CALL LYREMN(IPU1,IPU2)
      IF(IPY(48).EQ.1) THEN
        LST(21)=15
        RETURN
      ENDIF
C     CALL GULIST(107,2)

C...Rearrange partons along strings
      MSTU(24)=0
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
C       CALL GULIST(188,2)
        IF(LST(3).GE.1) WRITE(6,*) ' LUPREP error MSTU(24)= ',MSTU(24)
        LST(21)=16
        RETURN
      ENDIF
C     CALL GULIST(109,2)

C...Clean up event record -> order:
C...1=inc. lepton; 2=inc. nucleon; 3=exch boson; 4=scat. lepton;
C...5=inc. parton before initial shower; 6=inc. parton at hard scattering
C...after shower; 7,8=first,second parton from hard scattering
C...before final shower
      LST(26)=7
      DO 510 J=1,5
      K(N+1,J)=K(4,J)
  510 P(N+1,J)=P(4,J)
      DO 520 J=1,5
      K(3,J)=K(5,J)
      P(3,J)=P(5,J)
      K(4,J)=K(9,J)
      P(4,J)=P(9,J)
      K(5,J)=K(N+1,J)
      P(5,J)=P(N+1,J)
      K(6,J)=K(NS+3,J)
      P(6,J)=P(NS+3,J)
C     K(7,J)=K(IT,J)
C     P(7,J)=P(IT,J)
      K(7,J)=K(25,J)
      P(7,J)=P(25,J)
      K(8,J)=K(26,J)
      P(8,J)=P(26,J)
  520 CONTINUE
      K(3,3)=1
      K(4,3)=1
      K(6,1)=21
      K(6,3)=5
      K(6,4)=0
      K(6,5)=0
      K(7,1)=21
      K(7,3)=6
      K(7,4)=0
      K(7,5)=0
      K(8,1)=21
      K(8,3)=6
      K(8,4)=0
      K(8,5)=0
C...Activate line with scattered lepton.
      K(4,1)=1
C...Deactivate obsolete lines 9, 10, 21, NS+1 (extra lines with boson)
      K(9,1)=0
      K(10,1)=0
      K(21,1)=0
      IF(K(NS+1,2).EQ.K(3,2)) K(NS+1,1)=0
C...Zero irrelevant lines with K(I,1)<0
      DO 540 I=1,N
      IF(K(I,1).LT.0) THEN
        DO 530 J=1,5
        K(I,J)=0
  530   P(I,J)=0.
      ENDIF
  540 CONTINUE
C     CALL GULIST(110,2)
C...Delete internal parton lines, i.e. with K(I,1)=13,14
      IF(MOD(LST(4)/10,10).EQ.0) THEN
        CALL LTIMEX(T1)
        CALL LUEDIT(14)
        CALL LTIMEX(T2)
C       CALL GULIST(111,2)
      ENDIF
C...Delete empty lines
      CALL LTIMEX(T1)
      CALL LUEDIT(12)
      CALL LTIMEX(T2)
C     CALL GULIST(112,2)

      RETURN
      END
