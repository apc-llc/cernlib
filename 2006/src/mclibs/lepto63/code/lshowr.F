
C#######################################################################
C
C  The following routines for parton cascades were made together
C  with M. Bengtsson and T. Sjostrand (Z. Phys. C37 (1988) 465,
C  Nucl. Phys. B301 (1988) 554). Contain modifications of
C  routines in PYTHIA 4.8 (Sjostrand, Bengtsson, CPC 46 (1987) 43).
C
C **********************************************************************

      SUBROUTINE LSHOWR(ICALL)

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON /LYPROC/ ISUB,KFL(3,2),XPY(2),SH,TH,UH,Q2PY,XSEC(0:40)
      COMMON /LYINT1/ XQPY(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      DOUBLE PRECISION DTHETA,DPHI,DBETA
      DOUBLE PRECISION DPQ2,DPB(3),DPA(3),DCTHET,DROBO(5)
      DIMENSION KS(9,5),PS(9,5),ROBO(5),XPQ(-6:6)
      SAVE KS,PS

      IF(ICALL.EQ.0) THEN
C...Initialize cascade for each event, save event record in overall cms.
        DO 10 I=1,9
        DO 10 J=1,5
        KS(I,J)=0
   10   PS(I,J)=0.
        DO 20 J=1,5
        KS(1,J)=K(1,J)
        PS(1,J)=P(1,J)
        KS(2,J)=K(2,J)
        PS(2,J)=P(2,J)
        KS(5,J)=K(3,J)
        PS(5,J)=P(3,J)
        KS(7,J)=K(4,J)
   20   PS(7,J)=P(4,J)
        KS(5,3)=3
        KS(7,1)=21
        KS(7,3)=5
C       CALL GULIST(0,2)
        RETURN
      ENDIF

C     CALL GULIST(1,2)
C...Apply parton cascade on QPM event.
C...Save incoming and outgoing quark as well as scattered lepton.
      KS(6,1)=21
      KS(6,2)=LST(25)
      KS(6,3)=4
      KS(8,1)=21
      KS(8,2)=K(5,2)
      KS(8,3)=6
      KS(9,1)=0
      KS(9,2)=K(4,2)
      KS(9,3)=5
      DO 110 J=1,5
      PS(6,J)=0.
      PS(8,J)=P(5,J)
  110 PS(9,J)=P(4,J)
      XR=X
      DPQ2=DBLE(Q2)
      PMA1=0.
      PS(6,5)=PMA1
      PMA2=PS(8,5)
      DPB(1)=0.5D0*(DPQ2*(1D0/XR-1D0)+DBLE(PS(1,5))**2-
     &ULMASS(IABS(KS(7,2)))**2)/(PS(1,4)+PS(2,4))
      DPB(2)=DSQRT(DPB(1)**2+DPQ2)
      DCTHET=(DBLE(PS(2,4))*DPB(1)-DPQ2/(2D0*XR))/(DBLE(PS(2,3))*
     &DPB(2))
      DPA(1)=(DPB(2)*DCTHET)**2-DPB(1)**2
      DPA(2)=DPQ2-DBLE(PMA1)**2+DBLE(PMA2)**2
      PS(6,4)=-(DPA(2)*DPB(1)-DPB(2)*DCTHET*DSQRT(DPA(2)**2+4D0*
     &DBLE(PMA1)**2*DPA(1)))/(2D0*DPA(1))
      PS(6,3)=-SQRT((PS(6,4)+PMA1)*(PS(6,4)-PMA1))
C...Partons with colour information in hadronic cms frame.
      DO 120 I=10,26
      DO 120 J=1,5
      K(I,J)=0
      P(I,J)=0.
  120 V(I,J)=0.
      NS=20
      K(NS+1,1)=21
      K(NS+1,2)=K(3,2)
      K(NS+1,3)=3
      K(NS+2,1)=-1
      K(NS+2,3)=NS+1
      K(NS+3,2)=KS(6,2)
      DO 130 J=1,5
  130 P(NS+1,J)=P(3,J)
      K(NS+3,1)=13
      K(NS+3,3)=2
      P(NS+3,5)=0.
      K(NS+4,1)=-1
      K(NS+4,3)=NS+3
      K(NS+3,4)=NS+5
      K(NS+3,5)=NS+5
      P(NS+4,3)=NS+5
      P(NS+4,4)=NS+5
      K(NS+5,1)=3
      K(NS+5,3)=8
      K(NS+5,2)=KS(8,2)
      K(NS+6,1)=-1
      K(NS+6,3)=NS+5
      DO 140 J=1,4
      P(NS+5,J)=P(5,J)
      P(NS+3,J)=P(NS+5,J)-P(NS+1,J)
  140 CONTINUE
      P(NS+5,5)=PMA2
      P(NS+6,1)=NS+3
      P(NS+6,2)=NS+3
      K(NS+5,4)=(NS+3)*MSTU(5)
      K(NS+5,5)=(NS+3)*MSTU(5)
      N=NS+6
C     CALL GULIST(2,2)
C...Copy saved record in overall cms to line 1 through 9.
C...Lines 1,2,5,6,7 in ep cms, 8,9 in hadronic cms
      DO 150 I=1,9
      DO 150 J=1,5
      K(I,J)=KS(I,J)
  150 P(I,J)=PS(I,J)
C     CALL GULIST(3,2)
C...Scale for bremsstrahlung etc.
      Q2PY=Q2
      IPY(40)=8
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
      IFL1=K(6,2)
      IFL2=K(8,2)
      KFL(2,2)=IFL1
      KFL(1,1)=KFL(2,1)
      KFL(1,2)=KFL(2,2)
      IF(ISUB.EQ.39) KFL(3,1)=K(1,2)
      IF(ISUB.EQ.40) KFL(3,1)=K(1,2)+ISIGN(1,K(1,2))
      KFL(3,2)=IFL2
      PYVAR(2)=(P(1,4)+P(2,4))**2
      PYVAR(1)=SQRT(PYVAR(2))
      PYVAR(3)=P(1,5)
      PYVAR(4)=P(2,5)
      PYVAR(5)=P(1,3)
      IPY(41)=K(1,2)
      IPY(42)=K(2,2)
      IPY(48)=0

C...Generate timelike parton shower (if required)
      IF(IPY(13).EQ.1) THEN
        CALL LSCALE(1,QMAX)
        QMAX=MIN(QMAX,P(25,4))
        CALL LUSHOW(25,0,QMAX)
      ENDIF
      IT=25
      IF(N.GE.27) IT=27
      NS=N
C     CALL GULIST(4,2)

C...Generate spacelike parton shower (if required)
      IPU1=0
      IPU2=23
      IF(XPY(2)*(1.+(P(IT,5)**2+PYPAR(22))/P(21,5)**2).GT.0.999) THEN
        LST(21)=7
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
C     CALL GULIST(5,2)

      IF(ABS(P(IT,1)).GT.0.1.OR.ABS(P(IT,2)).GT.0.1) THEN
C       WRITE(6,*) 'Warning: non-zero pt on final shower initiator'
C       WRITE(6,*) '0:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
        LST(21)=8
        RETURN
      ENDIF
      P(IT,1)=0.
      P(IT,2)=0.

C...Rotate and boost outgoing parton shower
      IF(N.GT.30) THEN
        K(N+1,1)=0
        DO 210 J=1,4
  210   P(N+1,J)=P(NS+1,J)+P(NS+3,J)
        IF(P(N+1,4).LE.1.01*P(IT,5)) THEN
          LST(21)=9
          RETURN
        ENDIF
        ROBO(1)=ULANGL(P(IT,3),SQRT(P(IT,1)**2+P(IT,2)**2))
        ROBO(2)=ULANGL(P(IT,1),P(IT,2))
      IF(ABS(ROBO(1)).GT.0.001.OR.ABS(ROBO(2)).GT.0.001) THEN
      WRITE(6,*) '0:',IT,K(IT,2),P(IT,1),P(IT,2),P(IT,3),P(IT,4),P(IT,5)
      WRITE(6,*) '   ROBO(1-2)=',ROBO(1),ROBO(2)
      ENDIF
        CALL LUDBRB(25,NS,0.,-ROBO(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(25,NS,-ROBO(1),0.,0.D0,0.D0,0.D0)
        DROBO(5)=-(P(IT,3)*P(IT,4)-P(N+1,4)*SQRT(P(N+1,4)**2-
     &  P(IT,4)**2+P(IT,3)**2))/(P(IT,3)**2+P(N+1,4)**2)
        CALL LUDBRB(25,NS,0.,0.,0.D0,0.D0,DROBO(5))
        ROBO(1)=ULANGL(P(N+1,3),SQRT(P(N+1,1)**2+P(N+1,2)**2))
        ROBO(2)=ULANGL(P(N+1,1),P(N+1,2))
        CALL LUDBRB(25,NS,ROBO(1),ROBO(2),0.D0,0.D0,0.D0)
      ENDIF
C     CALL GULIST(6,2)

      Q2PY=Q2
C...Hadron remnant and primordial kt
      IPY(47)=N
      CALL LYREMN(IPU1,IPU2)
      IF(IPY(48).EQ.1) THEN
        LST(21)=10
        RETURN
      ENDIF
C     CALL GULIST(7,2)

C...Transform line 1,2 and 5-7 to hadronic cms frame.
      CALL LUDBRB(1,2,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      CALL LUDBRB(1,2,-STHETA(2),0.,0.D0,0.D0,0.D0)
      CALL LUDBRB(5,7,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
      CALL LUDBRB(5,7,-STHETA(2),0.,0.D0,0.D0,0.D0)
C     CALL GULIST(8,2)

C...Rearrange partons along strings
      MSTU(24)=0
      CALL LUPREP(0)
      IF(MSTU(24).NE.0) THEN
C       CALL GULIST(88,2)
        IF(LST(3).GE.1) WRITE(6,*) ' LUPREP error MSTU(24)= ',MSTU(24)
        LST(21)=11
        RETURN
      ENDIF
C     CALL GULIST(9,2)

C...Clean up event record -> order:
C...1=inc. lepton; 2=inc. nucleon; 3=exch boson; 4=scat. lepton;
C...5=inc. parton before initial shower; 6=inc. quark at boson vertex
C...after shower; 7=scat. quark at boson vertex before final shower
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
C     K(7,J)=K(8,J)
C     P(7,J)=P(8,J)
      K(6,J)=K(NS+3,J)
      P(6,J)=P(NS+3,J)
      K(7,J)=K(IT,J)
      P(7,J)=P(IT,J)
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
C...Activate line with scattered lepton.
      K(4,1)=1
C...Deactivate obsolete lines 8, 9 and 21, NS+1 (extra lines with boson)
      K(8,1)=0
      K(9,1)=0
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
C     CALL GULIST(10,2)
C...Delete internal parton lines, i.e. with K(I,1)=13,14
      IF(MOD(LST(4)/10,10).EQ.0) THEN
        CALL LTIMEX(T1)
        CALL LUEDIT(14)
        CALL LTIMEX(T2)
C       CALL GULIST(11,2)
      ENDIF
C...Delete empty lines
      CALL LTIMEX(T1)
      CALL LUEDIT(12)
      CALL LTIMEX(T2)
C     CALL GULIST(12,2)

      RETURN
      END
