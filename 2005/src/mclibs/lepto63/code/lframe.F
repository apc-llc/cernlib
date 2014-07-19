
C **********************************************************************

      SUBROUTINE LFRAME(IFR,IPH)

C...Make transformation from hadronic CM frame to lab frame.

      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DTHETA,DPHI,DBETA
      INTEGER IFR,IPH,IFRAME,IPHI

      IFRAME=IFR
      IPHI=IPH
      IF(IFRAME.LT.1.OR.IFRAME.GT.4.OR.IPHI.LT.0.OR.IPHI.GT.1) GOTO 999
      IF(IFRAME.EQ.1) IPHI=0

CGI -- Also boost lines up to N+MSTU(3)
      N=N+1+MSTU(3)
      DO 5 J=1,5
    5 P(N,J)=PB(J)

   10 CONTINUE
      IF(IPHI.NE.LST(29)) THEN
         IFRAME=2
      ELSE
         IFRAME=IFR
      ENDIF
      IF((IFRAME.EQ.LST(28)).AND.(IPHI.EQ.LST(29))) THEN
        DO 15 J=1,5
   15   PB(J)=P(N,J)
        N=N-1-MSTU(3)
        RETURN
      ENDIF

      GOTO(100,200,300,400), LST(28)
      GOTO 999

  100 IF(IFRAME.GE.2) THEN
        CALL LUDBRB(0,0,STHETA(2),SPHI(2),0.D0,0.D0,0.D0)
        CALL LUDBRB(0,0,0.,0.,DBETA(2,1),DBETA(2,2),DBETA(2,3))
        LST(28)=2
      ELSE
        GOTO 999
      ENDIF
      GOTO 10

  200 IF(IPHI.NE.LST(29)) THEN
       CALL LUDBRB(0,0,0.,SIGN(PHIR,FLOAT(IPHI-LST(29))),0.D0,0.D0,0.D0)
       LST(29)=IPHI
      ENDIF

      IF(IFRAME.EQ.1) THEN
        CALL LUDBRB(0,0,0.,0.,-DBETA(2,1),-DBETA(2,2),-DBETA(2,3))
        CALL LUDBRB(0,0,-STHETA(2),0.,0.D0,0.D0,0.D0)
        LST(28)=1
      ELSEIF(IFRAME.GE.3) THEN
        IF(LST(17).EQ.0) THEN
          CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,DBETA(1,3))
          IF(PSAVE(3,1,3).LT.0.) THEN
            DO 210 I=1,N
            V(I,3)=-V(I,3)
  210       P(I,3)=-P(I,3)
          ENDIF
        ELSE
          CALL LUDBRB(0,0,STHETA(1),SPHI(1),0.D0,0.D0,0.D0)
          CALL LUDBRB(0,0,0.,0.,DBETA(1,1),DBETA(1,2),DBETA(1,3))
        ENDIF
        LST(28)=3
      ENDIF
      GOTO 10

  300 IF(IFRAME.LE.2) THEN
        IF(LST(17).EQ.0) THEN
          IF(PSAVE(3,1,3).LT.0.) THEN
            DO 310 I=1,N
            V(I,3)=-V(I,3)
  310       P(I,3)=-P(I,3)
          ENDIF
          CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        ELSE
          CALL LUDBRB(0,0,0.,0.,-DBETA(1,1),-DBETA(1,2),-DBETA(1,3))
          CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
          CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        ENDIF
        LST(28)=2
      ELSEIF(IFRAME.EQ.4) THEN
        THEBOS=PLU(N,13)
        PHIBOS=PLU(N,15)
        CALL LUDBRB(0,0,0.,-PHIBOS,0.D0,0.D0,0.D0)
        CALL LUDBRB(0,0,-THEBOS,0.,0.D0,0.D0,0.D0)
        LST(28)=4
      ENDIF
      GOTO 10

  400 IF(IFRAME.LE.3) THEN
        CALL LUDBRB(0,0,THEBOS,PHIBOS,0.D0,0.D0,0.D0)
        LST(28)=3
      ENDIF
      GOTO 10

  999 WRITE(6,1000) IFRAME,IPHI,LST(28),LST(29)
 1000 FORMAT(' BAD VARIABLES IN SUBROUTINE LFRAME: IFRAME,IPHI,',
     &'LST(28),LST(29) =',4I5)
      RETURN
      END
