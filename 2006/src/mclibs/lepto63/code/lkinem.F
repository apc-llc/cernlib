
C **********************************************************************

      FUNCTION LKINEM(L)

C...Calculate kinematical variables and reject (optionally) if outside
C...required limits.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      DOUBLE PRECISION DTHETA,DPHI,DBETA,DE,DPZ,DPT,DETOT

      LKINEM=1
      IF(L.EQ.-3) THEN
C...x,W known from LWEITS, no cuts applied.
        U=(W2-P(2,5)**2)/(2.*P(2,5)*(1.-X))
        Q2=2.*P(2,5)*U*X
        Y=Q2/(PARL(21)*X)
        GOTO 200
      ENDIF
C...x,y given.
      PARL(22)=Y*PARL(21)
      Q2=X*PARL(22)
      U=PARL(22)/(2.*P(2,5))
      W2=PARL(22)*(1.-X)+P(2,5)**2
      P(4,5)=ULMASS(K(4,2))
      IF(P(4,5)/SQRT(PARL(21)).LT.0.001) THEN
C...Simpler formulae for effectively massless scattered lepton.
        DE=DBLE(P(1,4))*(1.-DBLE(Y))+DBLE(X)*DBLE(Y)*DBLE(ABS(P(2,3)))
        DPZ=DE-DBLE(X)*DBLE(Y)*(DBLE(P(2,4))+DBLE(ABS(P(2,3))))
      ELSE
C...Formulae for massive scattered lepton.
        DE=DBLE(P(1,4))+(DBLE(ABS(P(2,3)))*(DBLE(Q2)+DBLE(P(1,5))**2+
     &  DBLE(P(4,5))**2)/(2.D0*DBLE(P(1,4)))-DBLE(PARL(22))/2.D0)/
     &  (DBLE(P(2,4))+DBLE(ABS(P(2,3))))
        DPZ=DBLE(P(1,4))-(DBLE(P(2,4))*(DBLE(Q2)+DBLE(P(1,5))**2+
     &  DBLE(P(4,5))**2)/(2.D0*DBLE(P(1,4)))+DBLE(PARL(22))/2.D0)/
     &  (DBLE(P(2,4))+DBLE(ABS(P(2,3))))
      ENDIF
      DPT=DE**2-DPZ**2-DBLE(P(4,5))**2
      IF(DPT.LT.0.D0) RETURN
      DPT=SQRT(DPT)
      P(4,1)=DPT
      P(4,2)=0.
      P(4,3)=DPZ
      P(4,4)=DE
      P(3,1)=-DPT
      P(3,2)=0.
      P(3,3)=DBLE(P(1,3))-DPZ
      P(3,4)=DBLE(P(1,4))-DE
      P(3,5)=-SQRT(Q2)
      K(3,3)=1
      K(4,3)=1
      N=4
      IF(L.EQ.3) GOTO 200

      IF(X.LT.XMIN.OR.X.GT.XMAX) RETURN
      IF(Y.LT.YMIN.OR.Y.GT.YMAX) RETURN
      IF(Q2.LT.Q2MIN.OR.Q2.GT.Q2MAX) RETURN
      IF(W2.LT.W2MIN.OR.W2.GT.W2MAX) RETURN
C-check: CUT(9),CUT(10) --> UMIN,UMAX needs change in /LINTRL/ --> next update 
      IF(U.LT.CUT(9).OR.U.GT.CUT(10)) RETURN
      IF(LST(17).EQ.0) THEN
        IF(P(4,4).LT.CUT(11).OR.P(4,4).GT.CUT(12))  RETURN
        THETAL=PLU(4,13)
C       THETAL=ACOS((P(1,1)*P(4,1)+P(1,2)*P(4,2)+P(1,3)*P(4,3))
C    &  /SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2)/
C    &  SQRT(P(4,1)**2+P(4,2)**2+P(4,3)**2))
      ELSE
C...No cuts on energy, angle for initialisation of varying energy mode
        IF(LST(32).NE.0) GOTO 200
C...Transform scattered lepton back to lab system to make cut
C...in energy and angle (defined as space angle to incoming lepton).
        DO 110 J=1,5
        K(6,J)=K(4,J)
  110   P(6,J)=P(4,J)
        CALL LUDBRB(6,6,STHETA(1),SPHI(1),0.D0,0.D0,0.D0)
        CALL LUDBRB(6,6,0.,0.,DBETA(1,1),DBETA(1,2),DBETA(1,3))
        IF(P(6,4).LT.CUT(11).OR.P(6,4).GT.CUT(12))  RETURN
        THETAL=ACOS((PSAVE(3,1,1)*P(6,1)+PSAVE(3,1,2)*P(6,2)+
     &  PSAVE(3,1,3)*P(6,3))
     &  /SQRT(PSAVE(3,1,1)**2+PSAVE(3,1,2)**2+PSAVE(3,1,3)**2)/
     &  SQRT(P(6,1)**2+P(6,2)**2+P(6,3)**2))
      ENDIF
      IF(THETAL.LT.CUT(13).OR.THETAL.GT.CUT(14))  RETURN
  200 LKINEM=0
      RETURN
      END
