
C **********************************************************************

      SUBROUTINE FLTABL

C...Integrates the longitudinal structure function, store on grid
C...in x, Q**2.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LINTEG/ NTOT,NPASS
      COMMON /FLGRID/ NFX,NFQ,XR(2),QR(2),FLQT(41,16),FLGT(41,16),
     &FLMT(41,16)
      EXTERNAL FLQINT,FLGINT,FLTINT

      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      LHT=LST(11)/100
      IF(LST(3).GE.3) WRITE(6,1000) LST(11),LQCD,LTM,LHT
      IF(LQCD.LT.1.AND.LTM.LT.1) RETURN
      CALL LTIMEX(T1)
      DO 10 IX=1,NFX
      DO 10 IQ=1,NFQ
      FLQT(IX,IQ)=0.
      FLGT(IX,IQ)=0.
   10 FLMT(IX,IQ)=0.
      QR(1)=Q2MIN
      XR(1)=XMIN
      XR(2)=XMAX
      DO 500 IX=1,NFX
      X=10**(ALOG10(XR(1))+(ALOG10(XR(2))-ALOG10(XR(1)))*(IX-1)/(NFX-1))
      QR(2)=X*PARL(21)
      IF(QR(1).GT.QR(2)) GOTO 500
      LQ=0
      DO 400 IQ=1,NFQ
      Q2=10**(ALOG10(QR(1))+(ALOG10(QR(2))-ALOG10(QR(1)))*
     &(IQ-1)/(NFQ-1))
Ctest IF(LQ.GT.0) GOTO 500
      IF(Q2.GT.PARL(21)) LQ=LQ+1
      Y=Q2/(PARL(21)*X)
      IF(Y.LT.0.0.OR.Y.GT.1.0) LQ=LQ+1
      PARL(25)=ULALPS(Q2)
      IF(LQCD.EQ.1) THEN
C...Quark part.
        ACCUR=PARL(11)
        IT=0
  100   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLQINT,EPS,FLQ)
        IF(FLQ.LT.1) THEN
          ACCUR=FLQ*PARL(11)
          IF(IT.LT.2) GOTO 100
        ENDIF
        FLQT(IX,IQ)=FLQ
C...Gluon part.
        ACCUR=PARL(11)
        IT=0
  200   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLGINT,EPS,FLG)
        IF(FLG.LT.1.) THEN
          ACCUR=FLG*PARL(11)
          IF(IT.LT.2) GOTO 200
        ENDIF
        FLGT(IX,IQ)=FLG
      ENDIF
      IF(LTM.EQ.1) THEN
C...Target mass  part.
        ACCUR=PARL(11)
        IT=0
  300   IT=IT+1
        NTOT=0
        NPASS=0
        EPS=ACCUR
        CALL GADAP(X,1.,FLTINT,EPS,FLM)
        IF(FLM.LT.1) THEN
          ACCUR=FLM*PARL(11)
          IF(IT.LT.2) GOTO 300
        ENDIF
        FLMT(IX,IQ)=FLM
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE
      CALL LTIMEX(T2)
      IF(LST(3).GE.3) WRITE(6,1100) T2-T1
      RETURN

 1000 FORMAT(' Initialisation for FL; QCD, target mass, higher twist: ',
     &/,' LST(11) =',I5,' --> LQCD, LTM, LHT =',3I3)
 1100 FORMAT(' FL integrations performed if LQCD=1 and/or LTM=1, ',
     &'results on grid.'/,' Time for FL integrations is ',F7.1,' sec.')
      END
