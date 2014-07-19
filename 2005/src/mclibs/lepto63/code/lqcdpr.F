C **********************************************************************

      SUBROUTINE LQCDPR(QG,QQB)

C...Probabilities for hard QCD events, qg or qqb, from integration of
C...QCD matrix elements event-by event or interpolation on x-W grid.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2),
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      DIMENSION PQSAVE(17)
      EXTERNAL DSIGMA,DSIGM2
      DATA NOUT,NABOVE/2*0/,NWARN/10/
      
      LOGICAL ZOOM

C...Get ycut from grid
      IF(LST(19).GE.0.OR.LST(19).EQ.-10) THEN
C...
C...qg and qqb event probabilities from interpolation on grid
      QG=0.
      QQB=0.
C...QCD weight zero for x->1 above grid and W small below grid
      IF(X.GT.XX(NXX).AND.X.GT.0.999) RETURN
      IF(LST(19).LT.10.AND.SQRT(W2).LT.WW(1).AND.WW(1).LT.6.) RETURN

      XP=X
C...Local variable W is W or y
      W=SQRT(W2)
      IF(LST(19).GE.10.OR.LST(19).EQ.-10) W=Y
      IF(X.LT.XX(1).OR.X.GT.XX(NXX).OR.
     &W.LT.WW(1).OR.W.GT.WW(NWW)) THEN
C...x and/or W/y outside limits of grid, write warning NWARN first times
        NOUT=NOUT+1
        IF(LST(3).GE.1.AND.NOUT.LE.NWARN)
     &  WRITE(6,1000) X,W,INT(PARI(29)),NWARN
        IF(X.LT.XX(1)) XP=XX(1)
        IF(X.GT.XX(NXX)) XP=XX(NXX)
        IF(W.LT.WW(1)) W=WW(1)
        IF(W.GT.WW(NWW)) W=WW(NWW)
      ENDIF

      IH=1
      IF(LST(30).EQ.1) IH=2
      IX=0
  100 IX=IX+1
      IF(XP.GT.XX(IX+1)) GOTO 100
      IW=0
  200 IW=IW+1
      IF(W.GT.WW(IW+1)) GOTO 200
      WD=(W-WW(IW))/(WW(IW+1)-WW(IW))
      XD=(XP-XX(IX))/(XX(IX+1)-XX(IX))

      DO 500 IP=1,NP
      X1P=(PQG(IX+1,IW,IP)-PQG(IX,IW,IP))*XD+PQG(IX,IW,IP)
      X2P=(PQG(IX+1,IW+1,IP)-PQG(IX,IW+1,IP))*XD+PQG(IX,IW+1,IP)
      QGIP=(X2P-X1P)*WD+X1P
      IF(NP.EQ.1) THEN
        QG=QGIP
        PARI(15)=MAX(QGMAX(IX,IW,IH),QGMAX(IX+1,IW+1,IH),
     &  QGMAX(IX+1,IW,IH),QGMAX(IX,IW+1,IH))
      ELSE
        QG=QG+PARI(23+IP)*QGIP
        PARI(14+IP)=MAX(QGMAX(IX,IW,IP),QGMAX(IX+1,IW+1,IP),
     &  QGMAX(IX+1,IW,IP),QGMAX(IX,IW+1,IP))
      ENDIF
      IF(IP.EQ.3) GOTO 500
      X1P=(PQQB(IX+1,IW,IP)-PQQB(IX,IW,IP))*XD+PQQB(IX,IW,IP)
      X2P=(PQQB(IX+1,IW+1,IP)-PQQB(IX,IW+1,IP))*XD+PQQB(IX,IW+1,IP)
      QQBIP=(X2P-X1P)*WD+X1P
      IF(NP.EQ.1) THEN
        QQB=QQBIP
        PARI(18)=MAX(QQBMAX(IX,IW,IH),QQBMAX(IX+1,IW+1,IH),
     &  QQBMAX(IX+1,IW,IH),QQBMAX(IX,IW+1,IH))
      ELSE
        QQB=QQB+PARI(23+IP)*QQBIP
        PARI(17+IP)=MAX(QQBMAX(IX,IW,IP),QQBMAX(IX+1,IW+1,IP),
     &  QQBMAX(IX+1,IW,IP),QQBMAX(IX,IW+1,IP))
      ENDIF
  500 CONTINUE

      IF(NP.NE.1) THEN
C...Get total x-section from interpolation to be used for normalization.
        X1P=(XTOT(IX+1,IW)-XTOT(IX,IW))*XD+XTOT(IX,IW)
        X2P=(XTOT(IX+1,IW+1)-XTOT(IX,IW+1))*XD+XTOT(IX,IW+1)
        PQ17=(X2P-X1P)*WD+X1P
        QG=QG/PQ17
        QQB=QQB/PQ17
      ENDIF

C..Interpolate in the grid
      X1P=(YCUT(IX+1,IW)-YCUT(IX,IW))*XD+YCUT(IX,IW)
      X2P=(YCUT(IX+1,IW+1)-YCUT(IX,IW+1))*XD+YCUT(IX,IW+1)
      PARL(27)=(X2P-X1P)*WD+X1P
C...Include alpha-strong in weight.
      QG=QG*PARL(25)
      QQB=QQB*PARL(25)
C...Get value of y-cut,
      IF(LST(19).GE.0) THEN
        IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
          QTOT=1.+QG+QQB
          QG =QG/QTOT
          QQB=QQB/QTOT
        ENDIF
        IF(QG+QQB.GT.1) THEN
C...Sum of QCD event probabilities larger than unity, rescale to unity
C...and print warning for first NWARN cases.
          NABOVE=NABOVE+1
          IF(LST(3).GE.1.AND.NABOVE.LE.NWARN)
     &    WRITE(6,1100) QG,QQB,X,W,INT(PARI(29)),NWARN
          QGQQB=QG+QQB
          QG=QG/QGQQB
          QQB=QQB/QGQQB
        ENDIF
      ELSE
        IF(MAX(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &         YCUT(IX+1,IW),YCUT(IX,IW+1))-
     &     MIN(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &         YCUT(IX+1,IW),YCUT(IX,IW+1)).EQ.0.0) THEN
          RETURN
        ELSE
C...Get the minimum from the grid
          PARL(27)=MIN(YCUT(IX,IW),YCUT(IX+1,IW+1),
     &                 YCUT(IX+1,IW),YCUT(IX,IW+1))
        ENDIF
      ENDIF

C...Grid
      ENDIF
      
C...Calculate probabilities directly or refine value from grid
      IF(LST(19).LE.0) THEN
      
C...qg and qqbar event probabilities (and max values for simulation)
C...obtained by integrating QCD matrix elements for each event.
C     LST2=LST(2)
C     LST(2)=-3
C     NP=1
      LST(32)=1

      DO 1 I=1,17
    1 PQSAVE(I)=PQ(I)

      PARL(25)=ULALPS(Q2)
      PARI(20)=PQ(17)
      IF(LST(19).GT.-10) THEN
        IF(LST(20).LE.1) THEN
          PARL(27)=MAX(PARL(9)**2/W2,PARL(8))
          P27MAX=1.0
        ELSEIF(LST(20).EQ.2) THEN
          PARL(27)=MAX(PARL(9)**2/Q2,PARL(8))
          P27MAX=W2/Q2
        ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
          PARL(27)=PARL(8)
          P27MAX=0.5
        ELSEIF(LST(20).EQ.6) THEN
          PARL(27)=PARL(9)
          P27MAX=W2
        ENDIF
      ELSE
        IF(LST(20).LE.1) THEN
          P27MAX=1.0
        ELSEIF(LST(20).EQ.2) THEN
          P27MAX=W2/Q2
        ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
          P27MAX=0.5
        ELSEIF(LST(20).EQ.6) THEN
          P27MAX=W2
        ENDIF
      ENDIF

      ZOOM=.FALSE.
      IYCUT=0
      YCMIN=PARL(27)
      YCMAX=PARL(27)
   10 IYCUT=IYCUT+1
      RQG=0.
      RQQB=0.
CAE.Scheme for ME cutoff: W2, Q2, mixed
      IF(LST(20).LE.1) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*(1.D0-DBLE(X))*DBLE(PARL(27)))
        XPMAX=DBLE(X)/(DBLE(X)+(1.D0-DBLE(X))*DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.2) THEN
        XPMIN=DBLE(X)/(1.D0-2.D0*DBLE(X)*DBLE(PARL(27)))
        XPMAX=1.D0/(1.D0+DBLE(PARL(27)))
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        XPMIN=X
        XPMAX=1./(1.+PARL(9))
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(9))
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(27))
      ELSE
        WRITE(6,*) 'LQCDPR: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.LT.X.OR.XPMIN.GT.1.) GOTO 40
      IF(XPMIN.GE.XPMAX) GOTO 40

      PARI(15)=0.
      PARI(16)=0.
      PARI(18)=0.
      PARI(19)=0.
C...QCD-Compton -> qg-event
      LST(24)=2
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RQG)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RQG)
C...QCD-fusion  -> qq-event
      LST(24)=3
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RQQB)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RQQB)
C...q-event
      RQ=1.-RQG-RQQB
CAE      WRITE(6,*) IYCUT,RQ,PARL(27),YCMIN,YCMAX
      IF(.NOT.ZOOM) THEN
CAE.First find interval so that RQ>0
        IF(RQ.LT.0.AND.IYCUT.LT.10) THEN
          PARL(27)=MIN(1.1*EXP(-2.0*RQ)*PARL(27),P27MAX)
          YCMIN=YCMAX
          YCMAX=PARL(27)
        ELSEIF(RQ.LT.0.AND.IYCUT.GE.10) THEN
C...Terminate procedure after some iterations
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/RTOT
          RQQB=RQQB/RTOT
          RQ=1.-RQG-RQQB
C          IF(LST(3).GE.1) THEN
C            WRITE(6,*) 'Warning! sigma>tot for x,q2,cut=',X,Q2,PARL(27)
C            WRITE(6,*) 'Weights set to=',RQ,RQG,RQQB
C          ENDIF
C...Break loop
          GOTO 40
        ELSEIF(IYCUT.GE.2.AND.RQ.GT.PARL(13)) THEN
C...If RQ>PARL(13), then ycut was increased to much
          ZOOM=.TRUE.
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSE
C...correct ycut found
          GOTO 40
        ENDIF
      ELSE
C...Zoom in on ycut so that 0<RQ<PARL(13)
        IF(RQ.LT.0.AND.IYCUT.LT.40) THEN
          YCMIN=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(RQ.GT.PARL(13).AND.IYCUT.LT.40) THEN
          YCMAX=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
C...Catch infinite loop
        ELSEIF(IYCUT.GE.40) THEN
          IF(LST(3).GE.1) THEN
            WRITE(6,*) 'LQCDPR: Warning, PARL(27) not found.'
          ENDIF
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/MAX(1.0,RTOT)
          RQQB=RQQB/MAX(1.0,RTOT)
          RQ=1.-RQG-RQQB
C...Break loop
          GOTO 40
        ELSE
C...ycut found, break loop
          GOTO 40
        ENDIF
      ENDIF
C...Loop until correct weights found
      GOTO 10

   40 CONTINUE
CAE
      IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
        RQ=1.-RQG-RQQB
      ENDIF

C     LST(2)=LST2
      LST(32)=0
      DO 90 I=1,17
   90 PQ(I)=PQSAVE(I)
      QG=RQG
      QQB=RQQB

C...Refine      
      ENDIF
      
 1000 FORMAT(' Warning: x=',F7.4,' or W/y=',F10.4,' outside QCD grid',
     &' in event no.',I8,/,10X,
     &'weight on limit of grid used. Only first',I5,' warnings printed')
 1100 FORMAT(' Warning: Sum of QCD probabilities larger than unity ',
     &' QG, QQB =',2F8.4,/10X,'occurs at x, W/y =',2F10.4,
     &' in event no.',I8,/,10X,
     &'Weights rescaled to unit sum. Only first',I5,' warnings printed')
      RETURN
      END
