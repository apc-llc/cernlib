
C **********************************************************************

      SUBROUTINE LWEITS(LFILE)

C...Integrates the QCD matrix elements to obtain probabilities for
C...qg- and qq-events as a function of (x,W). Also finds various
C...maximum values to be used for the QCD simulation. Results stored
C...in common LGRID and optionally written to logical file LFILE.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2),
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      DIMENSION WWI(21,4),XXI(31,4)
      EXTERNAL DSIGMA,DSIGM2
      LOGICAL ZOOM
      DATA WWI/
     1  5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,
     1  20.,21.,22.,23.,24.,25.,
     2  5.,7.,9.,11.,13.,15.,17.,19.,21.,23.,25.,27.,29.,31.,33.,
     2  35.,37.,39.,41.,43.,45.,
     3  5.,7.,10.,15.,20.,25.,30.,40.,50.,75.,100.,125.,150.,175.,
     3  200.,225.,250.,275.,300.,325.,350.,
     4  5.,10.,15.,20.,30.,50.,75.,100.,150.,200.,250.,300.,400.,
     4  500.,700.,900.,1200.,1500.,1800.,2100.,2500./
      DATA XXI/
     1  1.E-3,2.E-3,3.E-3,4.E-3,5.E-3,6.E-3,8.E-3,
     1  1.E-2,2.E-2,3.E-2,4.E-2,5.E-2,6.E-2,8.E-2,
     1  .1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.72,.8,.87,.94,.999,
     2  1.E-3,2.E-3,3.E-3,4.E-3,5.E-3,6.E-3,8.E-3,
     2  1.E-2,2.E-2,3.E-2,4.E-2,5.E-2,6.E-2,8.E-2,
     2  .1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.72,.8,.87,.94,.999,
     3  1.E-5,2.E-5,4.E-5,6.E-5,8.E-5,1.E-4,2.E-4,4.E-4,6.E-4,8.E-4,
     3  1.E-3,2.E-3,4.E-3,6.E-3,8.E-3,1.E-2,2.E-2,4.E-2,6.E-2,8.E-2,
     3  .1,.2,.3,.4,.5,.6,.7,.8,.87,.94,.999,
     4  1.E-5,2.E-5,4.E-5,6.E-5,8.E-5,1.E-4,2.E-4,4.E-4,6.E-4,8.E-4,
     4  1.E-3,2.E-3,4.E-3,6.E-3,8.E-3,1.E-2,2.E-2,4.E-2,6.E-2,8.E-2,
     4  .1,.2,.3,.4,.5,.6,.7,.8,.87,.94,.999/
      DATA NCALL/0/,XSPLIT/0.1/,YSPLIT/2./

      NCALL=NCALL+1
      LST2=LST(2)
      LST(2)=-3
      WMAX=SQRT(PARL(21))

      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &WRITE(6,1000) PARL(11),LST(13),MSTU(112),PARU(112),
     &PARL(8),PARL(9),PARL(12),PARL(13)
      IF(LST(17).EQ.0) THEN
        NP=1
        IPMAX=2
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1010)
      ELSE
        NP=3
        IF(LST(23).EQ.1) NP=2
        IPMAX=3
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) THEN
          IF(LST(19).GE.0.AND.LST(19).LT.10) WRITE(6,1020)
          IF(LST(19).GE.10.OR.LST(19).EQ.-10) WRITE(6,2020)
        ENDIF
      ENDIF

      IF(LST(19).GE.0.AND.LST(19).LT.10) THEN
C...Fixed grid in x and W
        IF(LST(19).EQ.0)THEN
C...Grid specified by user.
          READ(5,*) NWW,NXX
          READ(5,*) (WW(IW),IW=1,NWW)
          READ(5,*) (XX(IX),IX=1,NXX)
          IF(XX(NXX).GT..99) XX(NXX)=.99
        ELSEIF(LST(19).GE.1.AND.LST(19).LE.4) THEN
C...Grid taken from data in arrays WWI, XXI.
          DO 10 IW=1,NWW
   10     WW(IW)=WWI(IW,LST(19))
          DO 20 IX=1,NXX
   20     XX(IX)=XXI(IX,LST(19))
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1030) LST(19),NXX,NWW,XX,WW
        IF(WMAX.GT.WW(NWW)) THEN
          IF(LST(3).GE.1) WRITE(6,1040) WMAX,WW(NWW)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1100)
      ELSEIF(LST(19).GE.10.OR.LST(19).EQ.-10) THEN
C...Grid in x,y automatically defined from available x,y-ranges
        NX=NXX
        NY=NWW
        IF(XMIN.GE.XSPLIT) THEN
          DO 30 I=1,NX
   30     XX(I)=MIN(0.999,XMIN+(XMAX-XMIN)*(I-1)/FLOAT(NX-1))
        ELSEIF(XMAX.GT.XSPLIT) THEN
          NL=MIN(2.*NX/3.,MAX(NX/3.,NX*LOG(XSPLIT/XMIN)/LOG(XMAX/XMIN)))
          DO 40 I=1,NL
   40     XX(I)=MIN(0.999,
     &   10.**(LOG10(XMIN)+(LOG10(XSPLIT)-LOG10(XMIN))*(I-1)/FLOAT(NL)))
          DO 41 I=NL+1,NX
   41     XX(I)=MIN(0.999,XSPLIT+(XMAX-XSPLIT)*(I-NL-1)/FLOAT(NX-NL-1))
        ELSE
          DO 50 I=1,NX
   50     XX(I)=MIN(0.999,
     &   10.**(LOG10(XMIN)+(LOG10(XMAX)-LOG10(XMIN))*(I-1)/FLOAT(NX-1)))
        ENDIF
C...y-variable stored in same array as W
        IF(YMIN.GE.YSPLIT) THEN
          DO 60 I=1,NY
   60     WW(I)=MIN(0.999,YMIN+(YMAX-YMIN)*(I-1)/FLOAT(NY-1))
        ELSEIF(YMAX.GT.YSPLIT) THEN
          NL=MIN(2.*NY/3.,MAX(NY/3.,NY*LOG(YSPLIT/YMIN)/LOG(YMAX/YMIN)))
          DO 70 I=1,NL
   70     WW(I)=MIN(0.999,
     &   10.**(LOG10(YMIN)+(LOG10(YSPLIT)-LOG10(YMIN))*(I-1)/FLOAT(NL)))
          DO 71 I=NL+1,NY
   71     WW(I)=MIN(0.999,YSPLIT+(YMAX-YSPLIT)*(I-NL-1)/FLOAT(NY-NL-1))
        ELSE
          DO 80 I=1,NY
   80     WW(I)=MIN(0.999,
     &   10.**(LOG10(YMIN)+(LOG10(YMAX)-LOG10(YMIN))*(I-1)/FLOAT(NY-1)))
        ENDIF
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,2030) LST(19),NXX,NWW,XX,WW
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,2100)
      ENDIF

      LW=0
      DO 500 IW=1,NWW
      W=WW(IW)
      Y=WW(IW)
      IF(LW.GT.0) GOTO 600
      IF(LST(19).LT.10.AND.W.GT.WMAX) LW=LW+1
      LX=0
      DO 400 IX=1,NXX
      X=XX(IX)
      IF(LST(19).GE.0.AND.LST(19).LT.10) THEN
C...x,W given.
        W2=W**2
        U=(W2-PSAVE(3,2,5)**2)/(2.*PSAVE(3,2,5)*(1.-X))
        Q2=2.*PSAVE(3,2,5)*U*X
        Y=Q2/(PARL(21)*X)
      ELSEIF(LST(19).GE.10.OR.LST(19).EQ.-10) THEN
C...x,y given.
        PARL(22)=Y*PARL(21)
        Q2=X*PARL(22)
        U=PARL(22)/(2.*PSAVE(3,2,5))
        W2=PARL(22)*(1.-X)+PSAVE(3,2,5)**2
        W=SQRT(W2)
      ENDIF
C...Protection against too small Q2 in structure functions
Ctest IF(Q2.LT.2.098) GOTO 400
      IF(LX.GT.0) GOTO 500
      IF(LST(19).GE.0.AND.LST(19).LT.10.AND.X.GT.1.-W2/WMAX**2) LX=LX+1
      CALL LEPTO
      PQCOM=PARI(31)*PQ(17)*COMFAC
      PARL(25)=ULALPS(Q2)
      PARI(20)=PQ(17)
      XTOT(IX,IW)=PQ(17)
      IF(LST(20).LE.1) THEN
        PARL(27)=MAX(PARL(9)**2/W2,PARL(8))
        P27MAX=1.0
      ELSEIF(LST(20).EQ.2) THEN
        PARL(27)=MAX(PARL(9)**2/Q2,PARL(8))
        P27MAX=W2/Q2
      ELSEIF(LST(20).EQ.3.OR.LST(20).EQ.4) THEN
        PARL(27)=PARL(8)
        P27MAX=0.5
      ELSEIF(LST(20).EQ.5) THEN
        PARL(27)=PARL(8)
        P27MAX=0.5
      ELSEIF(LST(20).EQ.6) THEN
        PARL(27)=PARL(9)
        P27MAX=W2
      ENDIF

      ZOOM=.FALSE.
      YCMIN=PARL(27)
      YCMAX=PARL(27)
      IYCUT=0
  100 IYCUT=IYCUT+1
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
        XPMAX=1.D0/(1.D0+DBLE(PARL(9)))
      ELSEIF(LST(20).EQ.5) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(9))
      ELSEIF(LST(20).EQ.6) THEN
        XPMIN=X
        XPMAX=Q2/(Q2+PARL(27))
      ELSE
        WRITE(6,*) 'LWEITS: No such jet scheme!'
      ENDIF
CAE
      IF(XPMIN.GE.XPMAX.OR.XPMIN.LE.0.) GOTO 210
      DO 200 IP=1,NP
      IF(LST(17).EQ.0) THEN
        PARI(15)=0.
        PARI(16)=0.
        PARI(18)=0.
        PARI(19)=0.
      ELSE
        PARI(14+IP)=0.
        IF(IP.LE.2) PARI(17+IP)=0.
      ENDIF
      LST(32)=IP
      LST(24)=2
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RESULT)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RESULT)
      RQG=RQG+RESULT
      PQG(IX,IW,IP)=RESULT/PARL(25)
      IF(LST(17).EQ.0) THEN
        QGMAX(IX,IW,1)=PARI(15)
        QGMAX(IX,IW,2)=PARI(16)
      ELSE
        PQG(IX,IW,IP)=RESULT*PARI(20)/PARI(23+IP)/PARL(25)
        QGMAX(IX,IW,IP)=PARI(14+IP)
      ENDIF
      IF(IP.EQ.3) GOTO 200
      LST(24)=3
      EPS=PARL(11)
CAE      CALL GADAP(XPMIN,XPMAX,DSIGMA,EPS,RESULT)
      CALL GADAP(LOG(1.0-XPMAX),LOG(1.0-XPMIN),DSIGM2,EPS,RESULT)
      RQQB=RQQB+RESULT
      PQQB(IX,IW,IP)=RESULT/PARL(25)
      IF(LST(17).EQ.0) THEN
        QQBMAX(IX,IW,1)=PARI(18)
        QQBMAX(IX,IW,2)=PARI(19)
      ELSE
        PQQB(IX,IW,IP)=RESULT*PARI(20)/PARI(23+IP)/PARL(25)
        QQBMAX(IX,IW,IP)=PARI(17+IP)
      ENDIF
  200 CONTINUE
  210 CONTINUE
      RQ=1.-RQG-RQQB
      IF(.NOT.ZOOM) THEN
CAE.First find interval so that RQ>0
        IF(RQ.LT.0.AND.IYCUT.LT.10) THEN
          PARL(27)=MIN(1.1*EXP(-2.0*RQ)*PARL(27),P27MAX)
          YCMIN=YCMAX
          YCMAX=PARL(27)
        ELSEIF(RQ.LT.0.AND.IYCUT.GE.10) THEN
C...Terminate procedure after some iterations
          WRITE(6,*) 'Warning! sigma>tot for x,q2,cut=',X,Q2,PARL(27)
          WRITE(6,*) 'Weights=',RQ,RQG,RQQB
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/RTOT
          RQQB=RQQB/RTOT
          RQ=1.-RQG-RQQB
          DO 220 IP=1,3
            PQG(IX,IW,IP)=PQG(IX,IW,IP)/RTOT
            QGMAX(IX,IW,IP)=QGMAX(IX,IW,IP)/RTOT
220       CONTINUE
          DO 230 IP=1,2
            PQQB(IX,IW,IP)=PQQB(IX,IW,IP)/RTOT
            QQBMAX(IX,IW,IP)=QQBMAX(IX,IW,IP)/RTOT
230       CONTINUE
C...Break loop
          GOTO 250
        ELSEIF(IYCUT.GE.2.AND.RQ.GT.PARL(13)) THEN
C...If RQ>PARL(13), then ycut was increased to much
          ZOOM=.TRUE.
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSE
C...correct ycut found
          GOTO 250
        ENDIF
      ELSE
C...Zoom in on ycut so that 0<RQ<PARL(13)
        IF(RQ.LT.0.AND.IYCUT.LT.100) THEN
          YCMIN=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(RQ.GT.PARL(13).AND.IYCUT.LT.100) THEN
          YCMAX=PARL(27)
          PARL(27)=(YCMIN+YCMAX)/2.
        ELSEIF(IYCUT.GE.100) THEN
          IF(LST(3).GE.1) THEN
            WRITE(6,*) 'LWEITS: Warning, PARL(27) not found.'
          ENDIF
          RTOT=(RQG+RQQB)*1.05
          RQG=RQG/MAX(1.0,RTOT)
          RQQB=RQQB/MAX(1.0,RTOT)
          RQ=1.-RQG-RQQB
C...Break loop
          GOTO 250        
        ELSE
C...ycut found, break loop
          GOTO 250
        ENDIF
      ENDIF
C...Loop until correct weights found
      GOTO 100

 250  CONTINUE
CAE
      YCUT(IX,IW)=PARL(27)
      IF(LST(33).EQ.-91) THEN
Ctest...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
        RQ=1.-RQG-RQQB
      ENDIF
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) THEN
        IF(LST(19).LT.10) WRITE(6,1200) W,X,Y,Q2,PARL(25),PQCOM,
     &  PARL(27),IYCUT,RQ,RQG,RQQB,(QGMAX(IX,IW,IP),IP=1,IPMAX),
     &  (QQBMAX(IX,IW,IP),IP=1,MIN(2,IPMAX))
        IF(LST(19).GE.10) WRITE(6,2200) X,Y,Q2,W,PARL(25),PQCOM,
     &  PARL(27),IYCUT,RQ,RQG,RQQB,(QGMAX(IX,IW,IP),IP=1,IPMAX),
     &  (QQBMAX(IX,IW,IP),IP=1,MIN(2,IPMAX))
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE

      LST(2)=LST2
      IF(LFILE.LT.0) THEN
C...Write results on logical file number IABS(LFILE)
        WRITE(IABS(LFILE)) LST,PARL,NXX,NWW,NP,XX,WW
        WRITE(IABS(LFILE))(((PQG(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((PQQB(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((QGMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,IPMAX),
     &  (((QQBMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,MIN(2,IPMAX)),
     &  YCUT
        IF(NP.NE.1) WRITE(IABS(LFILE)) XTOT
        CLOSE(IABS(LFILE))
      ENDIF
      RETURN

 1000 FORMAT('1',/,5X,'Integration of 1st order QCD matrix elements',
     &           /,5X,'============================================',
     &/,' for gluon radiation (qg-event) and boson-gluon fusion ',
     &'(qq-event) probability.',
     &//,' Required precision in integration, PARL(11) =',F8.4,
     &//,' Heaviest flavour produced in boson-gluon fusion, LST(13) =',
     &I5,//,' Alpha-strong parameters: # flavours, MSTU(112) =',I3,
     &/,25X,' QCD lambda, PARU(112) =',F6.3,' GeV',
     &//,' Cuts on matrix elements:',
     &/,' PARL(8), PARL(9), PARL(12), PARL(13) =',4F8.4,/)
 1010 FORMAT(' Lepton energy not allowed to vary in simulation.',/)
 1020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' y in table below calculated assuming max energy.',/)
 1030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, W =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' W-values in array WW:',/,10F7.1,/,11F7.1,/)
 1040 FORMAT(' Warning: max W outside grid, Wmax, grid-max =',2F12.1)
 1100 FORMAT(//,6X,'W',7X,'x',7X,'y',6X,'Q**2',1X,'alpha',1X,'dsigma',
     &9X,'cut',' it',2X,'q-event',1X,'qg-event',
     &1X,'qq-event',' max of matrix elements qg & qq; L,R or T,S,I',
     &/,1X,132(1H-),/)
 1200 FORMAT(F7.1,2F8.4,1PG10.3,0PF6.2,1PG11.3,0PF8.4,I3,3F9.4,1P,5E9.2)
 1900 FORMAT(' Execution stopped ',/)
 2020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' W in table below calculated assuming max energy.',/)
 2030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, y =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' y-values in array WW:',/,10F7.4,/,11F7.4,/)
 2100 FORMAT(//,7X,'x',7X,'y',6X,'Q**2',6X,'W',1X,'alpha',1X,'dsigma',
     &9X,'cut',' it',2X,'q-event',1X,'qg-event',
     &1X,'qq-event',' max of matrix elements qg & qq; L,R or T,S,I',
     &/,1X,132(1H-),/)
 2200 FORMAT(2F8.5,1PG10.3,0PF7.1,F6.2,1PG11.3,0PF8.4,I3,3F9.4,1P,5E9.2)
      END
