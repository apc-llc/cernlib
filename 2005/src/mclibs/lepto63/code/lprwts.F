
C **********************************************************************

      SUBROUTINE LPRWTS(NSTEP)

C...Prints probabilities for q-, qg- and qqbar-events using the present
C...QCD weights stored in common block LGRID.

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2),
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ

      WMAX=SQRT(PARL(21)+PSAVE(3,1,5)**2+PSAVE(3,2,5)**2)
      WRITE(6,1000) PARL(11),LST(13),MSTU(112),PARU(112),
     &PARL(8),PARL(9),PARL(12),PARL(13)
      IF(NP.EQ.1) THEN
        WRITE(6,1010)
      ELSE
        IF(LST(19).LT.10) WRITE(6,1020)
        IF(LST(19).GE.10) WRITE(6,2020)
      ENDIF
      IF(LST(19).LT.10) THEN
        WRITE(6,1030) LST(19),NXX,NWW,XX,WW
        IF(WMAX.GT.WW(NWW)) WRITE(6,1040) WMAX,WW(NWW)
        WRITE(6,1100)
      ELSEIF(LST(19).GE.10) THEN
        WRITE(6,2030) LST(19),NXX,NWW,XX,WW
        WRITE(6,2100)
      ENDIF

      LW=0
      DO 500 IW=1,NWW,MAX(1,NSTEP)
      W=WW(IW)
      Y=WW(IW)
      IF(LW.GT.0) GOTO 600
      IF(LST(19).LT.10.AND.W.GT.WMAX) LW=LW+1
      W2=W**2
      LX=0
      DO 400 IX=1,NXX,MAX(1,NSTEP)
      X=XX(IX)
      IF(LX.GT.0) GOTO 500
      IF(LST(19).LT.10) THEN
C...x,W2 given.
        U=(W2-PSAVE(3,2,5)**2)/(2.*PSAVE(3,2,5)*(1.-X))
        Q2=2.*PSAVE(3,2,5)*U*X
        Y=Q2/(PARL(21)*X)
      ELSEIF(LST(19).GE.10) THEN
C...x,y given.
        PARL(22)=Y*PARL(21)
        Q2=X*PARL(22)
        U=PARL(22)/(2.*PSAVE(3,2,5))
        W2=PARL(22)*(1.-X)+PSAVE(3,2,5)**2
      ENDIF
      PARI(24)=(1.+(1.-Y)**2)/2.
      PARI(25)=1.-Y
      PARI(26)=(1.-(1.-Y)**2)/2.
      PARL(25)=ULALPS(Q2)
      IF(Y.GT.1.) LX=LX+1
      RQG=0.
      RQQB=0.
      DO 200 IP=1,NP
      IF(NP.EQ.1) THEN
        RQG=PQG(IX,IW,IP)
        RQQB=PQQB(IX,IW,IP)
      ELSE
        RQG=RQG+PQG(IX,IW,IP)*PARI(23+IP)/XTOT(IX,IW)
        IF(IP.LT.3) RQQB=RQQB+PQQB(IX,IW,IP)*PARI(23+IP)/XTOT(IX,IW)
      ENDIF
  200 CONTINUE
C...Include alpha-strong in weight.
      RQG=RQG*PARL(25)
      RQQB=RQQB*PARL(25)
      IF(LST(33).EQ.-91) THEN
C...Include 3-jet cross section in denominator
        QTOT=1.+RQG+RQQB
        RQG =RQG/QTOT
        RQQB=RQQB/QTOT
      ENDIF
      RQ=1.-RQG-RQQB
      IF(LST(19).LT.10) THEN
        WRITE(6,1200) W,X,Y,Q2,PARL(25),YCUT(IX,IW),RQ,RQG,RQQB
      ELSEIF(LST(19).GE.10) THEN
        WRITE(6,2200) X,Y,Q2,W,PARL(25),YCUT(IX,IW),RQ,RQG,RQQB
      ENDIF
  400 CONTINUE
  500 CONTINUE
  600 CONTINUE
      RETURN

 1000 FORMAT('1',/,5X,'SUMMARY OF QCD MATRIX ELEMENT INTEGRATION',
     &           /,5X,'-----------------------------------------',//,
     &/,' for gluon radiation (qg-event) and boson-gluon fusion ',
     &'(qq-event) probability.',
     &//,' Required precision in integration, PARL(11) =',F8.4,
     &//,' Heaviest flavour produced in boson-gluon fusion, LST(13) =',
     &I5,//,' Alpha-strong parameters: # flavours, MSTU(112) =',I3,
     &'  QCD lambda, PARU(112) =',F6.3,' GeV',
     &//,' Cuts on matrix elements:',
     &/,' PARL(8), PARL(9), PARL(12), PARL(13) =',4F8.4,/)
 1010 FORMAT(' Lepton energy not allowed to vary in simulation.',/)
 1020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' y in table below calculated assuming max energy.',/)
 1030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, W =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' W-values in array WW:',/,10F7.1,/,11F7.1,/)
 1040 FORMAT(' Max W outside grid, execution stopped ! Wmax, grid-max ='
     &,2F12.1)
 1100 FORMAT(//,6X,'W',7X,'x',7X,'y',8X,'Q**2',3X,'alpha',
     &5X,'cut',2X,'q-event',1X,'qg-event',1X,'qq-event',
     &/,1X,77(1H-),/)
 1200 FORMAT(F7.1,2F8.4,1PG12.3,0PF8.2,F8.4,3F9.4)
 2020 FORMAT(' Lepton energy allowed to vary in simulation, ',/,
     &' W in table below calculated assuming max energy.',/)
 2030 FORMAT(' Grid choice, LST(19) =',I3,5X,'# grid points in x, y =',
     &2I5,/,' x-values in array XX:',/,10F8.5,/,10F8.5,/,11F8.5,
     &    /,' y-values in array WW:',/,10F7.4,/,11F7.4,/)
 2100 FORMAT(//,7X,'x',7X,'y',8X,'Q**2',6X,'W',3X,'alpha',
     &5X,'cut',2X,'q-event',1X,'qg-event',1X,'qq-event',
     &/,1X,77(1H-),/)
 2200 FORMAT(2F8.5,1PG12.3,0PF7.1,F8.2,F8.4,3F9.4)
      END
