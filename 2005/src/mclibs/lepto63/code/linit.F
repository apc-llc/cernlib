
C **********************************************************************


      SUBROUTINE LINIT(LFILE,LEPIN,PLZ,PPZ,INTER)

C...Initialize for an incoming lepton (type LEPIN, momentum pz=PLZ)
C...and target nucleon (momentum pz=PPZ) to interact via INTER.
C...Find maximum of differential cross section, calculate QCD event
C...probabilities or read them from logical file LFILE (if >0).
C...Numerical integration to obtain total cross-section.

      COMMON /LINTRL/ PSAVE(3,4,5),KSAVE(4),XMIN,XMAX,YMIN,YMAX,
     &Q2MIN,Q2MAX,W2MIN,W2MAX,ILEP,INU,IG,IZ
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON /LGRID/ NXX,NWW,XX(31),WW(21),PQG(31,21,3),PQQB(31,21,2),
     &QGMAX(31,21,3),QQBMAX(31,21,2),YCUT(31,21),XTOT(31,21),NP
      COMMON /LOPTIM/ OPTX(4),OPTY(4),OPTQ2(4),OPTW2(4),COMFAC
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON /LBOOST/ DBETA(2,3),STHETA(2),SPHI(2),PB(5),PHIR
      COMMON /LMINUI/ XKIN(4),UKIN(4),WKIN(4),AIN(4),BIN(4),
     &MAXFIN,RELUP,RELERR,RELER2,FCNMAX
      COMMON /LMINUC/ NAMKIN(4),NAM(30)
      COMMON /LPFLAG/ LST3
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARIPY(200)
      CHARACTER*10 NAMKIN,NAM
      DIMENSION LSTW(40),PARLW(30)
      DOUBLE PRECISION DTHETA,DPHI,DBETA
      DATA PI/3.1415927/,NCALL/0/

      NCALL=NCALL+1
      LST3=LST(3)
      IF(LST(18).GE.1) THEN
C...W, Z masses from theta-Weinberg, Fermi constant GF and rad. corr.
        PMAS(24,1)=SQRT(PI*PARL(16)/(SQRT(2.)*PARL(17)*PARL(5)*
     &  (1.-PARL(18))))
        PMAS(23,1)=PMAS(24,1)/SQRT(1.-PARL(5))
      ENDIF
C...Couplings between Z0 and left/right-handed leptons and quarks.
      ZL(1,1)=-.5+PARL(5)
      ZL(1,2)=PARL(5)
      ZL(2,1)=ZL(1,2)
      ZL(2,2)=ZL(1,1)
      ZL(1,3)=0.5
      ZL(2,3)=0.
      ZL(1,4)=0.
      ZL(2,4)=0.5
      DO 10 IFL=1,8
      ZQ(1,IFL)=SIGN(0.5,QC(IFL))-QC(IFL)*PARL(5)
   10 ZQ(2,IFL)=-QC(IFL)*PARL(5)

C...Set initial state.
      LST(23)=INTER
      KSAVE(1)=LEPIN
      KSAVE(2)=2212
      K(1,1)=21
      K(1,2)=KSAVE(1)
      K(1,3)=0
      K(1,4)=0
      K(1,5)=0
      K(2,1)=21
      K(2,2)=KSAVE(2)
      K(2,3)=0
      K(2,4)=0
      K(2,5)=0
      P(1,1)=0.
      P(1,2)=0.
      P(1,3)=PLZ
      P(1,5)=ULMASS(KSAVE(1))
      P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
      P(2,1)=0.
      P(2,2)=0.
      P(2,3)=PPZ
      P(2,5)=ULMASS(KSAVE(2))
      P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
      N=2
      LST(28)=3
C...Save momentum vectors of incoming particles
      DO 20 I=1,2
      DO 20 J=1,5
   20 PSAVE(3,I,J)=P(I,J)
C...Dot-product of initial particles, cms energy
      PARL(21)=2.*(DBLE(P(1,4))*DBLE(P(2,4))-DBLE(P(1,3))*DBLE(P(2,3)))
      ROOTS=SQRT((DBLE(P(1,4))+DBLE(P(2,4)))**2
     &          -(DBLE(P(1,3))+DBLE(P(2,3)))**2)
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1000)
     &LEPIN,(P(1,J),J=1,3),PARL(1),PARL(2),(P(2,J),J=1,3),INTER,ROOTS
      IF(PLZ*PPZ.GT.0.1) THEN
        WRITE(6,1010)
        STOP
      ENDIF

C...Reduced header for Jetset/Pythia
      MSTU(12)=0
      MSTP(122)=0
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) 
     &WRITE(6,1020) MSTU(181),MSTU(182),MSTP(181),MSTP(182)
C...If JETSET version before 7.402, problem with azimuthal dependence
C...in LUSHOW solved by chosing flat azimuthal dependence.
      IF(MSTU(181).LE.7.AND.MSTU(182).LT.402) THEN
        MSTJ(46)=0
        WRITE(6,1030) MSTJ(46)
      ENDIF
C...Initialize PYTHIA for parton densities.
      IF(LST(15).GT.0) THEN
C...Set switches and parameters for parton densities in PYSTFU.
        MSTP(51)=LST(15)
        MSTP(52)=LST(16)
        MSTP(58)=LST(12)
      ENDIF
      CALL PYINIT('NONE','e-','p',ROOTS)
      PARL(26)=PARP(1)
CAE--     use Lambda from parton densities in initial cascade
      PYPAR(21)=PARP(1)
C...Reset PYTHIA 4.8 parameters from LEPTO parameters.
      IF(MOD(LST(8),10).EQ.3.OR.MOD(LST(8),10).EQ.5) IPY(13)=0
      IF(LST(35).EQ.0.AND.
     &(MOD(LST(8),10).EQ.4.OR.MOD(LST(8),10).EQ.5)) IPY(14)=0
      IPY(8)=LST(12)

      IF(PSAVE(3,1,3).LT.0.) THEN
C...Flip event to have initial lepton along +z axis
        P(1,3)=-P(1,3)
        P(2,3)=-P(2,3)
      ENDIF
C...Boost parameters to cms of incoming particles
      DBETA(1,1)=0.D0
      DBETA(1,2)=0.D0
      DBETA(1,3)=(DBLE(P(1,3))+DBLE(P(2,3)))/(DBLE(P(1,4))+DBLE(P(2,4)))
      SPHI(1)=0.D0
      STHETA(1)=0.D0
      IF(LST(17).NE.0) THEN
C...For varying beam energies, transform to cms, lepton along +z axis.
        CALL LUDBRB(0,0,0.,0.,0.D0,0.D0,-DBETA(1,3))
        SPHI(1)=ULANGL(P(1,1),P(1,2))
        CALL LUDBRB(0,0,0.,-SPHI(1),0.D0,0.D0,0.D0)
        STHETA(1)=ULANGL(P(1,3),P(1,1))
        CALL LUDBRB(0,0,-STHETA(1),0.,0.D0,0.D0,0.D0)
        LST(28)=2
      ENDIF

C...Effective limits on kinematic variables x, y, Q**2, W**2
      PM2=P(2,5)**2
      S=PARL(21)
      XMIN=MAX(CUT(1),0.)
      XMAX=MIN(CUT(2),1.)
      YMIN=MAX(CUT(3),0.)
      YMAX=MIN(CUT(4),1.)
      Q2MIN=MAX(CUT(5),0.)
      Q2MAX=MIN(CUT(6),S)
      W2MIN=MAX(CUT(7),0.)
      W2MAX=MIN(CUT(8),S+PM2)
      UMIN=MAX(CUT(9),0.)
      UMAX=MIN(CUT(10),S/(2.*P(2,5)))
      DO 40 I=1,2
      XMIN=MAX(XMIN,Q2MIN/(S*YMAX),Q2MIN/(2.*P(2,5)*UMAX),
     &1.-(W2MAX-PM2)/MAX(S*YMIN,1.E-22),
     &1.-(W2MAX-PM2)/MAX(2.*P(2,5)*UMIN,1.E-22))
      XMAX=MIN(XMAX,Q2MAX/MAX(S*YMIN,1.E-22),
     &Q2MAX/MAX(2.*P(2,5)*UMIN,1.E-22),
     &1.-(W2MIN-PM2)/(S*YMAX),1.-(W2MIN-PM2)/(2.*P(2,5)*UMAX))
      YMIN=MAX(YMIN,Q2MIN/(S*XMAX),(W2MIN-PM2)/(S*(1.-XMIN)),
     &(W2MIN-PM2+Q2MIN)/S,2.*P(2,5)*UMIN/S)
      YMAX=MIN(YMAX,Q2MAX/MAX(S*XMIN,1.E-22),
     &(W2MAX-PM2)/MAX(S*(1.-XMAX),1.E-22),
     &(W2MAX-PM2+Q2MAX)/S,2.*P(2,5)*UMAX/S)
      Q2MIN=MAX(Q2MIN,S*XMIN*YMIN,S*YMIN-W2MAX+PM2,
     &2.*P(2,5)*UMIN*XMIN,(W2MIN-PM2)*XMIN/(1.-XMIN))
      Q2MAX=MIN(Q2MAX,S*XMAX*YMAX,S*YMAX-W2MIN+PM2,
     &2.*P(2,5)*UMAX*XMAX,(W2MAX-PM2)*XMAX/MAX(1.-XMAX,1.E-22))
      W2MIN=MAX(W2MIN,S*(1.-XMAX)*YMIN+PM2,Q2MIN*(1.-XMAX)/XMAX+PM2,
     &S*YMIN-Q2MAX+PM2,2.*P(2,5)*UMIN*(1.-XMAX)+PM2)
      W2MAX=MIN(W2MAX,S*(1.-XMIN)*YMAX+PM2,
     &Q2MAX*(1.-XMIN)/MAX(XMIN,1.E-22)+PM2,
     &S*YMAX-Q2MIN+PM2,2.*P(2,5)*UMAX*(1.-XMIN)+PM2)
C     UMIN=MAX(UMIN,....)
C     UMAX=MIN(UMAX,....)
   40 CONTINUE
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1)) WRITE(6,1050)
     &CUT,XMIN,XMAX,YMIN,YMAX,Q2MIN,Q2MAX,W2MIN,W2MAX,UMIN,UMAX
      IF(XMAX.LT.XMIN.OR.YMAX.LT.YMIN.OR.Q2MAX.LT.Q2MIN.OR.
     &W2MAX.LT.W2MIN) THEN
        IF(LST(3).GE.1) WRITE(6,1100)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(XMIN.LT.1.E-10.OR.Q2MIN.LT.1.E-01) THEN
        IF(LST(3).GE.1) WRITE(6,1110)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

      PARI(11)=(PARL(1)-PARL(2))/PARL(1)
      KSAVE(4)=LEPIN
      ILEP=1
      IF(LEPIN.LT.0) ILEP=2
      INU=0
      IF(IABS(LEPIN).EQ.12.OR.IABS(LEPIN).EQ.14
     &.OR.IABS(LEPIN).EQ.16) INU=1
      IF(INU.EQ.1) THEN
C...Set full polarisation for incoming neutrino.
        PARL(6)=-1.
        IF(LEPIN.LT.0) PARL(6)=1.
      ENDIF
      IF(LST(23).EQ.1.AND.INU.EQ.0) THEN
C...Electromagnetic interaction.
        KSAVE(3)=22
        IG=1
        IZ=0
      ELSEIF(LST(23).EQ.2) THEN
C...Weak charged current, only one helicity state contributes.
        IF(KSAVE(1).LT.0.AND.PARL(6).LT.-0.99
     &  .OR.KSAVE(1).GT.0.AND.PARL(6).GT.0.99) THEN
          IF(LST(3).GE.1) WRITE(6,1150) LEPIN,PARL(6)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
        IF(MOD(IABS(LEPIN),2).EQ.0) THEN
          KSAVE(3)=ISIGN(24,LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)-1,LEPIN)
        ELSE
          KSAVE(3)=ISIGN(24,-LEPIN)
          KSAVE(4)=ISIGN(IABS(LEPIN)+1,LEPIN)
        ENDIF
      ELSEIF(LST(23).EQ.3.OR.(LST(23).EQ.4.AND.INU.EQ.1)) THEN
C...Weak neutral current.
        KSAVE(3)=23
        IG=0
        IZ=1
      ELSEIF(LST(23).EQ.4.AND.INU.EQ.0) THEN
C...Neutral current, electromagnetic and weak with interference.
        KSAVE(3)=23
        IG=1
        IZ=1
      ELSE
        IF(LST(3).GE.1) WRITE(6,1200) INTER,LEPIN
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF

C...Choice of independent variables.
      IF(LST(1).EQ.0) THEN
        LST(31)=1
        IF(INTER.EQ.2.OR.INTER.EQ.3) LST(31)=2
      ELSE
        LST(31)=IABS(LST(1))
      ENDIF
      IF(LST(31).LT.1.OR.LST(31).GT.3) THEN
        IF(LST(3).GE.1) WRITE(6,1210) LST(1),LST(31)
        IF(LST(3).GE.2) THEN
          WRITE(6,1900)
          STOP
        ENDIF
      ENDIF
      IF(LST(1).LT.0) THEN
C...User-defined optimization parameters.
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1220) OPTX,OPTY,OPTQ2,OPTW2
      ELSE
C...Set optimization parameters.
        DO 50 I=1,4
        OPTX(I)=0.
        OPTY(I)=0.
        OPTQ2(I)=0.
   50   OPTW2(I)=0.
        IF(INTER.EQ.1) THEN
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(3)=1.
          OPTW2(3)=1.
        ELSEIF(INTER.EQ.4) THEN
          OPTX(1)=0.1
          OPTX(2)=1.
          OPTY(1)=1.
          OPTQ2(1)=0.5
          OPTQ2(2)=0.5
          OPTQ2(3)=1.
          OPTW2(1)=0.5
          OPTW2(2)=0.5
          OPTW2(3)=1.
        ELSE
          OPTX(1)=1.
          OPTY(1)=1.
          OPTQ2(1)=1.
          OPTW2(1)=1.
        ENDIF
      ENDIF

C...Initialize Monte Carlo estimate of cross section.
      PARL(24)=0.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      PARI(32)=0.
      IF(LST(23).EQ.2) THEN
C...Constant factor GF**2/pi for CC, transformation to picobarn.
        PARI(31)=PARL(17)**2/PI*0.39E+09
      ELSE
C...Constant factor 2*pi*alpha**2 for NC, transformation to picobarn.
        PARI(31)=2.*PI*PARL(16)**2*0.39E+09
      ENDIF
      IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &WRITE(6,1250) (I,LST(I),LST(I+10),PARL(I),PARL(I+10),I=1,10)

C...Set up grid with longitudinal structure function, QCD & target mass;
C...only when photon exchange is included
      LQCD=MOD(LST(11),10)
      LTM=MOD(LST(11)/10,10)
      IF(LST(11).NE.0.AND.(INTER.EQ.1.OR.INTER.EQ.4)) CALL FLTABL

C...Get integrated cross-section.
      PARL(23)=0.
      IF(LST(10).GT.0) CALL LXSECT
      IF(LQCD.EQ.2.OR.LTM.EQ.2) THEN
        WRITE(6,1300)
        IF(LQCD.EQ.2) WRITE(6,1310)
        IF(LTM .EQ.2) WRITE(6,1320)
        WRITE(6,1330)
      ENDIF

      IF(LST(2).EQ.1) THEN
C...Find max value of differential cross section for rejection.
        UKIN(1)=(XMAX+XMIN)/2.
        WKIN(1)=0.8*(XMAX-XMIN)/2.
        AIN(1)=XMIN
        BIN(1)=XMAX
        IF(LST(31).EQ.1) THEN
          UKIN(2)=(Q2MAX+Q2MIN)/2.
          WKIN(2)=0.8*(Q2MAX-Q2MIN)/2.
          AIN(2)=Q2MIN
          BIN(2)=Q2MAX
          NAMKIN(2)='      Q**2'
        ELSEIF(LST(31).EQ.2) THEN
          UKIN(2)=(YMAX+YMIN)/2.
          WKIN(2)=0.8*(YMAX-YMIN)/2.
          AIN(2)=YMIN
          BIN(2)=YMAX
          NAMKIN(2)='         y'
        ELSEIF(LST(31).EQ.3) THEN
          UKIN(2)=(W2MAX+W2MIN)/2.
          WKIN(2)=0.8*(W2MAX-W2MIN)/2.
          AIN(2)=W2MIN
          BIN(2)=W2MAX
          NAMKIN(2)='      W**2'
        ENDIF
C...Maximum obtained by minimizing -(diff. x-section).
        CALL LTIMEX(TI1)
        CALL LMINEW
        CALL LTIMEX(TI2)
        PARI(LST(23))=FCNMAX*1.1
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1400) PARI(LST(23)),TI2-TI1
      ENDIF

      IF(LFILE.GT.0.AND.LST(19).GE.0) THEN
C...Read QCD weights from file.
        READ(LFILE) LSTW,PARLW,NXX,NWW,NP,XX,WW
        IPMAX=2
        IF(LSTW(17).NE.0) IPMAX=3
        READ(LFILE) (((PQG(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((PQQB(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,NP),
     &  (((QGMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,IPMAX),
     &  (((QQBMAX(IX,IW,IP),IX=1,NXX),IW=1,NWW),IP=1,MIN(2,IPMAX)),
     &  YCUT
        IF(NP.NE.1) READ(LFILE) XTOT
        CLOSE(LFILE)
C...Reset parameters for matrix element integration.
        PARL(8)=PARLW(8)
        PARL(9)=PARLW(9)
        PARL(11)=PARLW(11)
        PARL(12)=PARLW(12)
        PARL(13)=PARLW(13)
C...Check current parameter values against those used when
C...calculating weights.
        IF(LST(12).NE.LSTW(12).OR.LST(13).NE.LSTW(13)
     &  .OR.LST(15).NE.LSTW(15).OR.LST(16).NE.LSTW(16)
     &  .OR.LST(17).NE.LSTW(17).OR.LST(23).NE.LSTW(23)
     &  .OR.ABS(PARL(1)-PARLW(1)).GT.0.1.OR.ABS(PARL(2)-PARLW(2)).GT.0.1
     &  .OR.ABS(PARL(5)-PARLW(5)).GT.0.01
     &  .OR.ABS(PARL(6)-PARLW(6)).GT.0.1) THEN
         IF(LST(3).GE.1)
     &    WRITE(6,1500) LST(12),LSTW(12),LST(13),LSTW(13),LST(15),
     &    LSTW(15),LST(16),LSTW(16),LST(17),LSTW(17),LST(23),LSTW(23),
     &    PARL(1),PARLW(1),PARL(2),PARLW(2),PARL(5),PARLW(5),PARL(6),
     &    PARLW(6)
          IF(LST(3).GE.2) THEN
            WRITE(6,1900)
            STOP
          ENDIF
        ENDIF
      ELSEIF((LST(19).GE.0.OR.LST(19).EQ.-10).AND.
     &(LST(8).EQ.1.OR.LST(8)/10.EQ.1.OR.MOD(LST(8),10).EQ.9)) THEN
C...Calculate weights if 1st order QCD from grid is requested.
        CALL LTIMEX(TI1)
        CALL LWEITS(LFILE)
        CALL LTIMEX(TI2)
        IF(LST(3).GE.4.OR.(LST(3).EQ.3.AND.NCALL.EQ.1))
     &  WRITE(6,1510) TI2-TI1
      ENDIF

C...Reset counters to zero for Monte Carlo estimate of cross section.
      PARI(27)=0.
      PARI(28)=0.
      PARI(29)=0.
      PARI(30)=0.
      LST(32)=0
      RETURN

 1000 FORMAT(' ',//,5X,
     &'A MONTE CARLO GENERATOR FOR DEEP INELASTIC LEPTON-'
     &,'NUCLEON SCATTERING',/,5X,68('='),//,
     &25X,'LEPTO version 6.5.1, October 31, 1996',//,
C    &25X,'PRELIMINARY VERSION, DO NOT CIRCULATE',//,
     &' Lepton: type =',I3,5X,'momentum (px,py,pz) =',3F8.1,
     &' GeV',//,' Target: A, Z =',2F3.0,2X,
     &'momentum (px,py,pz) =',3F8.1,' GeV',//,
     &' Interaction :',I3,14X,' CMS energy =',1PG12.4,' GeV',/)
 1010 FORMAT(' Warning: lepton and nucleon momenta in same direction',
     &' not allowed.',/,10X,'Execution stopped.')
 1020 FORMAT(/,' JETSET version ',I3,'.',I3,' is used.',/,
     &' Parton densities in PYTHIA version ',I3,'.',I3,' are used.',/)
 1030   FORMAT(' Warning (LINIT): JETSET version before 7.402, MSTJ(46)'
     &  ,' set to',I4,/,18X,'to avoid mismatch LEPTO<-->LUSHOW.',/)
 1050 FORMAT(/,' User applied cuts (+ phase space) : ',1P,
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4,
     &/,37X,G12.4,' <  E''   < ',G12.4,
     &/,37X,G12.4,' < theta < ',G12.4,/,
     &/,       ' Effective ranges (from above cuts): ',
     &      G12.4,' <   x   < ',G12.4,
     &/,37X,G12.4,' <   y   < ',G12.4,
     &/,37X,G12.4,' < Q**2  < ',G12.4,
     &/,37X,G12.4,' < W**2  < ',G12.4,
     &/,37X,G12.4,' <  nu   < ',G12.4)
 1100 FORMAT(' Warning: effective upper limit of kinematical ',
     &'variable(s) smaller than corresponding lower limit.')
 1110 FORMAT(' Warning: lower limit in x and/or Q2 too small for ',
     &'DIS formalism.')
 1150 FORMAT(' Warning: weak charged current cross section zero for ',
     &'specified lepton helicity; LEPIN, PARL(6) =',I3,F5.2)
 1200 FORMAT(' Warning: unrecognized interaction in LINIT call: ',
     &'INTER = ',I5,'  for lepton LEPIN =',I5)
 1210 FORMAT(' Warning: unallowed value of LST(1) =',I3,
     &' and/or LST(31) =',I3)
 1220 FORMAT(/,' User-defined optimization parameters:',
     &/,5X,'OPTX(1...4)  =',4G11.3,/,5X,'OPTY(1...4)  =',4G11.3,
     &/,5X,'OPYQ2(1...4) =',4G11.3,/,5X,'OPTW2(1...4) =',4G11.3,/)
 1250 FORMAT(/,' Parameter values:',//,9X,'I',4X,'LST(I)',1X,
     &'LST(I+10)',8X,'PARL(I)',5X,'PARL(I+10)',1P,
     &/,5X,55('-'),10(/,3I10,2G15.4),/)
 1300 FORMAT(' Warning: cross section, PARL(23), excludes FL (see ',
     &'LST(11)) from:')
 1310 FORMAT(10X,'QCD, since evaluated event by event for LQCD=2')
 1320 FORMAT(10X,'TM , since evaluated event by event for LTM =2')
 1330 FORMAT(' Cross section in PARL(24) includes these contributions.')
 1400 FORMAT(' Max of differential cross section (for weighting) =',
     &E12.4,/,' obtained in ',F7.2,' seconds.',/)
 1500 FORMAT(//,' Warning: current parameter values do not match ',
     &'with those used when calculating QCD weights.',//,15X,
     &'current value     value for weights',/,
     &/,'     LST(12)   ',I12,10X,I12,
     &/,'     LST(13)   ',I12,10X,I12,
     &/,'     LST(15)   ',I12,10X,I12,
     &/,'     LST(16)   ',I12,10X,I12,
     &/,'     LST(17)   ',I12,10X,I12,
     &/,'     LST(23)   ',I12,10X,I12,
     &/,'     PARL(1)   ',E12.4,10X,E12.4,
     &/,'     PARL(2)   ',E12.4,10X,E12.4,
     &/,'     PARL(5)   ',E12.4,10X,E12.4,
     &/,'     PARL(6)   ',E12.4,10X,E12.4)
 1510 FORMAT(/,' Time for calculating QCD weights =',F5.1,' seconds',/)
 1900 FORMAT(' Execution stopped ',/)
      END
