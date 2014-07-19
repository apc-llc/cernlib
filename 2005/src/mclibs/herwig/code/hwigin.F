CDECK  ID>, HWIGIN.
*CMZ :-        -14/07/92  19.49.55  by  Mike Seymour
*-- Author :    Bryan Webber
C----------------------------------------------------------------------
      SUBROUTINE HWIGIN
C-----------------------------------------------------------------------
C     SETS INPUT PARAMETERS
C----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION FAC,ANGLE
      INTEGER I,J,N,L
      CHARACTER*28 TITLE
      DATA TITLE/'HERWIG 5.9    22nd July 1996'/
      WRITE (6,10) TITLE
  10  FORMAT(//10X,A28//,
     &         10X,'Please reference: G. Marchesini, B.R. Webber,',/,
     &         10X,'G.Abbiendi, I.G.Knowles, M.H.Seymour & L.Stanco',/,
     &         10X,'Computer Physics Communications 67 (1992) 465')
C---PRINT OPTIONS:
C
C     IPRINT=0     NO PRINTOUT
C            1     PRINT SELECTED INPUT PARAMETERS
C            2     1 + TABLE OF PARTICLE CODES AND PROPERTIES
C            3     2 + TABLES OF SUDAKOV FORM FACTORS
C----------------------------------------------------------------------
C     PRNDEC=.TRUE.  to use decimal for track numbers in event listing
C            .FALSE. to use hexadecimal
C----------------------------------------------------------------------
      IPRINT=1
      PRNDEC=(NMXHEP.LE.9999)
C Print out vertex information
      PRVTX=.TRUE.
C---UNIT FOR READING SUDAKOV FORM FACTORS (IF ZERO THEN COMPUTE THEM)
      LRSUD=0
C---UNIT FOR WRITING SUDAKOV FORM FACTORS (IF ZERO THEN NOT WRITTEN)
      LWSUD=77
C---UNIT FOR WRITING EVENT DATA IN HWANAL (IF ZERO THEN NOT WRITTEN)
      LWEVT=0
C---SEEDS FOR RANDOM NUMBER GENERATOR (CALLED HWRGEN)
      NRN(1)= 17673
      NRN(2)= 63565
C---AZIMUTHAL CORRELATIONS?
C   THESE INCLUDE SOFT GLUON (INSIDE CONE)
      AZSOFT=.TRUE.
C   AND NEAREST-NEIGHBOUR SPIN CORRELATIONS
      AZSPIN=.TRUE.
C---MATCH HARD EMISSION IN E+E- AND DIS TO MATRIX-ELEMENT
      HARDME=.TRUE.
C---MATCH SOFT EMISSION IN E+E- AND DIS TO MATRIX-ELEMENT
      SOFTME=.TRUE.
C Electromagnetic fine structure constant: Thomson limit
      ALPHEM=.0072993
C---QCD LAMBDA: CORRESPONDS TO 5-FLAVOUR LAMBDA-MS-BAR AT LARGE X ONLY
      QCDLAM=0.18
C---NUMBER OF COLOURS
      NCOLO=3
C---NUMBER OF FLAVOURS
      NFLAV=6
C---QUARK, GLUON AND PHOTON VIRTUAL MASS CUTOFFS IN
C   PARTON SHOWER (ADDED TO MASSES GIVEN BELOW)
      VQCUT=0.48
      VGCUT=0.10
      VPCUT=0.40
      ALPFAC=1
C---D,U,S,C,B,T QUARK AND GLUON MASSES (IN THAT ORDER)
      RMASS(1)=0.32
      RMASS(2)=0.32
      RMASS(3)=0.5
      RMASS(4)=1.55
      RMASS(5)=4.95
      RMASS(6)=170.
      RMASS(13)=0.75
C---W+/- AND Z0 MASSES
      RMASS(198)=80.4
      RMASS(199)=80.4
      RMASS(200)=91.2
C---HIGGS BOSON MASS
      RMASS(201)=150.
C---WIDTHS OF W, Z, HIGGS
      GAMW=2.0
      GAMZ=2.5
      GAMH=0.02
C Include additional neutral, massive vector boson (Z')
      ZPRIME=.FALSE.
C Z' mass and width
      RMASS(202)=500.
      GAMZP=5.
C Lepton (EPOLN) and anti-lepton (PPOLN) beam polarisations used in:
C e+e- --> ffbar/qqbar g; and l/lbar N DIS.
C Cpts. 1,2 Transverse polarisation; cpt. 3 longitudinal polarisation.
C Note require POLN(1)**2+POLN(2)**2+POLN(3)**2 < 1.
      DO 20 I=1,3
      EPOLN(I)=0.
  20  PPOLN(I)=0.
C-----------------------------------------------------------------------
C     Specify couplings of weak vector bosons to fermions:
C
C     electric current:      QFCH(I)*e*G_mu       (electric charge, e>0)
C     weak neutral current: [VFCH(I,J).1+AFCH(I,J).G_5]*e*G_mu
C     weak charged current: SQRT(VCKM(K,L)/2.)*g*(1+G_5)*G_mu
C
C     I= 1- 6: d,u,s,c,b,t (quarks)
C      =11-16: e,nu_e,mu,nu_mu,tau,nu_tau (leptons) (`I=IDHW-110')
C     J=1 for minimal SM:
C      =2 for Z' couplings (ZPRIME=.TRUE.)
C     K=1,2,3 for u,c,t;    L=1,2,3 for d,s,b
C-----------------------------------------------------------------------
C Minimal standard model neutral vector boson couplings
C VFCH(I,1)=(T3/2-Q*S^2_W)/(C_W*S_W);  AFCH(I,1)=T3/(2*C_W*S_W)
C sin**2 Weinberg angle (PDG '94)
      SWEIN=.2319
      FAC=1./SQRT(SWEIN*(1.-SWEIN))
      DO 30 I=1,3
C Down-type quarks
      J=2*I-1
      QFCH(J)=-1./3.
      VFCH(J,1)=(-0.25+SWEIN/3.)*FAC
      AFCH(J,1)= -0.25*FAC
C Up-type quarks
      J=2*I
      QFCH(J)=+2./3.
      VFCH(J,1)=(+0.25-2.*SWEIN/3.)*FAC
      AFCH(J,1)= +0.25*FAC
C Charged leptons
      J=2*I+9
      QFCH(J)=-1.
      VFCH(J,1)=(-0.25+SWEIN)*FAC
      AFCH(J,1)= -0.25*FAC
C Neutrinos
      J=2*I+10
      QFCH(J)=0.
      VFCH(J,1)=+0.25*FAC
      AFCH(J,1)=+0.25*FAC
  30  CONTINUE
C Additional Z' couplings (To be set by the user)
      IF (.NOT.ZPRIME) THEN
         DO 40 I=1,6
         AFCH(I,2)=0.
         AFCH(10+I,2)=0.
         VFCH(I,2)=0.
         VFCH(10+I,2)=0.
  40     CONTINUE
      ENDIF
C Cabibbo-Kobayashi-Maskawa matrix elements squared (PDG '92):
C sin**2 of Cabibbo angle
      SCABI=.0488
C u ---> d,s,b
      VCKM(1,1)=1.-SCABI
      VCKM(1,2)=SCABI
      VCKM(1,3)=0.0
C c ---> d,s,b
      VCKM(2,1)=SCABI
      VCKM(2,2)=1.-SCABI-.002
      VCKM(2,3)=0.002
C t ---> d,b,s
      VCKM(3,1)=0.0
      VCKM(3,2)=0.002
      VCKM(3,3)=0.998
C---GAUGE BOSON DECAYS
      DO 50 I=1,12
      BRHIG(I)=1.D0/12
      ENHANC(I)=1.D0
 50   IF (I.LE.MODMAX) MODBOS(I)=0
C
C THE iTH GAUGE BOSON DECAY PER EVENT IS CONTROLLED BY MODBOS AS FOLLOWS
C         MODBOS(i)     W DECAY        Z DECAY
C             0           all            all
C             1          qqbar          qqbar
C             2           enu            e+e-
C             3           munu          mu+mu-
C             4          taunu         tau+tau-
C             5        enu & munu      ee & mumu
C             6           all            nunu
C             7           all           bbbar
C            >7           all            all
C BOSON PAIRS (eg FROM HIGGS DECAY) ARE CHOSEN FROM MODBOS(i),MODBOS(i+1
C
C---CONTROL OF LARGE EMH BEHAVIOUR (SEE HWHIGM FOR DETAILS)
      IOPHIG=3
      GAMMAX=10.
C Specicify approximation used in HWHIGA
      IAPHIG=1
C---MASSES OF HYPOTHETICAL NEW QUARKS GO
C   INTO 209-214 (ANTIQUARKS IN 215-220)
C   ID = 209,210 ARE B',T' WITH DECAYS T'->B'->C
C        211,212 ARE B',T' WITH DECAYS T'->B'->T
C        215-218 ARE THEIR ANTIQUARKS
      RMASS(209)=200.
      RMASS(215)=200.
C---MAXIMUM CLUSTER MASS PARAMETERS
C   N.B. LIMIT FOR Q1-Q2BAR CLUSTER MASS
C   IS (CLMAX**CLPOW + (QM1+QM2)**CLPOW)**(1/CLPOW)
      CLMAX=3.35
      CLPOW=2.0
C---MASS SPECTRUM OF PRODUCTS IN CLUSTER
C   SPLITTING ABOVE CLMAX - FLAT IN M**PSPLT
      PSPLT=1.0
C---KINEMATIC TREATMENT OF CLUSTER DECAY
C   0=ISOTROPIC, 1=REMEMBER DIRECTION OF PERTURBATIVELY PRODUCED QUARKS
      CLDIR=1
C   IF CLDIR=1, DO GAUSSIAN SMEARING OF DIRECTION:
C   ACTUALLY EXPONENTIAL IN 1-COS(THETA) WITH MEAN CLSMR
      CLSMR=0.0
C---OPTION FOR TREATMENT OF REMNANT CLUSTERS:
C   0=BOTH CHILDREN ARE SOFT, (EQUIVALENT TO PREVIOUS VERSIONS)
C   1=REMNANT CHILD IS SOFT, BUT PERTURBATIVE CHILD IS NORMAL
      IOPREM=1
C---TREATMENT OF LOWER LIMIT FOR SPACELIKE EVOLUTION
C   0=EVOLUTION STOPS AT QSPAC, BUT STRUCT FUNS CAN GET CALLED AT
C   SMALLER SCALES IN FORCED EMISSION (EQUIVALENT TO V5.7 AND EARLIER)
C   1=EVOLUTION STOPS AT QSPAC, STRUCTURE FUNCTIONS FREEZE AT QSPAC
C   2=EVOLUTION CONTINUES TO INFRARED CUT, BUT S.F.S FREEZE AT QSPAC
      ISPAC=0
C---LOWER LIMIT FOR SPACELIKE EVOLUTION
      QSPAC=2.5
C---SWITCH OFF SPACE-LIKE SHOWERS
      NOSPAC=.FALSE.
C---INTRINSIC PT OF SPACELIKE PARTONS (RMS)
      PTRMS=0.0
C---MASS PARAMETER IN REMNANT FRAGMENTATION
      BTCLM=1.0
C---STRUCTURE FUNCTION SET:
C   SET MODPDF(I)=MODE AND AUTPDF='AUTHOR GROUP' TO USE CERN LIBRARY
C   PDFLIB PACKAGE FOR STRUCTURE FUNCTIONS IN BEAM I
      MODPDF(1)=-1
      MODPDF(2)=-1
      AUTPDF(1)='MRS'
      AUTPDF(2)='MRS'
C   OR SET MODPDF(I)=-1 TO USE BUILT-IN STRUCTURE FUNCTION SET:
C   1,2 FOR DUKE+OWENS SETS 1,2 (SOFT/HARD GLUE)
C   3,4 FOR EICHTEN+AL SETS 1,2 (NUCLEONS ONLY)
C    5  FOR OWENS      SET  1.1 (SOFT GLUE ONLY)
      NSTRU=5
C   PARAMETER FOR B CLUSTER DECAY TO 1 HADRON. IF MCL IS CLUSTER MASS
C   AND MTH IS THRESHOLD FOR 2-HADRON DECAY, THEN PROBABILITY IS
C   1 IF MCL<MTH, 0 IF MCL>(1+B1LIM)*MTH, WITH LINEAR INTERPOLATION,
      B1LIM=0.0
C---B DECAY PACKAGE ('HERW'=>HERWIG, 'EURO'=>EURODEC, 'CLEO'=>CLEO)
      BDECAY='HERW'
C---HARD SUBPROCESS SCALE TO BE USED IN BOSON-GLUON FUSION
C   IF (BGSHAT) THEN SCALE=SHAT
C   ELSE SCALE=2.*SHAT*THAT*UHAT/(SHAT**2+THAT**2+UHAT**2)
      BGSHAT=.FALSE.
C---RECONSTRUCT DIS EVENTS IN BREIT FRAME
      BREIT=.TRUE.
C---TREAT ALL EVENTS IN THEIR CMF (ELSE USE LAB FRAME)
      USECMF=.TRUE.
C---PROBABILITY OF UNDERLYING SOFT EVENT:
      PRSOF=1.
C---MULTIPLICITY ENHANCEMENT FOR UNDERLYING SOFT EVENT:
C   NCH = NCH(PPBAR AT ENSOF*SQRT(S))
      ENSOF=1.
C   PARAMETERS FOR MUELLER TANG FORMUA: IPROC=2400
C---THE VALUE TO USE FOR FIXED ALPHA_S IN DENOMINATOR
      ASFIXD=0.25
C---OMEGA0=12*LOG(2)*ALPHA_S/PI, BUT NOT NECESSARILY THE SAME ALPHA_S
      OMEGA0=0.3
C---MIN AND MAX JET RAPIDITIES IN QCD 2->2,
C   HEAVY FLAVOUR AND DIRECT PHOTON PROCESSES
      YJMAX=8.
      YJMIN=-YJMAX
C---MIN AND MAX PARTON TRANSVERSE MOMENTUM
C   IN ELEMENTARY 2 -> 2 SUBPROCESSES
      PTMIN=1D1
      PTMAX=1D8
C---UPPER LIMIT ON HARD PROCESS SCALE
      QLIM=1D8
C---MAX PARTON THRUST IN 2->3 HARD PROCESSES
      THMAX=0.9
C---MIN AND MAX DILEPTON INVARIANT MASS IN DRELL-YAN PROCESS
      EMMIN=0D0
      EMMAX=1D8
C---MIN AND MAX ABS(Q**2) IN DEEP INELASTIC LEPTON SCATTERING
      Q2MIN=0D0
      Q2MAX=1D10
C---MIN AND MAX ABS(Q**2) IN WEISZACKER-WILLIAMS APPROXIMATION
      Q2WWMN=0.
      Q2WWMX=4.
C---MIN AND MAX ENERGY FRACTION IN WEISZACKER-WILLIAMS APPROXIMATION
      YWWMIN=0.
      YWWMAX=1.
C---MINIMUM HADRONIC MASS FOR PHOTON-INDUCED PROCESSES (INCLUDING DIS)
      WHMIN=0.
C---IF PHOMAS IS NON-ZERO, PARTON DISTRIBUTION FUNCTIONS FOR OFF-SHELL
C   PHOTONS IS DAMPED, WITH MASS PARAMETER = PHOMAS
      PHOMAS=0.
C---MIN AND MAX FLAVOURS GENERATED BY IPROC=9100,9110,9130
      IFLMIN=1
      IFLMAX=5
C---MAX Z IN J/PSI PHOTO- AND ELECTRO- PRODUCTION
      ZJMAX=0.9
C---MIN AND MAX BJORKEN-Y
      YBMIN=0.
      YBMAX=1.
C---MAX COS(THETA) FOR W'S IN E+E- -> W+W-
      CTMAX=0.9999
C   Minimum virtuality^2 of partons to use in calculating distances
      VMIN2=1.D-4
C   Exageration factor for lifetimes of weakly decaying heavy particles
      EXAG=1.
C   Include colour rearrangement in cluster formation
      CLRECO=.FALSE.
C   Probability for colour rearrangement to occur
      PRECO=1./9.
C   Minimum lifetime for particle to be considered stable
      PLTCUT=1.D-8
C   Incude neutral B-meson mixing
      MIXING=.TRUE.
C   Set B_s and B_d mixing parameters: X=Delta m/Gamma
      XMIX(1)=10.0
      XMIX(2)=0.70
C   Y=Delta Gamma/2*Gamma
      YMIX(1)=0.2
      YMIX(2)=0.0
C   Include a cut on particle decay lengths
      MAXDKL=.FALSE.
C   Set option for decay length cut (see HWDXLM)
      IOPDKL=1
C   Smear the primary interaction vertex: see HWRIPT for details
      PIPSMR=.TRUE.
      DO 60 I=0,NMXRES
C   Veto cluster decays into particle type I
      VTOCDK(I)=.FALSE.
C   Veto unstable particle decays into modes involving particle type I
  60  VTORDK(I)=.FALSE.
C---MINIMUM AND MAXIMUM S-HAT/S RANGE FOR PHOTON ISR
      TMNISR=1D-4
      ZMXISR=1-1D-6
C---COLISR IS .TRUE. TO MAKE ISR PHOTONS COLLINEAR WITH BEAMS
      COLISR=.FALSE.
C A Priori weights for mesons w.r.t. pionic n=1, 0-(+) states:
C old VECWT=REPWT(0,1,0) & TENWT=REPWT(0,2,0)
      DO 70 N=0,4
      DO 70 J=0,4
      DO 70 L=0,3
  70  REPWT(L,J,N)=1.
C and singlet (Lambda-like) and decuplet barons
      SNGWT=1.
      DECWT=1.
C---A PRIORI WEIGHTS FOR D,U,S,C,B,T QUARKS AND DIQUARKS (IN THAT ORDER)
      PWT(1)=1.
      PWT(2)=1.
      PWT(3)=1.
      PWT(4)=1.
      PWT(5)=1.
      PWT(6)=1.
      PWT(7)=1.
C   Octet-scalar mixing angles in degrees (use ANGLE for ideal mixing)
      ANGLE=ATAN(ONE/SQRT(TWO))*180./ACOS(-ONE)
      ETAMIX=-20.
      PHIMIX=36.
      H1MIX=ANGLE
      F0MIX=ANGLE
      F1MIX=ANGLE
      F2MIX=26.
C---PARAMETERS FOR NON-PERTURBATIVE SPLITTING OF GLUONS INTO
C   DIQUARK-ANTIDIQUARK PAIRS:
C   SCALE AT WHICH GLUONS CAN BE SPLIT INTO DIQUARKS
C   (0.0 FOR NO SPLITTING)
      QDIQK=0.0
C   PROBABILITY (PER UNIT LOG SCALE) OF DIQUARK SPLITTING
      PDIQK=5.0
C---PARAMETERS FOR IMPORTANCE SAMPLING
C   ASSUME QCD 2->2 DSIG/DET FALLS LIKE ET**(-PTPOW)
C   WHERE ET=SQRT(MQ**2+PT**2) FOR HEAVY FLAVOURS
      PTPOW=4.
C   ASSUME DRELL-YAN DSIG/DEM FALLS LIKE EM**(-EMPOW)
      EMPOW=4.
C   ASSUME DEEP INELASTIC DSIG/DQ**2 FALLS LIKE (Q**2)**(-Q2POW)
      Q2POW=2.5
C---GENERATE UNWEIGHTED EVENTS (EVWGT=AVWGT)?
      NOWGT=.TRUE.
C---DEFAULT MEAN EVENT WEIGHT
      AVWGT=1.
C---ASSUMED MAXIMUM WEIGHT (ZERO TO RECOMPUTE)
      WGTMAX=0.
C---MAX NO OF (CODE.GE.100) ERRORS
      MAXER=10
C---MAX NO OF EVENTS TO PRINT
      MAXPR=1
C---TIME (SEC) NEEDED TO TERMINATE GRACEFULLY
      TLOUT=5.
C---CURRENT NO OF EVENTS
      NEVHEP=0
C---CURRENT NO OF ENTRIES IN /HEPEVT/
      NHEP=0
C---ISTAT IS STATUS OF EVENT (I.E. STAGE IN PROCESSING)
      ISTAT=0
C---IERROR IS ERROR CODE
      IERROR=0
C---MORE TECHNICAL PARAMETERS - SHOULDN'T NEED ADJUSTMENT
C---PI
      PIFAC=ACOS(-1.D0)
C Speed of light (mm/s)
      CSPEED=2.99792D11
C Cross-section conversion factor (hbar.c/e)**2
      GEV2NB=389380
C---NUMBER OF SHOTS FOR INITIAL MAX WEIGHT SEARCH
      IBSH=2000
C---RANDOM NO. SEEDS FOR INITIAL MAX WEIGHT SEARCH
      IBRN(1)=1246579
      IBRN(2)=8447766
C---NUMBER OF ENTRIES IN LOOKUP TABLES OF SUDAKOV FORM FACTORS
      NQEV=1024
C---MAXIMUM BIN SIZE IN Z FOR SPACELIKE BRANCHING
      ZBINM=0.05
C---MAXIMUM NUMBER OF Z BINS FOR SPACELIKE BRANCHING
      NZBIN=100
C---MAXIMUM NUMBER OF BRANCH REJECTIONS (TO AVOID INFINITE LOOPS)
      NBTRY=200
C---MAXIMUM NUMBER OF TRIES TO GENERATE CLUSTER DECAY
      NCTRY=200
C---MAXIMUM NUMBER OF TRIES TO GENERATE MASS REQUESTED
      NETRY=50
C---MAXIMUM NUMBER OF TRIES TO GENERATE SOFT SUBPROCESS
      NSTRY=200
C---PRECISION FOR GAUSSIAN INTEGRATION
      ACCUR=1.D-6
C---ORDER OF INTERPOLATION IN SUDAKOV TABLES
      INTER=3
C---ORDER TO USE FOR ALPHAS IN SUDAKOV TABLES
      SUDORD=1
  999 END
