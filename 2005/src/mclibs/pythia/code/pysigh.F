 
C***********************************************************************
 
C...PYSIGH
C...Differential matrix elements for all included subprocesses
C...Note that what is coded is (disregarding the COMFAC factor)
C...1) for 2 -> 1 processes: s-hat/pi*d(sigma-hat), where,
C...when d(sigma-hat) is given in the zero-width limit, the delta
C...function in tau is replaced by a (modified) Breit-Wigner:
C...1/pi*s*H_res/((s*tau-m_res^2)^2+H_res^2),
C...where H_res = s-hat/m_res*Gamma_res(s-hat);
C...2) for 2 -> 2 processes: (s-hat)**2/pi*d(sigma-hat)/d(t-hat);
C...i.e., dimensionless quantities
C...3) for 2 -> 3 processes: abs(M)^2, where the total cross-section is
C...Integral abs(M)^2/(2shat') * (prod_(i=1)^3 d^3p_i/((2pi)^3*2E_i)) *
C...(2pi)^4 delta^4(P - sum p_i)
C...COMFAC contains the factor pi/s (or equivalent) and
C...the conversion factor from GeV^-2 to mb
 
      SUBROUTINE PYSIGH(NCHN,SIGS)
 
C...Double precision and integer declarations
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
C...Parameter statement to help give large particle numbers.
      PARAMETER (KSUSY1=1000000,KSUSY2=2000000,KTECHN=3000000,
     &KEXCIT=4000000,KDIMEN=5000000)
C...Commonblocks
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(500),KFPR(500,2),COEF(500,20),ICOL(40,4,2)
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)
      COMMON/PYSSMT/ZMIX(4,4),UMIX(2,2),VMIX(2,2),SMZ(4),SMW(2),
     &SFMIX(16,4),ZMIXI(4,4),UMIXI(2,2),VMIXI(2,2)
      SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYDAT3/,/PYSUBS/,/PYPARS/,
     &/PYINT1/,/PYINT2/,/PYINT3/,/PYINT4/,/PYINT5/,/PYINT7/,
     &/PYMSSM/,/PYSSMT/
C...Local arrays and complex variables
      DIMENSION X(2),XPQ(-25:25),KFAC(2,-40:40),WDTP(0:400),
     &WDTE(0:400,0:5),HGZ(6,3),HL3(3),HR3(3),HL4(3),HR4(3)
      COMPLEX*16 A004,A204,A114,A00U,A20U,A11U
      COMPLEX*16 CIGTOT,CIZTOT,F0ALP,F1ALP,F2ALP,F0BET,F1BET,F2BET,FIF,
     &COULCK,COULCP,COULCD,COULCR,COULCS
      REAL*8 A00L,A11L,A20L,COULXX
      COMPLEX*16 SSMZ,SSMR,SSMO,DETD,F2L,F2R,DARHO,DZRHO,DAOME,DZOME
      COMPLEX*16 DAA,DZZ,DAZ,DWW,DWRHO
      COMPLEX*16 ZTC(6,6),YTC(6,6),DGGS,DGGT,DGGU
      COMPLEX*16 DQQS,DQQT,DQQU,DQTS
      COMPLEX*16 DVVS,DVVT,DVVU
      COMPLEX*16 OLPP,ORPP,OLP,ORP,OL,OR,QLL,QLR
      COMPLEX*16 QRR,QRL,GLIJ,GRIJ,PROPW,PROPZ
      COMPLEX*16 ZMIXC(4,4),UMIXC(2,2),VMIXC(2,2)
      INTEGER INDX(6)
 
C...Reset number of channels and cross-section
      NCHN=0
      SIGS=0D0
 
C...Convert H or A process into equivalent h one
      ISUB=MINT(1)
      ISUBSV=ISUB
      IHIGG=1
      KFHIGG=25
      IF((ISUB.GE.151.AND.ISUB.LE.160).OR.(ISUB.GE.171.AND.
     &ISUB.LE.190)) THEN
        IHIGG=2
        IF(MOD(ISUB-1,10).GE.5) IHIGG=3
        KFHIGG=33+IHIGG
        IF(ISUB.EQ.151.OR.ISUB.EQ.156) ISUB=3
        IF(ISUB.EQ.152.OR.ISUB.EQ.157) ISUB=102
        IF(ISUB.EQ.153.OR.ISUB.EQ.158) ISUB=103
        IF(ISUB.EQ.171.OR.ISUB.EQ.176) ISUB=24
        IF(ISUB.EQ.172.OR.ISUB.EQ.177) ISUB=26
        IF(ISUB.EQ.173.OR.ISUB.EQ.178) ISUB=123
        IF(ISUB.EQ.174.OR.ISUB.EQ.179) ISUB=124
        IF(ISUB.EQ.181.OR.ISUB.EQ.186) ISUB=121
        IF(ISUB.EQ.182.OR.ISUB.EQ.187) ISUB=122
        IF(ISUB.EQ.183.OR.ISUB.EQ.188) ISUB=111
        IF(ISUB.EQ.184.OR.ISUB.EQ.189) ISUB=112
        IF(ISUB.EQ.185.OR.ISUB.EQ.190) ISUB=113
      ENDIF
 
CMRENNA++
C...Convert almost equivalent SUSY processes into each other
C...Extract differences in flavours and couplings
      IF(ISUB.GE.200.AND.ISUB.LE.301) THEN
 
C...Sleptons and sneutrinos
        IF(ISUB.EQ.201.OR.ISUB.EQ.204.OR.ISUB.EQ.207) THEN
          KFID=MOD(KFPR(ISUB,1),KSUSY1)
          ISUB=201
          ILR=0
        ELSEIF(ISUB.EQ.202.OR.ISUB.EQ.205.OR.ISUB.EQ.208) THEN
          KFID=MOD(KFPR(ISUB,1),KSUSY1)
          ISUB=201
          ILR=1
        ELSEIF(ISUB.EQ.203.OR.ISUB.EQ.206.OR.ISUB.EQ.209) THEN
          KFID=MOD(KFPR(ISUB,1),KSUSY1)
          ISUB=203
        ELSEIF(ISUB.GE.210.AND.ISUB.LE.212) THEN
          IF(ISUB.EQ.210) THEN
            RKF=2.0D0
          ELSEIF(ISUB.EQ.211) THEN
            RKF=SFMIX(15,1)**2
          ELSEIF(ISUB.EQ.212) THEN
            RKF=SFMIX(15,2)**2
          ENDIF
          ISUB=210
        ELSEIF(ISUB.EQ.213.OR.ISUB.EQ.214) THEN
          IF(ISUB.EQ.213) THEN
            KFID=MOD(KFPR(ISUB,1),KSUSY1)
            RKF=2.0D0
          ELSEIF(ISUB.EQ.214) THEN
            KFID=16
            RKF=1.0D0
          ENDIF
          ISUB=213
 
C...Neutralinos
        ELSEIF(ISUB.GE.216.AND.ISUB.LE.225) THEN
          IF(ISUB.EQ.216) THEN
            IZID1=1
            IZID2=1
          ELSEIF(ISUB.EQ.217) THEN
            IZID1=2
            IZID2=2
          ELSEIF(ISUB.EQ.218) THEN
            IZID1=3
            IZID2=3
          ELSEIF(ISUB.EQ.219) THEN
            IZID1=4
            IZID2=4
          ELSEIF(ISUB.EQ.220) THEN
            IZID1=1
            IZID2=2
          ELSEIF(ISUB.EQ.221) THEN
            IZID1=1
            IZID2=3
          ELSEIF(ISUB.EQ.222) THEN
            IZID1=1
            IZID2=4
          ELSEIF(ISUB.EQ.223) THEN
            IZID1=2
            IZID2=3
          ELSEIF(ISUB.EQ.224) THEN
            IZID1=2
            IZID2=4
          ELSEIF(ISUB.EQ.225) THEN
            IZID1=3
            IZID2=4
          ENDIF
          ISUB=216
 
C...Charginos
        ELSEIF(ISUB.GE.226.AND.ISUB.LE.228) THEN
          IF(ISUB.EQ.226) THEN
            IZID1=1
            IZID2=1
          ELSEIF(ISUB.EQ.227) THEN
            IZID1=2
            IZID2=2
          ELSEIF(ISUB.EQ.228) THEN
            IZID1=1
            IZID2=2
          ENDIF
          ISUB=226
 
C...Neutralino + chargino
        ELSEIF(ISUB.GE.229.AND.ISUB.LE.236) THEN
          IF(ISUB.EQ.229) THEN
            IZID1=1
            IZID2=1
          ELSEIF(ISUB.EQ.230) THEN
            IZID1=1
            IZID2=2
          ELSEIF(ISUB.EQ.231) THEN
            IZID1=1
            IZID2=3
          ELSEIF(ISUB.EQ.232) THEN
            IZID1=1
            IZID2=4
          ELSEIF(ISUB.EQ.233) THEN
            IZID1=2
            IZID2=1
          ELSEIF(ISUB.EQ.234) THEN
            IZID1=2
            IZID2=2
          ELSEIF(ISUB.EQ.235) THEN
            IZID1=2
            IZID2=3
          ELSEIF(ISUB.EQ.236) THEN
            IZID1=2
            IZID2=4
          ENDIF
          ISUB=229
 
C...Gluino + neutralino
        ELSEIF(ISUB.GE.237.AND.ISUB.LE.240) THEN
          IF(ISUB.EQ.237) THEN
            IZID=1
          ELSEIF(ISUB.EQ.238) THEN
            IZID=2
          ELSEIF(ISUB.EQ.239) THEN
            IZID=3
          ELSEIF(ISUB.EQ.240) THEN
            IZID=4
          ENDIF
          ISUB=237
 
C...Gluino + chargino
        ELSEIF(ISUB.GE.241.AND.ISUB.LE.242) THEN
          IF(ISUB.EQ.241) THEN
            IZID=1
          ELSEIF(ISUB.EQ.242) THEN
            IZID=2
          ENDIF
          ISUB=241
 
C...Squark + neutralino
        ELSEIF(ISUB.GE.246.AND.ISUB.LE.253) THEN
          ILR=0
          IF(MOD(ISUB,2).NE.0) ILR=1
          IF(ISUB.LE.247) THEN
            IZID=1
          ELSEIF(ISUB.LE.249) THEN
            IZID=2
          ELSEIF(ISUB.LE.251) THEN
            IZID=3
          ELSEIF(ISUB.LE.253) THEN
            IZID=4
          ENDIF
          ISUB=246
          RKF=5D0
 
C...Squark + chargino
        ELSEIF(ISUB.GE.254.AND.ISUB.LE.257) THEN
          IF(ISUB.LE.255) THEN
            IZID=1
          ELSEIF(ISUB.LE.257) THEN
            IZID=2
          ENDIF
          IF(MOD(ISUB,2).EQ.0) THEN
            ILR=0
          ELSE
            ILR=1
          ENDIF
          ISUB=254
          RKF=5D0
 
C...Squark + gluino
        ELSEIF(ISUB.EQ.258.OR.ISUB.EQ.259) THEN
          ISUB=258
          RKF=4D0
 
C...Stops
        ELSEIF(ISUB.EQ.261.OR.ISUB.EQ.262) THEN
          ILR=0
          IF(ISUB.EQ.262) ILR=1
          ISUB=261
        ELSEIF(ISUB.EQ.265) THEN
          ISUB=264
 
C...Squarks
        ELSEIF(ISUB.GE.271.AND.ISUB.LE.280) THEN
          ILR=0
          IF(ISUB.LE.273) THEN
            IF(ISUB.EQ.273) ILR=1
            ISUB=271
            RKF=16D0
          ELSEIF(ISUB.LE.276) THEN
            IF(ISUB.EQ.276) ILR=1
            ISUB=274
            RKF=16D0
          ELSEIF(ISUB.LE.278) THEN
            IF(ISUB.EQ.278) ILR=1
            ISUB=277
            RKF=4D0
          ELSE
            IF(ISUB.EQ.280) ILR=1
            ISUB=279
            RKF=4D0
          ENDIF
C...Sbottoms
        ELSEIF(ISUB.GE.281.AND.ISUB.LE.296) THEN
          ILR=0
          IF(ISUB.LE.283) THEN
            IF(ISUB.EQ.283) ILR=1
            ISUB=271
            RKF=4D0
          ELSEIF(ISUB.LE.286) THEN
            IF(ISUB.EQ.286) ILR=1
            ISUB=274
            RKF=4D0
          ELSEIF(ISUB.LE.288) THEN
            IF(ISUB.EQ.288) ILR=1
            ISUB=277
            RKF=1D0
          ELSEIF(ISUB.LE.290) THEN
            IF(ISUB.EQ.290) ILR=1
            ISUB=279
            RKF=1D0
          ELSEIF(ISUB.LE.293) THEN
            IF(ISUB.EQ.293) ILR=1
            ISUB=271
            RKF=1D0
          ELSEIF(ISUB.EQ.296) THEN
            ILR=1
            ISUB=274
            RKF=1D0
C...Squark + gluino
          ELSEIF(ISUB.EQ.294.OR.ISUB.EQ.295) THEN
            ISUB=258
            RKF=1D0
          ENDIF
C...H+/- + H0
        ELSEIF(ISUB.EQ.297.OR.ISUB.EQ.298) THEN
          IF(ISUB.EQ.297) THEN
            RKF=.5D0*PARU(195)**2
          ELSEIF(ISUB.EQ.298) THEN
            RKF=.5D0*(1D0-PARU(195)**2)
          ENDIF
          ISUB=210
C...A0 + H0
        ELSEIF(ISUB.EQ.299.OR.ISUB.EQ.300) THEN
          IF(ISUB.EQ.299) THEN
            RKF=PARU(186)**2
            KFID=25
          ELSEIF(ISUB.EQ.300) THEN
            RKF=PARU(187)**2
            KFID=35
          ENDIF
          ISUB=213
C...H+ + H-
        ELSEIF(ISUB.EQ.301) THEN
          KFID=37
          RKF=1D0
          ISUB=201
        ENDIF
 
C...Convert almost equivalent technicolor processes into
C...a few basic processes, and set distinguishing parameters.
      ELSEIF(ISUB.GE.361.AND.ISUB.LE.379) THEN
        SQTV=PARP(137)**2
        SQTA=PARP(138)**2
        SN2W=2D0*SQRT(PARU(102)*(1D0-PARU(102)))
        CS2W=1D0-2D0*PARU(102)
        TANW=SQRT(PARU(102)/(1D0-PARU(102)))
        CT2W=CS2W/SN2W
        CSXI=COS(ASIN(PARP(141)))
        CSXIP=COS(ASIN(PARP(139)))
        QUPD=2D0*PARP(143)-1D0
        Q2UD=PARP(143)**2+(PARP(143)-1D0)**2
C... rho_tc0 -> W_L W_L
        IF(ISUB.EQ.361) THEN
           KFA=24
           KFB=24
           CAB2=PARP(141)**4
C... rho_tc0 -> W_L pi_tc-
        ELSEIF(ISUB.EQ.362) THEN
           KFA=24
           KFB=KTECHN+211
           ISUB=361
           CAB2=PARP(141)**2*(1D0-PARP(141)**2)
C... pi_tc pi_tc
        ELSEIF(ISUB.EQ.363) THEN
           KFA=KTECHN+211
           KFB=KTECHN+211
           ISUB=361
           CAB2=(1D0-PARP(141)**2)**2
C... rho_tc0/omega_tc -> gamma pi_tc
        ELSEIF(ISUB.EQ.364) THEN
           KFA=22
           KFB=KTECHN+111
           VOGP=CSXI/PARP(137)
C..........!!!
           VRGP=VOGP*QUPD
           AOGP=0D0
           ARGP=0D0
           VAGP=2D0*QUPD*CSXI
           VZGP=QUPD*CSXI*(1D0-4D0*PARU(102))/SN2W
C... gamma pi_tc'
        ELSEIF(ISUB.EQ.365) THEN
           KFA=22
           KFB=KTECHN+221
           ISUB=364
           VRGP=CSXIP/PARP(137)
C..........!!!!
           VOGP=VRGP*QUPD
           AOGP=0D0
           ARGP=0D0
           VAGP=2D0*Q2UD*CSXIP
           VZGP=CSXIP/SN2W*(1D0-4D0*PARU(102)*Q2UD)
C... Z pi_tc
        ELSEIF(ISUB.EQ.366) THEN
           KFA=23
           KFB=KTECHN+111
           ISUB=364
           VOGP=CSXI*CT2W/PARP(137)
           VRGP=-QUPD*CSXI*TANW/PARP(137)
           AOGP=0D0
           ARGP=0D0
           VAGP=QUPD*CSXI*(1D0-4D0*PARU(102))/SN2W
           VZGP=-QUPD*CSXI*CS2W/(1D0-PARU(102))
C... Z pi_tc'
        ELSEIF(ISUB.EQ.367) THEN
           KFA=23
           KFB=KTECHN+221
           ISUB=364
           VRGP=CSXIP*CT2W/PARP(137)
           VOGP=-QUPD*CSXIP*TANW/PARP(137)
           AOGP=0D0
           ARGP=0D0
           VAGP=CSXIP*(1D0-4D0*Q2UD*PARU(102))/SN2W
           VZGP=2D0*CSXIP*(CS2W+4D0*Q2UD*PARU(102)**2)/SN2W**2
C... W_T pi_tc
        ELSEIF(ISUB.EQ.368) THEN
           KFA=24
           KFB=KTECHN+211
           ISUB=364
           VOGP=CSXI/(2D0*SQRT(PARU(102)))/PARP(137)
           VRGP=0D0
           AOGP=0D0
C..........!!!!
           ARGP=-CSXI/(2D0*SQRT(PARU(102)))/PARP(138)
           VAGP=QUPD*CSXI/(2D0*SQRT(PARU(102)))
           VZGP=-QUPD*CSXI/(2D0*SQRT(1D0-PARU(102)))
C... rho_tc+ -> W_L Z_L
        ELSEIF(ISUB.EQ.370) THEN
           KFA=24
           KFB=23
           CAB2=PARP(141)**4
C... W_L pi_tc0
        ELSEIF(ISUB.EQ.371) THEN
           KFA=24
           KFB=KTECHN+111
           ISUB=370
           CAB2=PARP(141)**2*(1D0-PARP(141)**2)
C... Z_L pi_tc+
        ELSEIF(ISUB.EQ.372) THEN
           KFA=KTECHN+211
           KFB=23
           ISUB=370
           CAB2=PARP(141)**2*(1D0-PARP(141)**2)
C... pi_tc+ pi_tc0
        ELSEIF(ISUB.EQ.373) THEN
           KFA=KTECHN+211
           KFB=KTECHN+111
           ISUB=370
           CAB2=(1D0-PARP(141)**2)**2
C... gamma pi_tc+
        ELSEIF(ISUB.EQ.374) THEN
           KFA=KTECHN+211
           KFB=22
           VRGP=QUPD*CSXI
           ARGP=0D0
           VWGP=QUPD*CSXI/(2D0*SQRT(PARU(102)))
C... Z_T pi_tc+
        ELSEIF(ISUB.EQ.375) THEN
           KFA=KTECHN+211
           KFB=23
           ISUB=374
           VRGP=-QUPD*CSXI*TANW
           ARGP=CSXI/(2D0*SQRT(PARU(102)*(1D0-PARU(102))))
           VWGP=-QUPD*CSXI/(2D0*SQRT(1D0-PARU(102)))
C... W_T pi_tc0
        ELSEIF(ISUB.EQ.376) THEN
           KFA=24
           KFB=KTECHN+111
           ISUB=374
           VRGP=0D0
           ARGP=-CSXI/(2D0*SQRT(PARU(102)))
           VWGP=0D0
C... W_T pi_tc0'
        ELSEIF(ISUB.EQ.377) THEN
           KFA=24
           KFB=KTECHN+221
           ISUB=374
           ARGP=0D0
           VRGP=CSXIP/(2D0*SQRT(PARU(102)))
           VWGP=CSXIP/(2D0*PARU(102))
        ENDIF
      ENDIF
CMRENNA--
 
C...Read kinematical variables and limits
      ISTSB=ISET(ISUBSV)
      TAUMIN=VINT(11)
      YSTMIN=VINT(12)
      CTNMIN=VINT(13)
      CTPMIN=VINT(14)
      TAUPMN=VINT(16)
      TAU=VINT(21)
      YST=VINT(22)
      CTH=VINT(23)
      XT2=VINT(25)
      TAUP=VINT(26)
      TAUMAX=VINT(31)
      YSTMAX=VINT(32)
      CTNMAX=VINT(33)
      CTPMAX=VINT(34)
      TAUPMX=VINT(36)
 
C...Derive kinematical quantities
      TAUE=TAU
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=TAUP
      X(1)=SQRT(TAUE)*EXP(YST)
      X(2)=SQRT(TAUE)*EXP(-YST)
      IF(MINT(45).EQ.2.AND.ISTSB.GE.1) THEN
        IF(X(1).GT.1D0-1D-7) RETURN
      ELSEIF(MINT(45).EQ.3) THEN
        X(1)=MIN(1D0-1.1D-10,X(1))
      ENDIF
      IF(MINT(46).EQ.2.AND.ISTSB.GE.1) THEN
        IF(X(2).GT.1D0-1D-7) RETURN
      ELSEIF(MINT(46).EQ.3) THEN
        X(2)=MIN(1D0-1.1D-10,X(2))
      ENDIF
      SH=MAX(1D0,TAU*VINT(2))
      SQM3=VINT(63)
      SQM4=VINT(64)
      RM3=SQM3/SH
      RM4=SQM4/SH
      BE34=SQRT(MAX(0D0,(1D0-RM3-RM4)**2-4D0*RM3*RM4))
      RPTS=4D0*VINT(71)**2/SH
      BE34L=SQRT(MAX(0D0,(1D0-RM3-RM4)**2-4D0*RM3*RM4-RPTS))
      RM34=MAX(1D-20,2D0*RM3*RM4)
      RSQM=1D0+RM34
      IF(2D0*VINT(71)**2/MAX(1D0,VINT(21)*VINT(2)).LT.0.0001D0)
     &RM34=MAX(RM34,2D0*VINT(71)**2/MAX(1D0,VINT(21)*VINT(2)))
      RTHM=(4D0*RM3*RM4+RPTS)/(1D0-RM3-RM4+BE34L)
      IF(ISTSB.EQ.0) THEN
        TH=VINT(45)
        UH=-0.5D0*SH*MAX(RTHM,1D0-RM3-RM4+BE34*CTH)
        SQPTH=MAX(VINT(71)**2,0.25D0*SH*BE34**2*VINT(59)**2)
      ELSE
C...Kinematics with incoming masses tricky: now depends on how
C...subprocess has been set up w.r.t. order of incoming partons.
        RM1=0D0
        IF(MINT(15).EQ.22.AND.VINT(3).LT.0D0) RM1=-VINT(3)**2/SH
        RM2=0D0
        IF(MINT(16).EQ.22.AND.VINT(4).LT.0D0) RM2=-VINT(4)**2/SH
        IF(ISUB.EQ.35) THEN
          RM2=MIN(RM1,RM2)
          RM1=0D0
        ENDIF
        BE12=SQRT(MAX(0D0,(1D0-RM1-RM2)**2-4D0*RM1*RM2))
        TUCOM=(1D0-RM1-RM2)*(1D0-RM3-RM4)
        TH=-0.5D0*SH*MAX(RTHM,TUCOM-2D0*RM1*RM4-2D0*RM2*RM3-
     &  BE12*BE34*CTH)
        UH=-0.5D0*SH*MAX(RTHM,TUCOM-2D0*RM1*RM3-2D0*RM2*RM4+
     &  BE12*BE34*CTH)
        SQPTH=MAX(VINT(71)**2,0.25D0*SH*BE34**2*(1D0-CTH**2))
      ENDIF
      SHR=SQRT(SH)
      SH2=SH**2
      TH2=TH**2
      UH2=UH**2
 
C...Choice of Q2 scale: hard, parton distributions, parton showers
      IF(ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5) THEN
        Q2=SH
      ELSEIF(ISTSB.EQ.8) THEN
        IF(MINT(107).EQ.4) Q2=VINT(307)
        IF(MINT(108).EQ.4) Q2=VINT(308)
      ELSEIF(MOD(ISTSB,2).EQ.0.OR.ISTSB.EQ.9) THEN
        Q2IN1=0D0
        IF(MINT(11).EQ.22.AND.VINT(3).LT.0D0) Q2IN1=VINT(3)**2
        Q2IN2=0D0
        IF(MINT(12).EQ.22.AND.VINT(4).LT.0D0) Q2IN2=VINT(4)**2
        IF(MSTP(32).EQ.1) THEN
          Q2=2D0*SH*TH*UH/(SH**2+TH**2+UH**2)
        ELSEIF(MSTP(32).EQ.2) THEN
          Q2=SQPTH+0.5D0*(SQM3+SQM4)
        ELSEIF(MSTP(32).EQ.3) THEN
          Q2=MIN(-TH,-UH)
        ELSEIF(MSTP(32).EQ.4) THEN
          Q2=SH
        ELSEIF(MSTP(32).EQ.5) THEN
          Q2=-TH
        ELSEIF(MSTP(32).EQ.6) THEN
          XSF1=X(1)
          IF(ISTSB.EQ.9) XSF1=X(1)/VINT(143)
          XSF2=X(2)
          IF(ISTSB.EQ.9) XSF2=X(2)/VINT(144)
          Q2=(1D0+XSF1*Q2IN1/SH+XSF2*Q2IN2/SH)*
     &    (SQPTH+0.5D0*(SQM3+SQM4))
        ELSEIF(MSTP(32).EQ.7) THEN
          Q2=(1D0+Q2IN1/SH+Q2IN2/SH)*(SQPTH+0.5D0*(SQM3+SQM4))
        ELSEIF(MSTP(32).EQ.8) THEN
          Q2=SQPTH+0.5D0*(Q2IN1+Q2IN2+SQM3+SQM4)
        ELSEIF(MSTP(32).EQ.9) THEN
          Q2=SQPTH+Q2IN1+Q2IN2+SQM3+SQM4
        ELSEIF(MSTP(32).EQ.10) THEN
          Q2=VINT(2)
        ENDIF
        IF(ISTSB.EQ.9) Q2=SQPTH
        IF(ISTSB.EQ.9.AND.MSTP(82).GE.2) Q2=Q2+
     &  (PARP(82)*(VINT(1)/PARP(89))**PARP(90))**2
      ENDIF
      Q2SF=Q2
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        Q2SF=PMAS(23,1)**2
        IF(ISUB.EQ.8.OR.ISUB.EQ.76.OR.ISUB.EQ.77.OR.ISUB.EQ.124.OR.
     &  ISUB.EQ.351) Q2SF=PMAS(24,1)**2
        IF(ISUB.EQ.352) Q2SF=PMAS(PYCOMP(9900024),1)**2
        IF(ISUB.EQ.121.OR.ISUB.EQ.122) THEN
          Q2SF=PMAS(PYCOMP(KFPR(ISUBSV,2)),1)**2
          IF(MSTP(39).EQ.2) Q2SF=Q2SF+MAX(VINT(202),VINT(207))
          IF(MSTP(39).EQ.3) Q2SF=SH
          IF(MSTP(39).EQ.4) Q2SF=VINT(26)*VINT(2)
          IF(MSTP(39).EQ.5) Q2SF=PMAS(KFHIGG,1)**2
        ENDIF
      ENDIF
      Q2PS=Q2SF
      Q2SF=Q2SF*PARP(34)
      IF(MSTP(69).GE.1.AND.MINT(47).EQ.5) Q2SF=VINT(2)
      IF(MSTP(69).GE.2) Q2SF=VINT(2)
      IF(MSTP(22).GE.1.AND.(ISUB.EQ.10.OR.ISUB.EQ.83).AND.
     &(MINT(43).EQ.2.OR.MINT(43).EQ.3)) THEN
        XBJ=X(2)
        IF(MINT(43).EQ.3) XBJ=X(1)
        IF(MSTP(22).EQ.1) THEN
          Q2PS=-TH
        ELSEIF(MSTP(22).EQ.2) THEN
          Q2PS=((1D0-XBJ)/XBJ)*(-TH)
        ELSEIF(MSTP(22).EQ.3) THEN
          Q2PS=SQRT((1D0-XBJ)/XBJ)*(-TH)
        ELSE
          Q2PS=(1D0-XBJ)*MAX(1D0,-LOG(XBJ))*(-TH)
        ENDIF
      ENDIF
      IF(MSTP(68).EQ.1.AND.(ISUBSV.EQ.1.OR.ISUBSV.EQ.2.OR.
     &ISUBSV.EQ.102.OR.ISUBSV.EQ.141.OR.ISUBSV.EQ.142.OR.
     &ISUBSV.EQ.144.OR.ISUBSV.EQ.152.OR.ISUBSV.EQ.157)) THEN
        Q2PS=VINT(2)
      ELSEIF(MSTP(68).GE.2.AND.(ISUBSV.NE.11.AND.ISUBSV.NE.12.AND.
     &ISUBSV.NE.13.AND.ISUBSV.NE.28.AND.ISUBSV.NE.53.AND.
     &ISUBSV.NE.68)) THEN
        Q2PS=VINT(2)
      ENDIF
 
C...Store derived kinematical quantities
      VINT(41)=X(1)
      VINT(42)=X(2)
      VINT(44)=SH
      VINT(43)=SQRT(SH)
      VINT(45)=TH
      VINT(46)=UH
      IF(ISTSB.NE.8) VINT(48)=SQPTH
      IF(ISTSB.NE.8) VINT(47)=SQRT(SQPTH)
      VINT(50)=TAUP*VINT(2)
      VINT(49)=SQRT(MAX(0D0,VINT(50)))
      VINT(52)=Q2
      VINT(51)=SQRT(Q2)
      VINT(54)=Q2SF
      VINT(53)=SQRT(Q2SF)
      VINT(56)=Q2PS
      VINT(55)=SQRT(Q2PS)
 
C...Calculate parton distributions
      IF(ISTSB.LE.0) GOTO 160
      IF(MINT(47).GE.2) THEN
        DO 110 I=3-MIN(2,MINT(45)),MIN(2,MINT(46))
          XSF=X(I)
          IF(ISTSB.EQ.9) XSF=X(I)/VINT(142+I)
          IF(ISUB.EQ.99) THEN
            XSF=VINT(309-I)/(VINT(2)+VINT(307)+VINT(308))
            Q2SF=VINT(309-I)
          ENDIF
          MINT(105)=MINT(102+I)
          MINT(109)=MINT(106+I)
          VINT(120)=VINT(2+I)
          IF(MSTP(57).LE.1) THEN
            CALL PYPDFU(MINT(10+I),XSF,Q2SF,XPQ)
          ELSE
            CALL PYPDFL(MINT(10+I),XSF,Q2SF,XPQ)
          ENDIF
          DO 100 KFL=-25,25
            XSFX(I,KFL)=XPQ(KFL)
  100     CONTINUE
  110   CONTINUE
      ENDIF
 
C...Calculate alpha_em, alpha_strong and K-factor
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2.OR.(ISUB.GE.71.AND.ISUB.LE.77)) XW=
     &1D0-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1D0-XW
      XWC=1D0/(16D0*XW*XW1)
      AEM=PYALEM(Q2)
      IF(MSTP(8).GE.1) AEM=SQRT(2D0)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      IF(MSTP(33).NE.3) AS=PYALPS(PARP(34)*Q2)
      FACK=1D0
      FACA=1D0
      IF(MSTP(33).EQ.1) THEN
        FACK=PARP(31)
      ELSEIF(MSTP(33).EQ.2) THEN
        FACK=PARP(31)
        FACA=PARP(32)/PARP(31)
      ELSEIF(MSTP(33).EQ.3) THEN
        Q2AS=PARP(33)*Q2
        IF(ISTSB.EQ.9.AND.MSTP(82).GE.2) Q2AS=Q2AS+
     &  PARU(112)*PARP(82)*(VINT(1)/PARP(89))**PARP(90)
        AS=PYALPS(Q2AS)
      ENDIF
      VINT(138)=1D0
      VINT(57)=AEM
      VINT(58)=AS
 
C...Set flags for allowed reacting partons/leptons
      DO 140 I=1,2
        DO 120 J=-25,25
          KFAC(I,J)=0
  120   CONTINUE
        IF(MINT(44+I).EQ.1) THEN
          KFAC(I,MINT(10+I))=1
        ELSEIF(MINT(40+I).EQ.1.AND.MSTP(12).EQ.0) THEN
          KFAC(I,MINT(10+I))=1
          KFAC(I,22)=1
          KFAC(I,24)=1
          KFAC(I,-24)=1
        ELSE
          DO 130 J=-25,25
            KFAC(I,J)=KFIN(I,J)
            IF(IABS(J).GT.MSTP(58).AND.IABS(J).LE.10) KFAC(I,J)=0
            IF(XSFX(I,J).LT.1D-10) KFAC(I,J)=0
  130     CONTINUE
        ENDIF
  140 CONTINUE
 
C...Lower and upper limit for fermion flavour loops
      MMIN1=0
      MMAX1=0
      MMIN2=0
      MMAX2=0
      DO 150 J=-20,20
        IF(KFAC(1,-J).EQ.1) MMIN1=-J
        IF(KFAC(1,J).EQ.1) MMAX1=J
        IF(KFAC(2,-J).EQ.1) MMIN2=-J
        IF(KFAC(2,J).EQ.1) MMAX2=J
  150 CONTINUE
      MMINA=MIN(MMIN1,MMIN2)
      MMAXA=MAX(MMAX1,MMAX2)
 
C...Common resonance mass and width combinations
      SQMZ=PMAS(23,1)**2
      SQMW=PMAS(24,1)**2
      SQMH=PMAS(KFHIGG,1)**2
      GMMZ=PMAS(23,1)*PMAS(23,2)
      GMMW=PMAS(24,1)*PMAS(24,2)
      GMMH=PMAS(KFHIGG,1)*PMAS(KFHIGG,2)
C...MRENNA+++
      ZWID=PMAS(23,2)
      WWID=PMAS(24,2)
      TANW=SQRT(XW/XW1)
      CT2W=(1D0-2D0*XW)/(2D0*XW/TANW)
C...MRENNA---
C...Polarization factors...implemented so far for W+W-(25)
      POLR=(1D0+PARJ(132))*(1D0-PARJ(131))
      POLL=(1D0-PARJ(132))*(1D0+PARJ(131))
      POLRR=(1D0+PARJ(132))*(1D0+PARJ(131))
      POLLL=(1D0-PARJ(132))*(1D0-PARJ(131))
 
C...Phase space integral in tau
      COMFAC=PARU(1)*PARU(5)/VINT(2)
      IF(MINT(41).EQ.2.AND.MINT(42).EQ.2) COMFAC=COMFAC*FACK
      IF((MINT(47).GE.2.OR.(ISTSB.GE.3.AND.ISTSB.LE.5)).AND.
     &ISTSB.NE.8.AND.ISTSB.NE.9) THEN
        ATAU1=LOG(TAUMAX/TAUMIN)
        ATAU2=(TAUMAX-TAUMIN)/(TAUMAX*TAUMIN)
        H1=COEF(ISUBSV,1)+(ATAU1/ATAU2)*COEF(ISUBSV,2)/TAU
        IF(MINT(72).GE.1) THEN
          TAUR1=VINT(73)
          GAMR1=VINT(74)
          ATAUD=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR1)/(TAUMAX+TAUR1))
          ATAU3=ATAUD/TAUR1
          IF(ATAUD.GT.1D-10) H1=H1+
     &    (ATAU1/ATAU3)*COEF(ISUBSV,3)/(TAU+TAUR1)
          ATAUD=ATAN((TAUMAX-TAUR1)/GAMR1)-ATAN((TAUMIN-TAUR1)/GAMR1)
          ATAU4=ATAUD/GAMR1
          IF(ATAUD.GT.1D-10) H1=H1+
     &    (ATAU1/ATAU4)*COEF(ISUBSV,4)*TAU/((TAU-TAUR1)**2+GAMR1**2)
        ENDIF
        IF(MINT(72).EQ.2) THEN
          TAUR2=VINT(75)
          GAMR2=VINT(76)
          ATAUD=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR2)/(TAUMAX+TAUR2))
          ATAU5=ATAUD/TAUR2
          IF(ATAUD.GT.1D-10) H1=H1+
     &    (ATAU1/ATAU5)*COEF(ISUBSV,5)/(TAU+TAUR2)
          ATAUD=ATAN((TAUMAX-TAUR2)/GAMR2)-ATAN((TAUMIN-TAUR2)/GAMR2)
          ATAU6=ATAUD/GAMR2
          IF(ATAUD.GT.1D-10) H1=H1+
     &    (ATAU1/ATAU6)*COEF(ISUBSV,6)*TAU/((TAU-TAUR2)**2+GAMR2**2)
        ENDIF
        IF(MINT(47).EQ.5.AND.(ISTSB.LE.2.OR.ISTSB.GE.5)) THEN
          ATAU7=LOG(MAX(2D-10,1D0-TAUMIN)/MAX(2D-10,1D0-TAUMAX))
          IF(ATAU7.GT.1D-10) H1=H1+(ATAU1/ATAU7)*COEF(ISUBSV,7)*TAU/
     &    MAX(2D-10,1D0-TAU)
        ELSEIF(MINT(47).GE.6.AND.(ISTSB.LE.2.OR.ISTSB.GE.5)) THEN
          ATAU7=LOG(MAX(1D-10,1D0-TAUMIN)/MAX(1D-10,1D0-TAUMAX))
          IF(ATAU7.GT.1D-10) H1=H1+(ATAU1/ATAU7)*COEF(ISUBSV,7)*TAU/
     &    MAX(1D-10,1D0-TAU)
        ENDIF
        COMFAC=COMFAC*ATAU1/(TAU*H1)
      ENDIF
 
C...Phase space integral in y*
      IF((MINT(47).EQ.4.OR.MINT(47).EQ.5).AND.ISTSB.NE.8.AND.ISTSB.NE.9)
     &THEN
        AYST0=YSTMAX-YSTMIN
        IF(AYST0.LT.1D-10) THEN
          COMFAC=0D0
        ELSE
          AYST1=0.5D0*(YSTMAX-YSTMIN)**2
          AYST2=AYST1
          AYST3=2D0*(ATAN(EXP(YSTMAX))-ATAN(EXP(YSTMIN)))
          H2=(AYST0/AYST1)*COEF(ISUBSV,8)*(YST-YSTMIN)+
     &    (AYST0/AYST2)*COEF(ISUBSV,9)*(YSTMAX-YST)+
     &    (AYST0/AYST3)*COEF(ISUBSV,10)/COSH(YST)
          IF(MINT(45).EQ.3) THEN
            YST0=-0.5D0*LOG(TAUE)
            AYST4=LOG(MAX(1D-10,EXP(YST0-YSTMIN)-1D0)/
     &      MAX(1D-10,EXP(YST0-YSTMAX)-1D0))
            IF(AYST4.GT.1D-10) H2=H2+(AYST0/AYST4)*COEF(ISUBSV,11)/
     &      MAX(1D-10,1D0-EXP(YST-YST0))
          ENDIF
          IF(MINT(46).EQ.3) THEN
            YST0=-0.5D0*LOG(TAUE)
            AYST5=LOG(MAX(1D-10,EXP(YST0+YSTMAX)-1D0)/
     &      MAX(1D-10,EXP(YST0+YSTMIN)-1D0))
            IF(AYST5.GT.1D-10) H2=H2+(AYST0/AYST5)*COEF(ISUBSV,12)/
     &      MAX(1D-10,1D0-EXP(-YST-YST0))
          ENDIF
          COMFAC=COMFAC*AYST0/H2
        ENDIF
      ENDIF
 
C...2 -> 1 processes: reduction in angular part of phase space integral
C...for case of decaying resonance
      ACTH0=CTNMAX-CTNMIN+CTPMAX-CTPMIN
      IF((ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5)) THEN
        IF(MDCY(PYCOMP(KFPR(ISUBSV,1)),1).EQ.1) THEN
          IF(KFPR(ISUB,1).EQ.25.OR.KFPR(ISUB,1).EQ.37.OR.
     &    KFPR(ISUB,1).EQ.39) THEN
            COMFAC=COMFAC*0.5D0*ACTH0
          ELSE
            COMFAC=COMFAC*0.125D0*(3D0*ACTH0+CTNMAX**3-CTNMIN**3+
     &      CTPMAX**3-CTPMIN**3)
          ENDIF
        ENDIF
 
C...2 -> 2 processes: angular part of phase space integral
      ELSEIF(ISTSB.EQ.2.OR.ISTSB.EQ.4) THEN
        ACTH1=LOG((MAX(RM34,RSQM-CTNMIN)*MAX(RM34,RSQM-CTPMIN))/
     &  (MAX(RM34,RSQM-CTNMAX)*MAX(RM34,RSQM-CTPMAX)))
        ACTH2=LOG((MAX(RM34,RSQM+CTNMAX)*MAX(RM34,RSQM+CTPMAX))/
     &  (MAX(RM34,RSQM+CTNMIN)*MAX(RM34,RSQM+CTPMIN)))
        ACTH3=1D0/MAX(RM34,RSQM-CTNMAX)-1D0/MAX(RM34,RSQM-CTNMIN)+
     &  1D0/MAX(RM34,RSQM-CTPMAX)-1D0/MAX(RM34,RSQM-CTPMIN)
        ACTH4=1D0/MAX(RM34,RSQM+CTNMIN)-1D0/MAX(RM34,RSQM+CTNMAX)+
     &  1D0/MAX(RM34,RSQM+CTPMIN)-1D0/MAX(RM34,RSQM+CTPMAX)
        H3=COEF(ISUBSV,13)+
     &  (ACTH0/ACTH1)*COEF(ISUBSV,14)/MAX(RM34,RSQM-CTH)+
     &  (ACTH0/ACTH2)*COEF(ISUBSV,15)/MAX(RM34,RSQM+CTH)+
     &  (ACTH0/ACTH3)*COEF(ISUBSV,16)/MAX(RM34,RSQM-CTH)**2+
     &  (ACTH0/ACTH4)*COEF(ISUBSV,17)/MAX(RM34,RSQM+CTH)**2
        COMFAC=COMFAC*ACTH0*0.5D0*BE34/H3
 
C...2 -> 2 processes: take into account final state Breit-Wigners
        COMFAC=COMFAC*VINT(80)
      ENDIF
 
C...2 -> 3, 4 processes: phace space integral in tau'
      IF(MINT(47).GE.2.AND.ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        ATAUP1=LOG(TAUPMX/TAUPMN)
        ATAUP2=((1D0-TAU/TAUPMX)**4-(1D0-TAU/TAUPMN)**4)/(4D0*TAU)
        H4=COEF(ISUBSV,18)+
     &  (ATAUP1/ATAUP2)*COEF(ISUBSV,19)*(1D0-TAU/TAUP)**3/TAUP
        IF(MINT(47).EQ.5) THEN
          ATAUP3=LOG(MAX(2D-10,1D0-TAUPMN)/MAX(2D-10,1D0-TAUPMX))
          H4=H4+(ATAUP1/ATAUP3)*COEF(ISUBSV,20)*TAUP/MAX(2D-10,1D0-TAUP)
        ELSEIF(MINT(47).GE.6) THEN
          ATAUP3=LOG(MAX(1D-10,1D0-TAUPMN)/MAX(1D-10,1D0-TAUPMX))
          H4=H4+(ATAUP1/ATAUP3)*COEF(ISUBSV,20)*TAUP/MAX(1D-10,1D0-TAUP)
        ENDIF
        COMFAC=COMFAC*ATAUP1/H4
      ENDIF
 
C...2 -> 3, 4 processes: effective W/Z parton distributions
      IF(ISTSB.EQ.3.OR.ISTSB.EQ.4) THEN
        IF(1D0-TAU/TAUP.GT.1D-4) THEN
          FZW=(1D0+TAU/TAUP)*LOG(TAUP/TAU)-2D0*(1D0-TAU/TAUP)
        ELSE
          FZW=1D0/6D0*(1D0-TAU/TAUP)**3*TAU/TAUP
        ENDIF
        COMFAC=COMFAC*FZW
      ENDIF
 
C...2 -> 3 processes: phase space integrals for pT1, pT2, y3, mirror
      IF(ISTSB.EQ.5) THEN
        COMFAC=COMFAC*VINT(205)*VINT(210)*VINT(212)*VINT(214)/
     &  (128D0*PARU(1)**4*VINT(220))*(TAU**2/TAUP)
      ENDIF
 
C...Phase space integral for low-pT and multiple interactions
      IF(ISTSB.EQ.9) THEN
        COMFAC=PARU(1)*PARU(5)*FACK*0.5D0*VINT(2)/SH2
        ATAU1=LOG(2D0*(1D0+SQRT(1D0-XT2))/XT2-1D0)
        ATAU2=2D0*ATAN(1D0/XT2-1D0)/SQRT(XT2)
        H1=COEF(ISUBSV,1)+(ATAU1/ATAU2)*COEF(ISUBSV,2)/SQRT(TAU)
        COMFAC=COMFAC*ATAU1/H1
        AYST0=YSTMAX-YSTMIN
        AYST1=0.5D0*(YSTMAX-YSTMIN)**2
        AYST3=2D0*(ATAN(EXP(YSTMAX))-ATAN(EXP(YSTMIN)))
        H2=(AYST0/AYST1)*COEF(ISUBSV,8)*(YST-YSTMIN)+
     &  (AYST0/AYST1)*COEF(ISUBSV,9)*(YSTMAX-YST)+
     &  (AYST0/AYST3)*COEF(ISUBSV,10)/COSH(YST)
        COMFAC=COMFAC*AYST0/H2
        IF(MSTP(82).LE.1) COMFAC=COMFAC*XT2**2*(1D0/VINT(149)-1D0)
C...For MSTP(82)>=2 an additional factor (xT2/(xT2+VINT(149))**2 is
C...introduced to make cross-section finite for xT2 -> 0
        IF(MSTP(82).GE.2) COMFAC=COMFAC*XT2**2/(VINT(149)*
     &  (1D0+VINT(149)))
      ENDIF
 
C...Real gamma + gamma: include factor 2 when different nature
  160 IF(MINT(11).EQ.22.AND.MINT(12).EQ.22.AND.MINT(123).GE.4.AND.
     &MSTP(14).LE.10) COMFAC=2D0*COMFAC
 
C...Extra factors to include the effects of
C...longitudinal resolved photons (but not direct or DIS ones).
      DO 170 ISDE=1,2
        IF(MINT(10+ISDE).EQ.22.AND.MINT(106+ISDE).GE.1.AND.
     &  MINT(106+ISDE).LE.3) THEN
          VINT(314+ISDE)=1D0
          XY=PARP(166+ISDE)
          IF(MSTP(16).EQ.0) THEN
            IF(VINT(304+ISDE).GT.0D0.AND.VINT(304+ISDE).LT.1D0)
     &      XY=VINT(304+ISDE)
          ELSE
            IF(VINT(308+ISDE).GT.0D0.AND.VINT(308+ISDE).LT.1D0)
     &      XY=VINT(308+ISDE)
          ENDIF
          Q2GA=VINT(306+ISDE)
          IF(MSTP(17).GT.0.AND.XY.GT.0D0.AND.XY.LT.1D0.AND.
     &    Q2GA.GT.0D0) THEN
            REDUCE=0D0
            IF(MSTP(17).EQ.1) THEN
              REDUCE=4D0*Q2*Q2GA/(Q2+Q2GA)**2
            ELSEIF(MSTP(17).EQ.2) THEN
              REDUCE=4D0*Q2GA/(Q2+Q2GA)
            ELSEIF(MSTP(17).EQ.3) THEN
              PMVIRT=PMAS(PYCOMP(113),1)
              REDUCE=4D0*Q2GA/(PMVIRT**2+Q2GA)
            ELSEIF(MSTP(17).EQ.4.AND.MINT(106+ISDE).EQ.1) THEN
              PMVIRT=PMAS(PYCOMP(113),1)
              REDUCE=4D0*PMVIRT**2*Q2GA/(PMVIRT**2+Q2GA)**2
            ELSEIF(MSTP(17).EQ.4.AND.MINT(106+ISDE).EQ.2) THEN
              PMVIRT=PMAS(PYCOMP(113),1)
              REDUCE=4D0*PMVIRT**2*Q2GA/(PMVIRT**2+Q2GA)**2
            ELSEIF(MSTP(17).EQ.4.AND.MINT(106+ISDE).EQ.3) THEN
              PMVSMN=4D0*PARP(15)**2
              PMVSMX=4D0*VINT(154)**2
              REDTRA=1D0/(PMVSMN+Q2GA)-1D0/(PMVSMX+Q2GA)
              REDLON=(3D0*PMVSMN+Q2GA)/(PMVSMN+Q2GA)**3-
     &        (3D0*PMVSMX+Q2GA)/(PMVSMX+Q2GA)**3
              REDUCE=4D0*(Q2GA/6D0)*REDLON/REDTRA
            ELSEIF(MSTP(17).EQ.5.AND.MINT(106+ISDE).EQ.1) THEN
              PMVIRT=PMAS(PYCOMP(113),1)
              REDUCE=4D0*Q2GA/(PMVIRT**2+Q2GA)
            ELSEIF(MSTP(17).EQ.5.AND.MINT(106+ISDE).EQ.2) THEN
              PMVIRT=PMAS(PYCOMP(113),1)
              REDUCE=4D0*Q2GA/(PMVIRT**2+Q2GA)
            ELSEIF(MSTP(17).EQ.5.AND.MINT(106+ISDE).EQ.3) THEN
              PMVSMN=4D0*PARP(15)**2
              PMVSMX=4D0*VINT(154)**2
              REDTRA=1D0/(PMVSMN+Q2GA)-1D0/(PMVSMX+Q2GA)
              REDLON=1D0/(PMVSMN+Q2GA)**2-1D0/(PMVSMX+Q2GA)**2
              REDUCE=4D0*(Q2GA/2D0)*REDLON/REDTRA
            ENDIF
            BEAMAS=PYMASS(11)
            IF(VINT(302+ISDE).GT.0D0) BEAMAS=VINT(302+ISDE)
            FRACLT=1D0/(1D0+XY**2/2D0/(1D0-XY)*
     &      (1D0-2D0*BEAMAS**2/Q2GA))
            VINT(314+ISDE)=1D0+PARP(165)*REDUCE*FRACLT
          ENDIF
        ELSE
          VINT(314+ISDE)=1D0
        ENDIF
        COMFAC=COMFAC*VINT(314+ISDE)
  170 CONTINUE
 
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
      IF((MSTP(46).GE.3.AND.MSTP(46).LE.6).AND.(ISUB.EQ.71.OR.ISUB.EQ.
     &72.OR.ISUB.EQ.73.OR.ISUB.EQ.76.OR.ISUB.EQ.77)) THEN
C...Calculate M_R and N_R functions for Higgs-like and QCD-like models
        IF(MSTP(46).LE.4) THEN
          HDTLH=LOG(PMAS(25,1)/PARP(44))
          HDTMR=(4.5D0*PARU(1)/SQRT(3D0)-74D0/9D0)/8D0+HDTLH/12D0
          HDTNR=-1D0/18D0+HDTLH/6D0
        ELSE
          HDTNM=0.125D0*(1D0/(288D0*PARU(1)**2)+(PARP(47)/PARP(45))**2)
          HDTLQ=LOG(PARP(45)/PARP(44))
          HDTMR=-(4D0*PARU(1))**2*0.5D0*HDTNM+HDTLQ/12D0
          HDTNR=(4D0*PARU(1))**2*HDTNM+HDTLQ/6D0
        ENDIF
 
C...Calculate lowest and next-to-lowest order partial wave amplitudes
        HDTV=1D0/(16D0*PARU(1)*PARP(47)**2)
        A00L=DBLE(HDTV*SH)
        A20L=-0.5D0*A00L
        A11L=A00L/6D0
        HDTLS=LOG(SH/PARP(44)**2)
        A004=DBLE((HDTV*SH)**2/(4D0*PARU(1)))*
     &  CMPLX(DBLE((176D0*HDTMR+112D0*HDTNR)/3D0+11D0/27D0-
     &  (50D0/9D0)*HDTLS),DBLE(4D0*PARU(1)))
        A204=DBLE((HDTV*SH)**2/(4D0*PARU(1)))*
     &  CMPLX(DBLE(32D0*(HDTMR+2D0*HDTNR)/3D0+25D0/54D0-
     &  (20D0/9D0)*HDTLS),DBLE(PARU(1)))
        A114=DBLE((HDTV*SH)**2/(6D0*PARU(1)))*
     &  CMPLX(DBLE(4D0*(-2D0*HDTMR+HDTNR)-1D0/18D0),DBLE(PARU(1)/6D0))
 
C...Unitarize partial wave amplitudes with Pade or K-matrix method
        IF(MSTP(46).EQ.3.OR.MSTP(46).EQ.5) THEN
          A00U=A00L/(1D0-A004/A00L)
          A20U=A20L/(1D0-A204/A20L)
          A11U=A11L/(1D0-A114/A11L)
        ELSE
          A00U=(A00L+DBLE(A004))/(1D0-DCMPLX(0.D0,A00L+DBLE(A004)))
          A20U=(A20L+DBLE(A204))/(1D0-DCMPLX(0.D0,A20L+DBLE(A204)))
          A11U=(A11L+DBLE(A114))/(1D0-DCMPLX(0.D0,A11L+DBLE(A114)))
        ENDIF
      ENDIF
 
C...Supersymmetric processes - all of type 2 -> 2 :
C...correct final-state Breit-Wigners from fixed to running width.
      IF(ISUB.GE.200.AND.ISUB.LE.301.AND.MSTP(42).GT.0) THEN
        DO 180 I=1,2
        KFLW=KFPR(ISUBSV,I)
        KCW=PYCOMP(KFLW)
        IF(PMAS(KCW,2).LT.PARP(41)) GOTO 180
        IF(I.EQ.1) SQMI=SQM3
        IF(I.EQ.2) SQMI=SQM4
        SQMS=PMAS(KCW,1)**2
        GMMS=PMAS(KCW,1)*PMAS(KCW,2)
        HBWS=GMMS/((SQMI-SQMS)**2+GMMS**2)
        CALL PYWIDT(KFLW,SQMI,WDTP,WDTE)
        GMMI=SQRT(SQMI)*WDTP(0)
        HBWI=GMMI/((SQMI-SQMS)**2+GMMI**2)
        COMFAC=COMFAC*(HBWI/HBWS)
  180   CONTINUE
      ENDIF
 
C...QCD 2 -> 2 processes: corrections from virtual technicolor exchange.
      IF(ISUB.EQ.11.OR.ISUB.EQ.12.OR.ISUB.EQ.28.OR.ISUB.EQ.53.OR.
     $ISUB.EQ.68.OR.ISUB.EQ.81.OR.ISUB.EQ.82) THEN
        IF(MSTP(5).LE.4) THEN
          SQDQQS=1D0/SH2
          SQDQQT=1D0/TH2
          SQDQQU=1D0/UH2
          SQDGGS=SQDQQS
          SQDGGT=SQDQQT
          SQDGGU=SQDQQU
          REDGGS=1D0/SH
          REDGGT=1D0/TH
          REDGGU=1D0/UH
          REDGTU=1D0/UH/TH
          REDGSU=1D0/SH/UH
          REDGST=1D0/SH/TH
          REDQST=1D0/SH/TH
          REDQTU=1D0/UH/TH
          SQDLGS=0D0
          SQDLGT=0D0
          SQDQTS=SQDQQS
        ELSEIF(MSTP(5).EQ.5) THEN
          TANT3=ABS(PARP(155))
          IF(PARP(155).GT.0) THEN
            IMDL=1
          ELSE
            IMDL=2
          ENDIF
          ALPRHT=2.91D0*(3D0/PARP(144))
          SIN2T=2D0*TANT3/(TANT3**2+1D0)
          SINT3=TANT3/SQRT(TANT3**2+1D0)
          XIG=SQRT(PYALPS(SH)/ALPRHT)
          X12=(1D0/SQRT(2D0)*1D0/SQRT(2D0)+
     &        1D0/SQRT(2D0)*1D0/SQRT(2D0))/SQRT(2D0)/SIN2T
          X21=1D-3
          X11=(.25D0*((1D0/SQRT(2D0))**2+(1D0/SQRT(2D0))**2+2D0)-
     &    SINT3**2)*2D0/SIN2T
          X22=(.25D0*((1D0/SQRT(2D0))**2+(1D0/SQRT(2D0))**2)-
     &    SINT3**2)*2D0/SIN2T
          IF(PARP(156).GT.0.5D0) THEN
            SM1122=1D-6
            SM1112=1D-6
            SM1121=1D-6
            SM2212=1D-6
            SM2221=1D-6
            SM1221=1D-6
            X12=1D-6
            X21=1D-6
            X11=(1D0-SINT3**2)*2D0/SIN2T
            X22=-SINT3**2*2D0/SIN2T
          ELSE
            SM1122=100D0**2
            SM1112=150D0**2
            SM1121=150D0**2
            SM2212=150D0**2
            SM2221=75D0**2
            SM1221=50D0**2
          ENDIF
 
C.........SH LOOP
          ZTC(1,1)=DCMPLX(SH,0D0)
          CALL PYWIDT(3100021,SH,WDTP,WDTE)
          ZTC(2,2)=DCMPLX(SH-PMAS(PYCOMP(3100021),1)**2,-SHR*WDTP(0))
          CALL PYWIDT(3100113,SH,WDTP,WDTE)
          ZTC(3,3)=DCMPLX(SH-PMAS(PYCOMP(3100113),1)**2,-SHR*WDTP(0))
          CALL PYWIDT(3400113,SH,WDTP,WDTE)
          ZTC(4,4)=DCMPLX(SH-PMAS(PYCOMP(3400113),1)**2,-SHR*WDTP(0))
          CALL PYWIDT(3200113,SH,WDTP,WDTE)
          ZTC(5,5)=DCMPLX(SH-PMAS(PYCOMP(3200113),1)**2,-SHR*WDTP(0))
          CALL PYWIDT(3300113,SH,WDTP,WDTE)
          ZTC(6,6)=DCMPLX(SH-PMAS(PYCOMP(3300113),1)**2,-SHR*WDTP(0))
          ZTC(1,2)=(0D0,0D0)
          ZTC(1,3)=DCMPLX(SH*XIG,0D0)
          ZTC(1,4)=ZTC(1,3)
          ZTC(1,5)=ZTC(1,2)
          ZTC(1,6)=ZTC(1,2)
          ZTC(2,3)=DCMPLX(SH*XIG*X11,0D0)
          ZTC(2,4)=DCMPLX(SH*XIG*X22,0D0)
          ZTC(2,5)=DCMPLX(SH*XIG*X12,0D0)
          ZTC(2,6)=DCMPLX(SH*XIG*X21,0D0)
          ZTC(3,4)=-SM1122
          ZTC(3,5)=-SM1112
          ZTC(3,6)=-SM1121
          ZTC(4,5)=-SM2212
          ZTC(4,6)=-SM2221
          ZTC(5,6)=-SM1221
 
 
          DO 200 I=1,5
            DO 190 J=I+1,6
               ZTC(J,I)=ZTC(I,J)
  190       CONTINUE
  200     CONTINUE
          CALL PYLDCM(ZTC,6,6,INDX,D)
          DO 220 I=1,6
            DO 210 J=1,6
              YTC(I,J)=(0D0,0D0)
              IF(I.EQ.J) YTC(I,J)=(1D0,0D0)
  210       CONTINUE
  220     CONTINUE
 
          DO 230 I=1,6
            CALL PYBKSB(ZTC,6,6,INDX,YTC(1,I))
  230     CONTINUE
          DGGS=YTC(1,1)
          DVVS=YTC(2,2)
 
          XIG=SQRT(PYALPS(-TH)/ALPRHT)
C.........TH LOOP
          ZTC(1,1)=DCMPLX(TH)
          ZTC(2,2)=DCMPLX(TH-PMAS(PYCOMP(3100021),1)**2)
          ZTC(3,3)=DCMPLX(TH-PMAS(PYCOMP(3100113),1)**2)
          ZTC(4,4)=DCMPLX(TH-PMAS(PYCOMP(3400113),1)**2)
          ZTC(5,5)=DCMPLX(TH-PMAS(PYCOMP(3200113),1)**2)
          ZTC(6,6)=DCMPLX(TH-PMAS(PYCOMP(3300113),1)**2)
          ZTC(1,2)=(0D0,0D0)
          ZTC(1,3)=DCMPLX(TH*XIG,0D0)
          ZTC(1,4)=ZTC(1,3)
          ZTC(1,5)=ZTC(1,2)
          ZTC(1,6)=ZTC(1,2)
          ZTC(2,3)=DCMPLX(TH*XIG*X11,0D0)
          ZTC(2,4)=DCMPLX(TH*XIG*X22,0D0)
          ZTC(2,5)=DCMPLX(TH*XIG*X12,0D0)
          ZTC(2,6)=DCMPLX(TH*XIG*X21,0D0)
          ZTC(3,4)=-SM1122
          ZTC(3,5)=-SM1112
          ZTC(3,6)=-SM1121
          ZTC(4,5)=-SM2212
          ZTC(4,6)=-SM2221
          ZTC(5,6)=-SM1221
          DO 250 I=1,5
            DO 240 J=I+1,6
               ZTC(J,I)=ZTC(I,J)
  240       CONTINUE
  250     CONTINUE
          CALL PYLDCM(ZTC,6,6,INDX,D)
          DO 270 I=1,6
            DO 260 J=1,6
              YTC(I,J)=(0D0,0D0)
              IF(I.EQ.J) YTC(I,J)=(1D0,0D0)
  260       CONTINUE
  270     CONTINUE
          DO 280 I=1,6
            CALL PYBKSB(ZTC,6,6,INDX,YTC(1,I))
  280     CONTINUE
          DGGT=YTC(1,1)
          DVVT=YTC(2,2)
 
          XIG=SQRT(PYALPS(-UH)/ALPRHT)
C.........UH LOOP
          ZTC(1,1)=DCMPLX(UH,0D0)
          ZTC(2,2)=DCMPLX(UH-PMAS(PYCOMP(3100021),1)**2)
          ZTC(3,3)=DCMPLX(UH-PMAS(PYCOMP(3100113),1)**2)
          ZTC(4,4)=DCMPLX(UH-PMAS(PYCOMP(3400113),1)**2)
          ZTC(5,5)=DCMPLX(UH-PMAS(PYCOMP(3200113),1)**2)
          ZTC(6,6)=DCMPLX(UH-PMAS(PYCOMP(3300113),1)**2)
          ZTC(1,2)=(0D0,0D0)
          ZTC(1,3)=DCMPLX(UH*XIG,0D0)
          ZTC(1,4)=ZTC(1,3)
          ZTC(1,5)=ZTC(1,2)
          ZTC(1,6)=ZTC(1,2)
          ZTC(2,3)=DCMPLX(UH*XIG*X11,0D0)
          ZTC(2,4)=DCMPLX(UH*XIG*X22,0D0)
          ZTC(2,5)=DCMPLX(UH*XIG*X12,0D0)
          ZTC(2,6)=DCMPLX(UH*XIG*X21,0D0)
          ZTC(3,4)=-SM1122
          ZTC(3,5)=-SM1112
          ZTC(3,6)=-SM1121
          ZTC(4,5)=-SM2212
          ZTC(4,6)=-SM2221
          ZTC(5,6)=-SM1221
          DO 300 I=1,5
            DO 290 J=I+1,6
               ZTC(J,I)=ZTC(I,J)
  290       CONTINUE
  300     CONTINUE
          CALL PYLDCM(ZTC,6,6,INDX,D)
          DO 320 I=1,6
            DO 310 J=1,6
              YTC(I,J)=(0D0,0D0)
              IF(I.EQ.J) YTC(I,J)=(1D0,0D0)
  310       CONTINUE
  320     CONTINUE
          DO 330 I=1,6
            CALL PYBKSB(ZTC,6,6,INDX,YTC(1,I))
  330     CONTINUE
          DGGU=YTC(1,1)
          DVVU=YTC(2,2)
 
          IF(IMDL.EQ.1) THEN
            DQQS=DGGS+DVVS*DCMPLX(TANT3**2)
            DQQT=DGGT+DVVT*DCMPLX(TANT3**2)
            DQQU=DGGU+DVVU*DCMPLX(TANT3**2)
            DQTS=DGGS-DVVS
          ELSE
            DQQS=DGGS+DVVS*DCMPLX(1D0/TANT3**2)
            DQQT=DGGT+DVVT*DCMPLX(1D0/TANT3**2)
            DQQU=DGGU+DVVU*DCMPLX(1D0/TANT3**2)
            DQTS=DGGS+DVVS*DCMPLX(1D0/TANT3**2)
          ENDIF
 
          SQDQTS=ABS(DQTS)**2
          SQDQQS=ABS(DQQS)**2
          SQDQQT=ABS(DQQT)**2
          SQDQQU=ABS(DQQU)**2
          SQDLGS=ABS(DCMPLX(SH)*DGGS-DCMPLX(1D0))**2
          SQDLGT=ABS(DCMPLX(TH)*DGGT-DCMPLX(1D0))**2
 
          SQDGGS=ABS(DGGS)**2
          SQDGGT=ABS(DGGT)**2
          SQDGGU=ABS(DGGU)**2
          REDGGS=DBLE(DGGS)
          REDGGT=DBLE(DGGT)
          REDGGU=DBLE(DGGU)
          REDGTU=DBLE(DGGU*DCONJG(DGGT))
          REDGSU=DBLE(DGGU*DCONJG(DGGS))
          REDGST=DBLE(DGGS*DCONJG(DGGT))
          REDQST=DBLE(DQQS*DCONJG(DQQT))
          REDQTU=DBLE(DQQT*DCONJG(DQQU))
        ENDIF
      ENDIF
 
C...A: 2 -> 1, tree diagrams
 
      IF(ISUB.LE.10) THEN
        IF(ISUB.EQ.1) THEN
C...f + fbar -> gamma*/Z0
          MINT(61)=2
          CALL PYWIDT(23,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACZ=4D0*COMFAC*3D0
          HP0=AEM/3D0*SH
          HP1=AEM/3D0*XWC*SH
          DO 340 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 340
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            HI0=HP0
            IF(IABS(I).LE.10) HI0=HI0*FACA/3D0
            HI1=HP1
            IF(IABS(I).LE.10) HI1=HI1*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACZ*(EI**2/SH2*HI0*HP0*VINT(111)+
     &      EI*VI*(1D0-SQMZ/SH)/((SH-SQMZ)**2+HS**2)*
     &      (HI0*HP1+HI1*HP0)*VINT(112)+(VI**2+AI**2)/
     &      ((SH-SQMZ)**2+HS**2)*HI1*HP1*VINT(114))
  340     CONTINUE
 
        ELSEIF(ISUB.EQ.2) THEN
C...f + fbar' -> W+/-
          CALL PYWIDT(24,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMW)**2+HS**2)*3D0
          HP=AEM/(24D0*XW)*SH
          DO 360 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 360
            IA=IABS(I)
            DO 350 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 350
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 350
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 350
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP*2D0
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)*FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))
              SIGH(NCHN)=HI*FACBW*HF
  350       CONTINUE
  360     CONTINUE
 
        ELSEIF(ISUB.EQ.3) THEN
C...f + fbar -> h0 (or H0, or A0)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          HP=AEM/(8D0*XW)*SH/SQMW*SH
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          DO 370 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 370
            IA=IABS(I)
            RMQ=PYMRUN(IA,SH)**2/SH
            HI=HP*RMQ
            IF(IA.LE.10) HI=HP*RMQ*FACA/3D0
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              IKFI=1
              IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
              IF(IA.GT.10) IKFI=3
              HI=HI*PARU(150+10*IHIGG+IKFI)**2
              IF(IMSS(1).NE.0.AND.IA.EQ.5) THEN
                HI=HI/(1D0+RMSS(41))**2
                IF(IHIGG.NE.3) THEN
                  HI=HI*(1D0+RMSS(41)*PARU(152+10*IHIGG)/
     &            PARU(151+10*IHIGG))**2
                ENDIF
              ENDIF
            ENDIF
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=HI*FACBW*HF
  370     CONTINUE
 
        ELSEIF(ISUB.EQ.4) THEN
C...gamma + W+/- -> W+/-
 
        ELSEIF(ISUB.EQ.5) THEN
C...Z0 + Z0 -> h0
          CALL PYWIDT(25,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(25,1)).GT.PARP(48)*PMAS(25,2)) FACBW=0D0
          HP=AEM/(8D0*XW)*SH/SQMW*SH
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HI=HP/4D0
          FACI=8D0/(PARU(1)**2*XW1)*(AEM*XWC)**2
          DO 390 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 390
            DO 380 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 380
              EI=KCHG(IABS(I),1)/3D0
              AI=SIGN(1D0,EI)
              VI=AI-4D0*EI*XWV
              EJ=KCHG(IABS(J),1)/3D0
              AJ=SIGN(1D0,EJ)
              VJ=AJ-4D0*EJ*XWV
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACI*(VI**2+AI**2)*(VJ**2+AJ**2)*HI*FACBW*HF
  380       CONTINUE
  390     CONTINUE
 
        ELSEIF(ISUB.EQ.6) THEN
C...Z0 + W+/- -> W+/-
 
        ELSEIF(ISUB.EQ.7) THEN
C...W+ + W- -> Z0
 
        ELSEIF(ISUB.EQ.8) THEN
C...W+ + W- -> h0
          CALL PYWIDT(25,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(25,1)).GT.PARP(48)*PMAS(25,2)) FACBW=0D0
          HP=AEM/(8D0*XW)*SH/SQMW*SH
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HI=HP/2D0
          FACI=1D0/(4D0*PARU(1)**2)*(AEM/XW)**2
          DO 410 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 410
            EI=SIGN(1D0,DBLE(I))*KCHG(IABS(I),1)
            DO 400 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 400
              EJ=SIGN(1D0,DBLE(J))*KCHG(IABS(J),1)
              IF(EI*EJ.GT.0D0) GOTO 400
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACI*VINT(180+I)*VINT(180+J)*HI*FACBW*HF
  400       CONTINUE
  410     CONTINUE
 
C...B: 2 -> 2, tree diagrams
 
        ELSEIF(ISUB.EQ.10) THEN
C...f + f' -> f + f' (gamma/Z/W exchange)
          FACGGF=COMFAC*AEM**2*2D0*(SH2+UH2)/TH2
          FACGZF=COMFAC*AEM**2*XWC*4D0*SH2/(TH*(TH-SQMZ))
          FACZZF=COMFAC*(AEM*XWC)**2*2D0*SH2/(TH-SQMZ)**2
          FACWWF=COMFAC*(0.5D0*AEM/XW)**2*SH2/(TH-SQMW)**2
          DO 430 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 430
            IA=IABS(I)
            DO 420 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 420
              JA=IABS(J)
C...Electroweak couplings
              EI=KCHG(IA,1)*ISIGN(1,I)/3D0
              AI=SIGN(1D0,KCHG(IA,1)+0.5D0)*ISIGN(1,I)
              VI=AI-4D0*EI*XWV
              EJ=KCHG(JA,1)*ISIGN(1,J)/3D0
              AJ=SIGN(1D0,KCHG(JA,1)+0.5D0)*ISIGN(1,J)
              VJ=AJ-4D0*EJ*XWV
              EPSIJ=ISIGN(1,I*J)
C...gamma/Z exchange, only gamma exchange, or only Z exchange
              IF(MSTP(21).GE.1.AND.MSTP(21).LE.4) THEN
                IF(MSTP(21).EQ.1.OR.MSTP(21).EQ.4) THEN
                  FACNCF=FACGGF*EI**2*EJ**2+FACGZF*EI*EJ*
     &            (VI*VJ*(1D0+UH2/SH2)+AI*AJ*EPSIJ*(1D0-UH2/SH2))+
     &            FACZZF*((VI**2+AI**2)*(VJ**2+AJ**2)*(1D0+UH2/SH2)+
     &            4D0*VI*VJ*AI*AJ*EPSIJ*(1D0-UH2/SH2))
                ELSEIF(MSTP(21).EQ.2) THEN
                  FACNCF=FACGGF*EI**2*EJ**2
                ELSE
                  FACNCF=FACZZF*((VI**2+AI**2)*(VJ**2+AJ**2)*
     &            (1D0+UH2/SH2)+4D0*VI*VJ*AI*AJ*EPSIJ*(1D0-UH2/SH2))
                ENDIF
C...Extrafactor 2 for only one incoming neutrino spin state.
                IF(IA.GT.10.AND.MOD(IA,2).EQ.0) FACNCF=2D0*FACNCF
                IF(JA.GT.10.AND.MOD(JA,2).EQ.0) FACNCF=2D0*FACNCF
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                SIGH(NCHN)=FACNCF
              ENDIF
C...W exchange
              IF((MSTP(21).EQ.1.OR.MSTP(21).EQ.5).AND.AI*AJ.LT.0D0) THEN
                FACCCF=FACWWF*VINT(180+I)*VINT(180+J)
                IF(EPSIJ.LT.0D0) FACCCF=FACCCF*UH2/SH2
                IF(IA.GT.10.AND.MOD(IA,2).EQ.0) FACCCF=2D0*FACCCF
                IF(JA.GT.10.AND.MOD(JA,2).EQ.0) FACCCF=2D0*FACCCF
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                SIGH(NCHN)=FACCCF
              ENDIF
  420       CONTINUE
  430     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.20) THEN
        IF(ISUB.EQ.11) THEN
C...f + f' -> f + f' (g exchange)
          FACQQ1=COMFAC*AS**2*4D0/9D0*(SH2+UH2)*SQDQQT
          FACQQB=COMFAC*AS**2*4D0/9D0*((SH2+UH2)*SQDQQT*FACA-
     &    MSTP(34)*2D0/3D0*UH2*REDQST)
          FACQQ2=COMFAC*AS**2*4D0/9D0*(SH2+TH2)*SQDQQU
          FACQQI=-COMFAC*AS**2*4D0/9D0*MSTP(34)*2D0/3D0*SH2/(TH*UH)
          RATQQI=(FACQQ1+FACQQ2+FACQQI)/(FACQQ1+FACQQ2)
          IF(MSTP(5).GE.1.AND.MSTP(5).LE.4) THEN
C...Modifications from contact interactions (compositeness)
            FACCI1=FACQQ1+COMFAC*(SH2/PARU(155)**4)
            FACCIB=FACQQB+COMFAC*(8D0/9D0)*(AS*PARU(156)/PARU(155)**2)*
     &      (UH2/TH+UH2/SH)+COMFAC*(5D0/3D0)*(UH2/PARU(155)**4)
            FACCI2=FACQQ2+COMFAC*(8D0/9D0)*(AS*PARU(156)/PARU(155)**2)*
     &      (SH2/TH+SH2/UH)+COMFAC*(5D0/3D0)*(SH2/PARU(155)**4)
            FACCI3=FACQQ1+COMFAC*(UH2/PARU(155)**4)
            RATCII=(FACCI1*FACCI2+FACQQI)/(FACCI1+FACCI2)
          ELSEIF(MSTP(5).EQ.5) THEN
            FACCI1=FACQQ1
            FACCIB=FACQQB
            FACCI2=FACQQ2
            FACCI3=FACQQ1
            RATCII=1D0
          ENDIF
          DO 450 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.MSTP(58).OR.KFAC(1,I).EQ.0) GOTO 450
            DO 440 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.MSTP(58).OR.KFAC(2,J).EQ.0) GOTO 440
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.(IA.GE.3.OR.
     &        JA.GE.3))) THEN
                SIGH(NCHN)=FACQQ1
                IF(I.EQ.-J) SIGH(NCHN)=FACQQB
              ELSE
                SIGH(NCHN)=FACCI1
                IF(I*J.LT.0) SIGH(NCHN)=FACCI3
                IF(I.EQ.-J) SIGH(NCHN)=FACCIB
              ENDIF
              IF(I.EQ.J) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.IA.GE.3)) THEN
                  SIGH(NCHN-1)=0.5D0*FACQQ1*RATQQI
                  SIGH(NCHN)=0.5D0*FACQQ2*RATQQI
                ELSE
                  SIGH(NCHN-1)=0.5D0*FACCI1*RATCII
                  SIGH(NCHN)=0.5D0*FACCI2*RATCII
                ENDIF
              ENDIF
  440       CONTINUE
  450     CONTINUE
 
        ELSEIF(ISUB.EQ.12) THEN
C...f + fbar -> f' + fbar' (q + qbar -> q' + qbar' only)
          CALL PYWIDT(21,SH,WDTP,WDTE)
C.........Do not use for b bbar in Standard TC2
          FACQQB=COMFAC*AS**2*4D0/9D0*(TH2+UH2)*SQDQQS*
     &    (WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          IF(MSTP(5).EQ.1) THEN
C...Modifications from contact interactions (compositeness)
            FACCIB=FACQQB
            DO 460 I=1,2
              FACCIB=FACCIB+COMFAC*(UH2/PARU(155)**4)*(WDTE(I,1)+
     &        WDTE(I,2)+WDTE(I,4))
  460       CONTINUE
          ELSEIF(MSTP(5).GE.2.AND.MSTP(5).LE.4) THEN
            FACCIB=FACQQB+COMFAC*(UH2/PARU(155)**4)*
     &      (WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          ENDIF
          DO 470 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 470
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.IABS(I).GE.3)) THEN
              SIGH(NCHN)=FACQQB
            ELSE
              SIGH(NCHN)=FACCIB
            ENDIF
  470     CONTINUE
 
        ELSEIF(ISUB.EQ.13) THEN
C...f + fbar -> g + g (q + qbar -> g + g only)
          FACGG1=COMFAC*AS**2*32D0/27D0*(UH/TH-(2D0+MSTP(34)*1D0/4D0)*
     &    UH2/SH2+9D0/4D0*TH*UH/SH2*SQDLGS)
          FACGG2=COMFAC*AS**2*32D0/27D0*(TH/UH-(2D0+MSTP(34)*1D0/4D0)*
     &    TH2/SH2+9D0/4D0*TH*UH/SH2*SQDLGS)
          DO 480 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 480
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=0.5D0*FACGG1
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=2
            SIGH(NCHN)=0.5D0*FACGG2
  480     CONTINUE
 
        ELSEIF(ISUB.EQ.14) THEN
C...f + fbar -> g + gamma (q + qbar -> g + gamma only)
          FACGG=COMFAC*AS*AEM*8D0/9D0*(TH2+UH2)/(TH*UH)
          DO 490 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 490
            EI=KCHG(IABS(I),1)/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACGG*EI**2
  490     CONTINUE
 
        ELSEIF(ISUB.EQ.15) THEN
C...f + fbar -> g + (gamma*/Z0) (q + qbar -> g + (gamma*/Z0) only)
          FACZG=COMFAC*AS*AEM*(8D0/9D0)*(TH2+UH2+2D0*SQM4*SH)/(TH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs
          HFGG=0D0
          HFGZ=0D0
          HFZZ=0D0
          RADC4=1D0+PYALPS(SQM4)/PARU(1)
          DO 500 I=1,MIN(16,MDCY(23,3))
            IDC=I+MDCY(23,2)-1
            IF(MDME(IDC,1).LT.0) GOTO 500
            IMDM=0
            IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &      IMDM=1
            IF(I.LE.8) THEN
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ELSEIF(I.LE.16) THEN
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC4
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.EQ.1) THEN
                HFGG=HFGG+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HFGZ=HFGZ+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HFZZ=HFZZ+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
  500     CONTINUE
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=(1D0/PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM4,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          HFGG=HFGG*HFAEM*VINT(111)/SQM4
          HFGZ=HFGZ*HFAEM*VINT(112)/SQM4
          HFZZ=HFZZ*HFAEM*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure
          DO 510 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 510
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACZG*(EI**2*HFGG+EI*VI*HFGZ+
     &      (VI**2+AI**2)*HFZZ)/HBW4
  510     CONTINUE
 
        ELSEIF(ISUB.EQ.16) THEN
C...f + fbar' -> g + W+/- (q + qbar' -> g + W+/- only)
          FACWG=COMFAC*AS*AEM/XW*2D0/9D0*(TH2+UH2+2D0*SQM4*SH)/(TH*UH)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM4,WDTP,WDTE)
          GMMWC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
          FACWG=FACWG*HBW4C/HBW4
          DO 530 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.10.OR.KFAC(1,I).EQ.0) GOTO 530
            DO 520 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.10.OR.KFAC(2,J).EQ.0) GOTO 520
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 520
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
              FCKM=VCKM((IA+1)/2,(JA+1)/2)
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWG*FCKM*WIDSC
  520       CONTINUE
  530     CONTINUE
 
        ELSEIF(ISUB.EQ.17) THEN
C...f + fbar -> g + h0 (q + qbar -> g + h0 only)
 
        ELSEIF(ISUB.EQ.18) THEN
C...f + fbar -> gamma + gamma
          FACGG=COMFAC*AEM**2*2D0*(TH2+UH2)/(TH*UH)
          DO 540 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 540
            EI=KCHG(IABS(I),1)/3D0
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=0.5D0*FACGG*FCOI*EI**4
  540     CONTINUE
 
        ELSEIF(ISUB.EQ.19) THEN
C...f + fbar -> gamma + (gamma*/Z0)
          FACGZ=COMFAC*2D0*AEM**2*(TH2+UH2+2D0*SQM4*SH)/(TH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs
          HFGG=0D0
          HFGZ=0D0
          HFZZ=0D0
          RADC4=1D0+PYALPS(SQM4)/PARU(1)
          DO 550 I=1,MIN(16,MDCY(23,3))
            IDC=I+MDCY(23,2)-1
            IF(MDME(IDC,1).LT.0) GOTO 550
            IMDM=0
            IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &      IMDM=1
            IF(I.LE.8) THEN
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ELSEIF(I.LE.16) THEN
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC4
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.EQ.1) THEN
                HFGG=HFGG+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HFGZ=HFGZ+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HFZZ=HFZZ+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
  550     CONTINUE
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=(1D0/PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM4,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          HFGG=HFGG*HFAEM*VINT(111)/SQM4
          HFGZ=HFGZ*HFAEM*VINT(112)/SQM4
          HFZZ=HFZZ*HFAEM*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure
          DO 560 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 560
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACGZ*FCOI*EI**2*(EI**2*HFGG+EI*VI*HFGZ+
     &      (VI**2+AI**2)*HFZZ)/HBW4
  560     CONTINUE
 
        ELSEIF(ISUB.EQ.20) THEN
C...f + fbar' -> gamma + W+/-
          FACGW=COMFAC*0.5D0*AEM**2/XW
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM4,WDTP,WDTE)
          GMMWC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
          FACGW=FACGW*HBW4C/HBW4
C...Anomalous couplings
          TERM1=(TH2+UH2+2D0*SQM4*SH)/(TH*UH)
          TERM2=0D0
          TERM3=0D0
          IF(MSTP(5).GE.1.AND.MSTP(5).LE.4) THEN
            TERM2=PARU(153)*(TH-UH)/(TH+UH)
            TERM3=0.5D0*PARU(153)**2*(TH*UH+(TH2+UH2)*SH/
     &      (4D0*SQMW))/(TH+UH)**2
          ENDIF
          DO 580 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 580
            DO 570 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(2,J).EQ.0) GOTO 570
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 570
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 570
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
              IF(IA.LE.10) THEN
                FACWR=UH/(TH+UH)-1D0/3D0
                FCKM=VCKM((IA+1)/2,(JA+1)/2)
                FCOI=FACA/3D0
              ELSE
                FACWR=-TH/(TH+UH)
                FCKM=1D0
                FCOI=1D0
              ENDIF
              FACWK=TERM1*FACWR**2+TERM2*FACWR+TERM3
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGW*FACWK*FCOI*FCKM*WIDSC
  570       CONTINUE
  580     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.30) THEN
        IF(ISUB.EQ.21) THEN
C...f + fbar -> gamma + h0
 
        ELSEIF(ISUB.EQ.22) THEN
C...f + fbar -> (gamma*/Z0) + (gamma*/Z0)
C...Kinematics dependence
          FACZZ=COMFAC*AEM**2*((TH2+UH2+2D0*(SQM3+SQM4)*SH)/(TH*UH)-
     &    SQM3*SQM4*(1D0/TH2+1D0/UH2))
C...gamma, gamma/Z interference and Z couplings to final fermion pairs
          DO 600 I=1,6
            DO 590 J=1,3
              HGZ(I,J)=0D0
  590       CONTINUE
  600     CONTINUE
          RADC3=1D0+PYALPS(SQM3)/PARU(1)
          RADC4=1D0+PYALPS(SQM4)/PARU(1)
          DO 610 I=1,MIN(16,MDCY(23,3))
            IDC=I+MDCY(23,2)-1
            IF(MDME(IDC,1).LT.0) GOTO 610
            IMDM=0
            IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2) IMDM=1
            IF(MDME(IDC,1).EQ.4.OR.MDME(IDC,1).EQ.5) IMDM=MDME(IDC,1)-2
            IF(I.LE.8) THEN
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ELSEIF(I.LE.16) THEN
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM3
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC3
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.GE.1) THEN
                HGZ(1,IMDM)=HGZ(1,IMDM)+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HGZ(2,IMDM)=HGZ(2,IMDM)+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HGZ(3,IMDM)=HGZ(3,IMDM)+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC4
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.GE.1) THEN
                HGZ(4,IMDM)=HGZ(4,IMDM)+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HGZ(5,IMDM)=HGZ(5,IMDM)+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HGZ(6,IMDM)=HGZ(6,IMDM)+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
  610     CONTINUE
C...Propagators: as simulated in PYOFSH and as desired
          HBW3=(1D0/PARU(1))*GMMZ/((SQM3-SQMZ)**2+GMMZ**2)
          HBW4=(1D0/PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM3,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          DO 620 J=1,3
            HGZ(1,J)=HGZ(1,J)*HFAEM*VINT(111)/SQM3
            HGZ(2,J)=HGZ(2,J)*HFAEM*VINT(112)/SQM3
            HGZ(3,J)=HGZ(3,J)*HFAEM*VINT(114)/SQM3
  620     CONTINUE
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM4,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          DO 630 J=1,3
            HGZ(4,J)=HGZ(4,J)*HFAEM*VINT(111)/SQM4
            HGZ(5,J)=HGZ(5,J)*HFAEM*VINT(112)/SQM4
            HGZ(6,J)=HGZ(6,J)*HFAEM*VINT(114)/SQM4
  630     CONTINUE
C...Loop over flavours; separate left- and right-handed couplings
          DO 650 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 650
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            VALI=VI-AI
            VARI=VI+AI
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            DO 640 J=1,3
              HL3(J)=EI**2*HGZ(1,J)+EI*VALI*HGZ(2,J)+VALI**2*HGZ(3,J)
              HR3(J)=EI**2*HGZ(1,J)+EI*VARI*HGZ(2,J)+VARI**2*HGZ(3,J)
              HL4(J)=EI**2*HGZ(4,J)+EI*VALI*HGZ(5,J)+VALI**2*HGZ(6,J)
              HR4(J)=EI**2*HGZ(4,J)+EI*VARI*HGZ(5,J)+VARI**2*HGZ(6,J)
  640       CONTINUE
            FACLR=HL3(1)*HL4(1)+HL3(1)*(HL4(2)+HL4(3))+
     &      HL4(1)*(HL3(2)+HL3(3))+HL3(2)*HL4(3)+HL4(2)*HL3(3)+
     &      HR3(1)*HR4(1)+HR3(1)*(HR4(2)+HR4(3))+
     &      HR4(1)*(HR3(2)+HR3(3))+HR3(2)*HR4(3)+HR4(2)*HR3(3)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=0.5D0*FACZZ*FCOI*FACLR/(HBW3*HBW4)
  650     CONTINUE
 
        ELSEIF(ISUB.EQ.23) THEN
C...f + fbar' -> Z0 + W+/- (Z0 only, i.e. no gamma* admixture.)
          FACZW=COMFAC*0.5D0*(AEM/XW)**2
          FACZW=FACZW*WIDS(23,2)
          THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
          FACBW=1D0/((SH-SQMW)**2+GMMW**2)
          DO 670 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 670
            DO 660 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(2,J).EQ.0) GOTO 660
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 660
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 660
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              EI=KCHG(IA,1)/3D0
              AI=SIGN(1D0,EI+0.1D0)
              VI=AI-4D0*EI*XWV
              EJ=KCHG(JA,1)/3D0
              AJ=SIGN(1D0,EJ+0.1D0)
              VJ=AJ-4D0*EJ*XWV
              IF(VI+AI.GT.0) THEN
                VISAV=VI
                AISAV=AI
                VI=VJ
                AI=AJ
                VJ=VISAV
                AJ=AISAV
              ENDIF
              FCKM=1D0
              IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
              FCOI=1D0
              IF(IA.LE.10) FCOI=FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZW*FCOI*FCKM*(FACBW*((9D0-8D0*XW)/4D0*THUH+
     &        (8D0*XW-6D0)/4D0*SH*(SQM3+SQM4))+(THUH-SH*(SQM3+SQM4))*
     &        (SH-SQMW)*FACBW*0.5D0*((VJ+AJ)/TH-(VI+AI)/UH)+
     &        THUH/(16D0*XW1)*((VJ+AJ)**2/TH2+(VI+AI)**2/UH2)+
     &        SH*(SQM3+SQM4)/(8D0*XW1)*(VI+AI)*(VJ+AJ)/(TH*UH))*
     &        WIDS(24,(5-KCHW)/2)
C***Protect against slightly negative cross sections. (Reason yet to be
C***sorted out. One possibility: addition of width to the W propagator.)
              SIGH(NCHN)=MAX(0D0,SIGH(NCHN))
  660       CONTINUE
  670     CONTINUE
 
        ELSEIF(ISUB.EQ.24) THEN
C...f + fbar -> Z0 + h0 (or H0, or A0)
          THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
          FACHZ=COMFAC*8D0*(AEM*XWC)**2*
     &    (THUH+2D0*SH*SQM3)/((SH-SQMZ)**2+GMMZ**2)
          FACHZ=FACHZ*WIDS(23,2)*WIDS(KFHIGG,2)
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACHZ=FACHZ*
     &    PARU(154+10*IHIGG)**2
          DO 680 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 680
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACHZ*FCOI*(VI**2+AI**2)
  680     CONTINUE
 
        ELSEIF(ISUB.EQ.25) THEN
C...f + fbar -> W+ + W-
C...Propagators: Z0, W+- as simulated in PYOFSH and as desired
          GMMZC=GMMZ
          HBWZC=SH**2/((SH-SQMZ)**2+GMMZC**2)
          HBW3=GMMW/((SQM3-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM3,WDTP,WDTE)
          GMMW3=SQRT(SQM3)*WDTP(0)
          HBW3C=GMMW3/((SQM3-SQMW)**2+GMMW3**2)
          HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM4,WDTP,WDTE)
          GMMW4=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMW4/((SQM4-SQMW)**2+GMMW4**2)
C...Kinematical functions
          THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
          THUH34=(2D0*SH*(SQM3+SQM4)+THUH)/(SQM3*SQM4)
          GS=(((SH-SQM3-SQM4)**2-4D0*SQM3*SQM4)*THUH34+12D0*THUH)/SH2
          GT=THUH34+4D0*THUH/TH2
          GST=((SH-SQM3-SQM4)*THUH34+4D0*(SH*(SQM3+SQM4)-THUH)/TH)/SH
          GU=THUH34+4D0*THUH/UH2
          GSU=((SH-SQM3-SQM4)*THUH34+4D0*(SH*(SQM3+SQM4)-THUH)/UH)/SH
C...Common factors and couplings
          FACWW=COMFAC*(HBW3C/HBW3)*(HBW4C/HBW4)
          FACWW=FACWW*WIDS(24,1)
          CGG=AEM**2/2D0
          CGZ=AEM**2/(4D0*XW)*HBWZC*(1D0-SQMZ/SH)
          CZZ=AEM**2/(32D0*XW**2)*HBWZC
          CNG=AEM**2/(4D0*XW)
          CNZ=AEM**2/(16D0*XW**2)*HBWZC*(1D0-SQMZ/SH)
          CNN=AEM**2/(16D0*XW**2)
C...Coulomb factor for W+W- pair
          IF(MSTP(40).GE.1.AND.MSTP(40).LE.3) THEN
            COULE=(SH-4D0*SQMW)/(4D0*PMAS(24,1))
            COULP=MAX(1D-10,0.5D0*BE34*SQRT(SH))
            IF(COULE.LT.100D0*PMAS(24,2)) THEN
              COULP1=SQRT(0.5D0*PMAS(24,1)*(SQRT(COULE**2+
     &        PMAS(24,2)**2)-COULE))
            ELSE
              COULP1=SQRT(0.5D0*PMAS(24,1)*(0.5D0*PMAS(24,2)**2/COULE))
            ENDIF
            IF(COULE.GT.-100D0*PMAS(24,2)) THEN
              COULP2=SQRT(0.5D0*PMAS(24,1)*(SQRT(COULE**2+
     &        PMAS(24,2)**2)+COULE))
            ELSE
              COULP2=SQRT(0.5D0*PMAS(24,1)*(0.5D0*PMAS(24,2)**2/
     &        ABS(COULE)))
            ENDIF
            IF(MSTP(40).EQ.1) THEN
              COULDC=PARU(1)-2D0*ATAN((COULP1**2+COULP2**2-COULP**2)/
     &        MAX(1D-10,2D0*COULP*COULP1))
              FACCOU=1D0+0.5D0*PARU(101)*COULDC/MAX(1D-5,BE34)
            ELSEIF(MSTP(40).EQ.2) THEN
              COULCK=DCMPLX(DBLE(COULP1),DBLE(COULP2))
              COULCP=DCMPLX(0D0,DBLE(COULP))
              COULCD=(COULCK+COULCP)/(COULCK-COULCP)
              COULCR=1D0+DBLE(PARU(101)*SQRT(SH))/
     &        (4D0*COULCP)*LOG(COULCD)
              COULCS=DCMPLX(0D0,0D0)
              NSTP=100
              DO 690 ISTP=1,NSTP
                COULXX=(ISTP-0.5)/NSTP
                COULCS=COULCS+(1D0/COULXX)*LOG((1D0+COULXX*COULCD)/
     &          (1D0+COULXX/COULCD))
  690         CONTINUE
              COULCR=COULCR+DBLE(PARU(101)**2*SH)/(16D0*COULCP*COULCK)*
     &        (COULCS/NSTP)
              FACCOU=ABS(COULCR)**2
            ELSEIF(MSTP(40).EQ.3) THEN
              COULDC=PARU(1)-2D0*(1D0-BE34)**2*ATAN((COULP1**2+
     &        COULP2**2-COULP**2)/MAX(1D-10,2D0*COULP*COULP1))
              FACCOU=1D0+0.5D0*PARU(101)*COULDC/MAX(1D-5,BE34)
            ENDIF
          ELSEIF(MSTP(40).EQ.4) THEN
            FACCOU=1D0+0.5D0*PARU(101)*PARU(1)/MAX(1D-5,BE34)
          ELSE
            FACCOU=1D0
          ENDIF
          VINT(95)=FACCOU
          FACWW=FACWW*FACCOU
C...Loop over allowed flavours
          DO 700 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 700
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            IF(MSTP(50).LE.0.OR.IABS(I).LE.10) THEN
              IF(AI.LT.0D0) THEN
                DSIGWW=(CGG*EI**2+CGZ*VI*EI+CZZ*(VI**2+AI**2))*GS+
     &          (CNG*EI+CNZ*(VI+AI))*GST+CNN*GT
              ELSE
                DSIGWW=(CGG*EI**2+CGZ*VI*EI+CZZ*(VI**2+AI**2))*GS-
     &          (CNG*EI+CNZ*(VI+AI))*GSU+CNN*GU
              ENDIF
            ELSE
              XMW02=0.5D0*(SQM3+SQM4)-0.25D0*(SQM3-SQM4)**2/SH
              BET=SQRT(1D0-4D0*XMW02/SH)
              GAT=1D0/SQRT(1D0-BET**2)
              STHE2=1D0-CTH**2
              AMPZG=BET**3*(16D0+(4D0*BET**2*GAT**2+3D0/GAT**2)*STHE2)
              AMPNU=BET*(2D0+BET**2*GAT**2*STHE2/2D0+
     &        2D0*BET**2*(1D0-BET**2)*STHE2/(1D0-2D0*BET*CTH+BET**2)**2)
              AMPNG=BET*((1D0+BET**2)*(4D0+BET**2*GAT**2*STHE2)+
     &        2D0*(1D0-BET**2)*(BET**2*STHE2-2D0*(1D0-BET**2))/
     &        (1D0-2D0*BET*CTH+BET**2))
              PROPI1=(0.25D0*SQMZ/XMW02)*HBWZC*(1D0-SQMZ/SH)
              PROPI2=(0.25D0*SQMZ/XMW02)**2*HBWZC
              A0=(2D0*(XMW02/SQMZ)-(1D0-BET**2)*XW)*POLL
              A1=(2D0*(XMW02/SQMZ)**2-2*XMW02/SQMZ*(1D0-BET**2)*XW)*POLL
              A2=(1D0-BET**2)**2*XW**2*(POLR+POLL)/2D0
              ATOT=AMPNU*POLL+(A1+A2)*PROPI2*AMPZG-A0*PROPI1*AMPNG
              ATOT=ATOT*CNN/SQMW*SH/BET*2D0
              DSIGWW=ATOT
            ENDIF
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACWW*FCOI*DSIGWW
  700     CONTINUE
 
        ELSEIF(ISUB.EQ.26) THEN
C...f + fbar' -> W+/- + h0 (or H0, or A0)
          THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
          FACHW=COMFAC*0.125D0*(AEM/XW)**2*(THUH+2D0*SH*SQM3)/
     &    ((SH-SQMW)**2+GMMW**2)
          FACHW=FACHW*WIDS(KFHIGG,2)
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACHW=FACHW*
     &    PARU(155+10*IHIGG)**2
          DO 720 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 720
            DO 710 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(1,J).EQ.0) GOTO 710
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 710
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 710
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              FCKM=1D0
              IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
              FCOI=1D0
              IF(IA.LE.10) FCOI=FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACHW*FCOI*FCKM*WIDS(24,(5-KCHW)/2)
  710       CONTINUE
  720     CONTINUE
 
        ELSEIF(ISUB.EQ.27) THEN
C...f + fbar -> h0 + h0
 
        ELSEIF(ISUB.EQ.28) THEN
C...f + g -> f + g (q + g -> q + g only)
          FACQG1=COMFAC*AS**2*4D0/9D0*((2D0+MSTP(34)*1D0/4D0)*UH2/TH2-
     &    UH/SH-9D0/4D0*SH*UH/TH2*SQDLGT)*FACA
          FACQG2=COMFAC*AS**2*4D0/9D0*((2D0+MSTP(34)*1D0/4D0)*SH2/TH2-
     &    SH/UH-9D0/4D0*SH*UH/TH2*SQDLGT)
          DO 740 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.10) GOTO 740
            DO 730 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 730
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 730
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACQG1
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=2
              SIGH(NCHN)=FACQG2
  730       CONTINUE
  740     CONTINUE
 
        ELSEIF(ISUB.EQ.29) THEN
C...f + g -> f + gamma (q + g -> q + gamma only)
          FGQ=COMFAC*FACA*AS*AEM*1D0/3D0*(SH2+UH2)/(-SH*UH)
          DO 760 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 760
            EI=KCHG(IABS(I),1)/3D0
            FACGQ=FGQ*EI**2
            DO 750 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 750
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 750
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGQ
  750       CONTINUE
  760     CONTINUE
 
        ELSEIF(ISUB.EQ.30) THEN
C...f + g -> f + (gamma*/Z0) (q + g -> q + (gamma*/Z0) only)
          FZQ=COMFAC*FACA*AS*AEM*(1D0/3D0)*(SH2+UH2+2D0*SQM4*TH)/
     &    (-SH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs
          HFGG=0D0
          HFGZ=0D0
          HFZZ=0D0
          RADC4=1D0+PYALPS(SQM4)/PARU(1)
          DO 770 I=1,MIN(16,MDCY(23,3))
            IDC=I+MDCY(23,2)-1
            IF(MDME(IDC,1).LT.0) GOTO 770
            IMDM=0
            IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &      IMDM=1
            IF(I.LE.8) THEN
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ELSEIF(I.LE.16) THEN
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC4
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.EQ.1) THEN
                HFGG=HFGG+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HFGZ=HFGZ+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HFZZ=HFZZ+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
  770     CONTINUE
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=(1D0/PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM4,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          HFGG=HFGG*HFAEM*VINT(111)/SQM4
          HFGZ=HFGZ*HFAEM*VINT(112)/SQM4
          HFZZ=HFZZ*HFAEM*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure
          DO 790 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 790
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            FACZQ=FZQ*(EI**2*HFGG+EI*VI*HFGZ+
     &      (VI**2+AI**2)*HFZZ)/HBW4
            DO 780 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 780
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 780
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZQ
  780       CONTINUE
  790     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.40) THEN
        IF(ISUB.EQ.31) THEN
C...f + g -> f' + W+/- (q + g -> q' + W+/- only)
          FACWQ=COMFAC*FACA*AS*AEM/XW*1D0/12D0*
     &    (SH2+UH2+2D0*SQM4*TH)/(-SH*UH)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM4,WDTP,WDTE)
          GMMWC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
          FACWQ=FACWQ*HBW4C/HBW4
          DO 810 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 810
            IA=IABS(I)
            KCHW=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
            WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
            DO 800 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 800
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 800
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWQ*VINT(180+I)*WIDSC
  800       CONTINUE
  810     CONTINUE
 
        ELSEIF(ISUB.EQ.32) THEN
C...f + g -> f + h0 (q + g -> q + h0 only)
          SQMHC=PMAS(25,1)**2
          FHCQ=COMFAC*FACA*AS*AEM/XW*1D0/24D0
          DO 830 I=MMINA,MMAXA
            IA=IABS(I)
            IF(IA.NE.5) GOTO 830
            SQML=PMAS(IA,1)**2
            IF(IA.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) SQML=SQML*
     &      (LOG(MAX(4D0,PARP(37)**2*SQML/PARU(117)**2))/
     &      LOG(MAX(4D0,SH/PARU(117)**2)))**(24D0/(33D0-2D0*MSTU(118)))
            IUA=IA+MOD(IA,2)
            SQMQ=SQML
            FACHCQ=FHCQ*SQML/SQMW*
     &      (SH/(SQMQ-UH)+2D0*SQMQ*(SQMHC-UH)/(SQMQ-UH)**2+(SQMQ-UH)/SH+
     &      2D0*SQMQ/(SQMQ-UH)+2D0*(SQMHC-UH)/(SQMQ-UH)*
     &      (SQMHC-SQMQ-SH)/SH)
            KCHHC=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
            DO 820 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 820
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,1).EQ.0) GOTO 820
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACHCQ*WIDS(37,(5-KCHHC)/2)
  820       CONTINUE
  830     CONTINUE
 
        ELSEIF(ISUB.EQ.33) THEN
C...f + gamma -> f + g (q + gamma -> q + g only)
          FGQ=COMFAC*AS*AEM*8D0/3D0*(SH2+UH2)/(-SH*UH)
          DO 850 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 850
            EI=KCHG(IABS(I),1)/3D0
            FACGQ=FGQ*EI**2
            DO 840 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 840
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 840
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGQ
  840       CONTINUE
  850     CONTINUE
 
        ELSEIF(ISUB.EQ.34) THEN
C...f + gamma -> f + gamma
          FGQ=COMFAC*AEM**2*2D0*(SH2+UH2)/(-SH*UH)
         DO 870 I=MMINA,MMAXA
            IF(I.EQ.0) GOTO 870
            EI=KCHG(IABS(I),1)/3D0
            FACGQ=FGQ*EI**4
            DO 860 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 860
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 860
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGQ
  860       CONTINUE
  870     CONTINUE
 
        ELSEIF(ISUB.EQ.35) THEN
C...f + gamma -> f + (gamma*/Z0)
          IF(MINT(15).EQ.22.AND.VINT(3).LT.0D0) THEN
            FZQN=SH2+UH2+2D0*(SQM4-VINT(3)**2)*TH
            FZQDTM=VINT(3)**2*SQM4-SH*(UH-VINT(4)**2)
          ELSEIF(MINT(16).EQ.22.AND.VINT(4).LT.0D0) THEN
            FZQN=SH2+UH2+2D0*(SQM4-VINT(4)**2)*TH
            FZQDTM=VINT(4)**2*SQM4-SH*(UH-VINT(3)**2)
          ELSE
            FZQN=SH2+UH2+2D0*SQM4*TH
            FZQDTM=-SH*UH
          ENDIF
          FZQN=COMFAC*2D0*AEM**2*MAX(0D0,FZQN)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs
          HFGG=0D0
          HFGZ=0D0
          HFZZ=0D0
          RADC4=1D0+PYALPS(SQM4)/PARU(1)
          DO 880 I=1,MIN(16,MDCY(23,3))
            IDC=I+MDCY(23,2)-1
            IF(MDME(IDC,1).LT.0) GOTO 880
            IMDM=0
            IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &      IMDM=1
            IF(I.LE.8) THEN
              EF=KCHG(I,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ELSEIF(I.LE.16) THEN
              EF=KCHG(I+2,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*XWV
            ENDIF
            RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
            IF(4D0*RM1.LT.1D0) THEN
              FCOF=1D0
              IF(I.LE.8) FCOF=3D0*RADC4
              BE34=SQRT(MAX(0D0,1D0-4D0*RM1))
              IF(IMDM.EQ.1) THEN
                HFGG=HFGG+FCOF*EF**2*(1D0+2D0*RM1)*BE34
                HFGZ=HFGZ+FCOF*EF*VF*(1D0+2D0*RM1)*BE34
                HFZZ=HFZZ+FCOF*(VF**2*(1D0+2D0*RM1)+
     &          AF**2*(1D0-4D0*RM1))*BE34
              ENDIF
            ENDIF
  880     CONTINUE
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=(1D0/PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
          MINT15=MINT(15)
          MINT(15)=1
          MINT(61)=1
          CALL PYWIDT(23,SQM4,WDTP,WDTE)
          MINT(15)=MINT15
          HFAEM=(PARU(108)/PARU(2))*(2D0/3D0)
          HFGG=HFGG*HFAEM*VINT(111)/SQM4
          HFGZ=HFGZ*HFAEM*VINT(112)/SQM4
          HFZZ=HFZZ*HFAEM*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure
          DO 900 I=MMINA,MMAXA
            IF(I.EQ.0) GOTO 900
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            FACZQ=EI**2*(EI**2*HFGG+EI*VI*HFGZ+
     &      (VI**2+AI**2)*HFZZ)/HBW4
            FZQD=MAX(PMAS(IABS(I),1)**2*SQM4,FZQDTM)
            DO 890 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 890
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 890
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZQ*FZQN/FZQD
  890       CONTINUE
  900     CONTINUE
 
        ELSEIF(ISUB.EQ.36) THEN
C...f + gamma -> f' + W+/-
          FWQ=COMFAC*AEM**2/(2D0*XW)*
     &    (SH2+UH2+2D0*SQM4*TH)/(SQPTH*SQM4-SH*UH)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
          CALL PYWIDT(24,SQM4,WDTP,WDTE)
          GMMWC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
          FWQ=FWQ*HBW4C/HBW4
          DO 920 I=MMINA,MMAXA
            IF(I.EQ.0) GOTO 920
            IA=IABS(I)
            EIA=ABS(KCHG(IABS(I),1)/3D0)
            FACWQ=FWQ*(EIA-SH/(SH+UH))**2
            KCHW=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
            WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
            DO 910 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 910
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 910
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWQ*VINT(180+I)*WIDSC
  910       CONTINUE
  920     CONTINUE
 
        ELSEIF(ISUB.EQ.37) THEN
C...f + gamma -> f + h0
 
        ELSEIF(ISUB.EQ.38) THEN
C...f + Z0 -> f + g (q + Z0 -> q + g only)
 
        ELSEIF(ISUB.EQ.39) THEN
C...f + Z0 -> f + gamma
 
        ELSEIF(ISUB.EQ.40) THEN
C...f + Z0 -> f + Z0
        ENDIF
 
      ELSEIF(ISUB.LE.50) THEN
        IF(ISUB.EQ.41) THEN
C...f + Z0 -> f' + W+/-
 
        ELSEIF(ISUB.EQ.42) THEN
C...f + Z0 -> f + h0
 
        ELSEIF(ISUB.EQ.43) THEN
C...f + W+/- -> f' + g (q + W+/- -> q' + g only)
 
        ELSEIF(ISUB.EQ.44) THEN
C...f + W+/- -> f' + gamma
 
        ELSEIF(ISUB.EQ.45) THEN
C...f + W+/- -> f' + Z0
 
        ELSEIF(ISUB.EQ.46) THEN
C...f + W+/- -> f' + W+/-
 
        ELSEIF(ISUB.EQ.47) THEN
C...f + W+/- -> f' + h0
 
        ELSEIF(ISUB.EQ.48) THEN
C...f + h0 -> f + g (q + h0 -> q + g only)
 
        ELSEIF(ISUB.EQ.49) THEN
C...f + h0 -> f + gamma
 
        ELSEIF(ISUB.EQ.50) THEN
C...f + h0 -> f + Z0
        ENDIF
 
      ELSEIF(ISUB.LE.60) THEN
        IF(ISUB.EQ.51) THEN
C...f + h0 -> f' + W+/-
 
        ELSEIF(ISUB.EQ.52) THEN
C...f + h0 -> f + h0
 
        ELSEIF(ISUB.EQ.53) THEN
C...g + g -> f + fbar (g + g -> q + qbar only)
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 940
          IDC0=MDCY(21,2)-1
C...Begin by d, u, s flavours.
          FLAVWT=0D0
          IF(MDME(IDC0+1,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(1,1)**2/SH))
          IF(MDME(IDC0+2,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(2,1)**2/SH))
          IF(MDME(IDC0+3,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(3,1)**2/SH))
          FACQQ1=COMFAC*AS**2*1D0/6D0*(UH/TH-(2D0+MSTP(34)*1D0/4D0)*
     &    UH2/SH2+9D0/4D0*TH*UH/SH2*SQDLGS)*FLAVWT*FACA
          FACQQ2=COMFAC*AS**2*1D0/6D0*(TH/UH-(2D0+MSTP(34)*1D0/4D0)*
     &    TH2/SH2+9D0/4D0*TH*UH/SH2*SQDLGS)*FLAVWT*FACA
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQQ2
C...Next c and b flavours: modified that and uhat for fixed
C...cos(theta-hat).
          DO 930 IFL=4,5
          SQMAVG=PMAS(IFL,1)**2
          IF(MDME(IDC0+IFL,1).GE.1.AND.SH.GT.4.04D0*SQMAVG) THEN
            BE34=SQRT(1D0-4D0*SQMAVG/SH)
            THQ=-0.5D0*SH*(1D0-BE34*CTH)
            UHQ=-0.5D0*SH*(1D0+BE34*CTH)
            THUHQ=THQ*UHQ-SQMAVG*SH
            IF(MSTP(34).EQ.0) THEN
              FACQQ1=UHQ/THQ-2D0*UHQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/THQ**2
              FACQQ2=THQ/UHQ-2D0*THQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/UHQ**2
            ELSE
              FACQQ1=UHQ/THQ-2.25D0*UHQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &        THQ**2+0.5D0*SQMAVG*(THQ+SQMAVG)/THQ**2-SQMAVG**2/(SH*THQ)
              FACQQ2=THQ/UHQ-2.25D0*THQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &        UHQ**2+0.5D0*SQMAVG*(UHQ+SQMAVG)/UHQ**2-SQMAVG**2/(SH*UHQ)
            ENDIF
            IF(MSTP(5).GE.5) THEN
              FACQQ1=FACQQ1+2.25D0*SQMAVG*(THQ-UHQ)/(SH*THQ)*REDGGS+
     &        2.25D0*THQ*UHQ/SH2*SQDLGS
              FACQQ2=FACQQ2+2.25D0*SQMAVG*(UHQ-THQ)/(SH*UHQ)*REDGGS+
     &        2.25D0*THQ*UHQ/SH2*SQDLGS
            ENDIF
            FACQQ1=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ1*BE34
            FACQQ2=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ2*BE34
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1+2*(IFL-3)
            SIGH(NCHN)=FACQQ1
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=2+2*(IFL-3)
            SIGH(NCHN)=FACQQ2
          ENDIF
  930     CONTINUE
  940     CONTINUE
 
        ELSEIF(ISUB.EQ.54) THEN
C...g + gamma -> f + fbar (g + gamma -> q + qbar only)
          CALL PYWIDT(21,SH,WDTP,WDTE)
          WDTESU=0D0
          DO 950 I=1,MIN(8,MDCY(21,3))
            EF=KCHG(I,1)/3D0
            WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+
     &      WDTE(I,4))
  950     CONTINUE
          FACQQ=COMFAC*AEM*AS*WDTESU*(TH2+UH2)/(TH*UH)
          IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
          IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
 
        ELSEIF(ISUB.EQ.55) THEN
C...g + Z -> f + fbar (g + Z -> q + qbar only)
 
        ELSEIF(ISUB.EQ.56) THEN
C...g + W -> f + f'bar (g + W -> q + q'bar only)
 
        ELSEIF(ISUB.EQ.57) THEN
C...g + h0 -> f + fbar (g + h0 -> q + qbar only)
 
        ELSEIF(ISUB.EQ.58) THEN
C...gamma + gamma -> f + fbar
          CALL PYWIDT(22,SH,WDTP,WDTE)
          WDTESU=0D0
          DO 960 I=1,MIN(12,MDCY(22,3))
            IF(I.LE.8) EF= KCHG(I,1)/3D0
            IF(I.GE.9) EF= KCHG(9+2*(I-8),1)/3D0
            WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+
     &      WDTE(I,4))
  960     CONTINUE
          FACFF=COMFAC*AEM**2*WDTESU*2D0*(TH2+UH2)/(TH*UH)
          IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACFF
          ENDIF
 
        ELSEIF(ISUB.EQ.59) THEN
C...gamma + Z0 -> f + fbar
 
        ELSEIF(ISUB.EQ.60) THEN
C...gamma + W+/- -> f + fbar'
        ENDIF
 
      ELSEIF(ISUB.LE.70) THEN
        IF(ISUB.EQ.61) THEN
C...gamma + h0 -> f + fbar
 
        ELSEIF(ISUB.EQ.62) THEN
C...Z0 + Z0 -> f + fbar
 
        ELSEIF(ISUB.EQ.63) THEN
C...Z0 + W+/- -> f + fbar'
 
        ELSEIF(ISUB.EQ.64) THEN
C...Z0 + h0 -> f + fbar
 
        ELSEIF(ISUB.EQ.65) THEN
C...W+ + W- -> f + fbar
 
        ELSEIF(ISUB.EQ.66) THEN
C...W+/- + h0 -> f + fbar'
 
        ELSEIF(ISUB.EQ.67) THEN
C...h0 + h0 -> f + fbar
 
        ELSEIF(ISUB.EQ.68) THEN
C...g + g -> g + g
          IF(MSTP(5).LE.4) THEN
            FACGG1=COMFAC*AS**2*9D0/4D0*(SH2/TH2+2D0*SH/TH+3D0+
     &      2D0*TH/SH+TH2/SH2)*FACA
            FACGG2=COMFAC*AS**2*9D0/4D0*(UH2/SH2+2D0*UH/SH+3D0+
     &      2D0*SH/UH+SH2/UH2)*FACA
            FACGG3=COMFAC*AS**2*9D0/4D0*(TH2/UH2+2D0*TH/UH+3D0+
     &      2D0*UH/TH+UH2/TH2)
          ELSE
            GST=  (12D0 + 40D0*TH/SH + 56D0*TH2/SH2 + 32D0*TH**3/SH**3 +
     &      16D0*TH**4/SH**4 + SQDGGS*(4D0*SH2 + 16D0*SH*TH + 16D0*TH2)+
     &      4D0*REDGST*(SH + 2D0*TH)*
     &      (2D0*SH**3 - 3D0*SH2*TH - 2D0*SH*TH2 + 2D0*TH**3)/SH2 +
     &      2D0*REDGGS*(2D0*SH - 12D0*TH2/SH - 8D0*TH**3/SH2) +
     &      2D0*REDGGT*(4D0*SH - 22D0*TH - 68D0*TH2/SH - 60D0*TH**3/SH2-
     &      32D0*TH**4/SH**3 - 16D0*TH**5/SH**4) +
     &      SQDGGT*(16D0*SH2 + 16D0*SH*TH + 68D0*TH2 + 144D0*TH**3/SH +
     &      96D0*TH**4/SH2 + 32D0*TH**5/SH**3 + 16D0*TH**6/SH**4))/16D0
            GSU=  (12D0 + 40D0*UH/SH + 56D0*UH2/SH2 + 32D0*UH**3/SH**3 +
     &      16D0*UH**4/SH**4 + SQDGGS*(4D0*SH2 + 16D0*SH*UH + 16D0*UH2)+
     &      4D0*REDGSU*(SH + 2D0*UH)*
     &      (2D0*SH**3 - 3D0*SH2*UH - 2D0*SH*UH2 + 2D0*UH**3)/SH2 +
     &      2D0*REDGGS*(2D0*SH - 12D0*UH2/SH - 8D0*UH**3/SH2) +
     &      2D0*REDGGU*(4D0*SH - 22D0*UH - 68D0*UH2/SH - 60D0*UH**3/SH2-
     &      32D0*UH**4/SH**3 - 16D0*UH**5/SH**4) +
     &      SQDGGU*(16D0*SH2 + 16D0*SH*UH + 68D0*UH2 + 144D0*UH**3/SH +
     &      96D0*UH**4/SH2 + 32D0*UH**5/SH**3 + 16D0*UH**6/SH**4))/16D0
            GUT=  (12D0 - 16D0*TH*(TH - UH)**2*UH/SH**4 +
     &      4D0*REDGGU*(2D0*TH**5 - 15D0*TH**4*UH - 48D0*TH**3*UH2 -
     &      58D0*TH2*UH**3 - 10D0*TH*UH**4 + UH**5)/SH**4 +
     &      4D0*REDGGT*(TH**5 - 10D0*TH**4*UH - 58D0*TH**3*UH2 -
     &      48D0*TH2*UH**3 - 15D0*TH*UH**4 + 2D0*UH**5)/SH**4 +
     &      4D0*SQDGGU*(4D0*TH**6 + 20D0*TH**5*UH + 57D0*TH**4*UH2 +
     &      72D0*TH**3*UH**3+ 38D0*TH2*UH**4+4D0*TH*UH**5 +UH**6)/SH**4+
     &      4D0*SQDGGT*(4D0*UH**6 + 4D0*TH**5*UH + 38D0*TH**4*UH2 +
     &      72D0*TH**3*UH**3 +57D0*TH2*UH**4+20D0*TH*UH**5+TH**6)/SH**4+
     &      2D0*REDGTU*((TH - UH)**2* (TH**4 + 20D0*TH**3*UH +
     &      30D0*TH2*UH2 + 20D0*TH*UH**3 + UH**4) +
     &      SH2*(7D0*TH**4 + 52D0*TH**3*UH + 274D0*TH2*UH2 +
     &      52D0*TH*UH**3 + 7D0*UH**4))/(2D0*SH**4))/16D0
            FACGG1=COMFAC*AS**2*9D0/4D0*GST*FACA
            FACGG2=COMFAC*AS**2*9D0/4D0*GSU*FACA
            FACGG3=COMFAC*AS**2*9D0/4D0*GUT
          ENDIF
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 970
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=0.5D0*FACGG1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=0.5D0*FACGG2
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=3
          SIGH(NCHN)=0.5D0*FACGG3
  970     CONTINUE
 
        ELSEIF(ISUB.EQ.69) THEN
C...gamma + gamma -> W+ + W-
          SQMWE=MAX(0.5D0*SQMW,SQRT(SQM3*SQM4))
          FPROP=SH2/((SQMWE-TH)*(SQMWE-UH))
          FACWW=COMFAC*6D0*AEM**2*(1D0-FPROP*(4D0/3D0+2D0*SQMWE/SH)+
     &    FPROP**2*(2D0/3D0+2D0*(SQMWE/SH)**2))*WIDS(24,1)
          IF(KFAC(1,22)*KFAC(2,22).EQ.0) GOTO 980
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACWW
  980     CONTINUE
 
        ELSEIF(ISUB.EQ.70) THEN
C...gamma + W+/- -> Z0 + W+/-
          SQMWE=MAX(0.5D0*SQMW,SQRT(SQM3*SQM4))
          FPROP=(TH-SQMWE)**2/(-SH*(SQMWE-UH))
          FACZW=COMFAC*6D0*AEM**2*(XW1/XW)*
     &    (1D0-FPROP*(4D0/3D0+2D0*SQMWE/(TH-SQMWE))+
     &    FPROP**2*(2D0/3D0+2D0*(SQMWE/(TH-SQMWE))**2))*WIDS(23,2)
          DO 1000 KCHW=1,-1,-2
            DO 990 ISDE=1,2
              IF(KFAC(ISDE,22)*KFAC(3-ISDE,24*KCHW).EQ.0) GOTO 990
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=22
              ISIG(NCHN,3-ISDE)=24*KCHW
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZW*WIDS(24,(5-KCHW)/2)
  990       CONTINUE
 1000     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.80) THEN
        IF(ISUB.EQ.71) THEN
C...Z0 + Z0 -> Z0 + Z0
          IF(SH.LE.4.01D0*SQMZ) GOTO 1030
 
          IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons
            BE2=1D0-4D0*SQMZ/SH
            TH=-0.5D0*SH*BE2*(1D0-CTH)
            UH=-0.5D0*SH*BE2*(1D0+CTH)
            IF(MAX(TH,UH).GT.-1D0) GOTO 1030
            SHANG=1D0/XW1*SQMW/SQMZ*(1D0+BE2)**2
            ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
            ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
            THANG=1D0/XW1*SQMW/SQMZ*(BE2-CTH)**2
            ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
            ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
            UHANG=1D0/XW1*SQMW/SQMZ*(BE2+CTH)**2
            AUHRE=(UH-SQMH)/((UH-SQMH)**2+GMMH**2)*UHANG
            AUHIM=-GMMH/((UH-SQMH)**2+GMMH**2)*UHANG
            FACZZ=COMFAC*1D0/(4096D0*PARU(1)**2*16D0*XW1**2)*
     &      (AEM/XW)**4*(SH/SQMW)**2*(SQMZ/SQMW)*SH2
            IF(MSTP(46).LE.0) FACZZ=FACZZ*(ASHRE**2+ASHIM**2)
            IF(MSTP(46).EQ.1) FACZZ=FACZZ*((ASHRE+ATHRE+AUHRE)**2+
     &      (ASHIM+ATHIM+AUHIM)**2)
            IF(MSTP(46).EQ.2) FACZZ=0D0
 
          ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
            FACZZ=COMFAC*(AEM/(16D0*PARU(1)*XW*XW1))**2*(64D0/9D0)*
     &      ABS(A00U+2D0*A20U)**2
          ENDIF
          FACZZ=FACZZ*WIDS(23,1)
 
          DO 1020 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1020
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            AVI=AI**2+VI**2
            DO 1010 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1010
              EJ=KCHG(IABS(J),1)/3D0
              AJ=SIGN(1D0,EJ)
              VJ=AJ-4D0*EJ*XWV
              AVJ=AJ**2+VJ**2
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=0.5D0*FACZZ*AVI*AVJ
 1010       CONTINUE
 1020     CONTINUE
 1030     CONTINUE
 
        ELSEIF(ISUB.EQ.72) THEN
C...Z0 + Z0 -> W+ + W-
          IF(SH.LE.4.01D0*SQMZ) GOTO 1060
 
          IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons
            BE2=SQRT((1D0-4D0*SQMW/SH)*(1D0-4D0*SQMZ/SH))
            CTH2=CTH**2
            TH=-0.5D0*SH*(1D0-2D0*(SQMW+SQMZ)/SH-BE2*CTH)
            UH=-0.5D0*SH*(1D0-2D0*(SQMW+SQMZ)/SH+BE2*CTH)
            IF(MAX(TH,UH).GT.-1D0) GOTO 1060
            SHANG=4D0*SQRT(SQMW/(SQMZ*XW1))*(1D0-2D0*SQMW/SH)*
     &      (1D0-2D0*SQMZ/SH)
            ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
            ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
            ATWRE=XW1/SQMZ*SH/(TH-SQMW)*((CTH-BE2)**2*(3D0/2D0+BE2/2D0*
     &      CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4D0*
     &      ((SQMW+SQMZ)/SH*(1D0-3D0*CTH2)+8D0*SQMW*SQMZ/SH2*
     &      (2D0*CTH2-1D0)+4D0*(SQMW**2+SQMZ**2)/SH2*CTH2+
     &      2D0*(SQMW+SQMZ)/SH*BE2*CTH))
            ATWIM=0D0
            AUWRE=XW1/SQMZ*SH/(UH-SQMW)*((CTH+BE2)**2*(3D0/2D0-BE2/2D0*
     &      CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4D0*
     &      ((SQMW+SQMZ)/SH*(1D0-3D0*CTH2)+8D0*SQMW*SQMZ/SH2*
     &      (2D0*CTH2-1D0)+4D0*(SQMW**2+SQMZ**2)/SH2*CTH2-
     &      2D0*(SQMW+SQMZ)/SH*BE2*CTH))
            AUWIM=0D0
            A4RE=2D0*XW1/SQMZ*(3D0-CTH2-4D0*(SQMW+SQMZ)/SH)
            A4IM=0D0
            FACWW=COMFAC*1D0/(4096D0*PARU(1)**2*16D0*XW1**2)*
     &      (AEM/XW)**4*(SH/SQMW)**2*(SQMZ/SQMW)*SH2
            IF(MSTP(46).LE.0) FACWW=FACWW*(ASHRE**2+ASHIM**2)
            IF(MSTP(46).EQ.1) FACWW=FACWW*((ASHRE+ATWRE+AUWRE+A4RE)**2+
     &      (ASHIM+ATWIM+AUWIM+A4IM)**2)
            IF(MSTP(46).EQ.2) FACWW=FACWW*((ATWRE+AUWRE+A4RE)**2+
     &      (ATWIM+AUWIM+A4IM)**2)
 
          ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
            FACWW=COMFAC*(AEM/(16D0*PARU(1)*XW*XW1))**2*(64D0/9D0)*
     &      ABS(A00U-A20U)**2
          ENDIF
          FACWW=FACWW*WIDS(24,1)
 
          DO 1050 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1050
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            AVI=AI**2+VI**2
            DO 1040 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1040
              EJ=KCHG(IABS(J),1)/3D0
              AJ=SIGN(1D0,EJ)
              VJ=AJ-4D0*EJ*XWV
              AVJ=AJ**2+VJ**2
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWW*AVI*AVJ
 1040       CONTINUE
 1050     CONTINUE
 1060     CONTINUE
 
        ELSEIF(ISUB.EQ.73) THEN
C...Z0 + W+/- -> Z0 + W+/-
          IF(SH.LE.2D0*SQMZ+2D0*SQMW) GOTO 1090
 
          IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons
            BE2=1D0-2D0*(SQMZ+SQMW)/SH+((SQMZ-SQMW)/SH)**2
            EP1=1D0-(SQMZ-SQMW)/SH
            EP2=1D0+(SQMZ-SQMW)/SH
            TH=-0.5D0*SH*BE2*(1D0-CTH)
            UH=(SQMZ-SQMW)**2/SH-0.5D0*SH*BE2*(1D0+CTH)
            IF(MAX(TH,UH).GT.-1D0) GOTO 1090
            THANG=(BE2-EP1*CTH)*(BE2-EP2*CTH)
            ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
            ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
            ASWRE=-XW1/SQMZ*SH/(SH-SQMW)*(-BE2*(EP1+EP2)**4*CTH+
     &      1D0/4D0*(BE2+EP1*EP2)**2*((EP1-EP2)**2-4D0*BE2*CTH)+
     &      2D0*BE2*(BE2+EP1*EP2)*(EP1+EP2)**2*CTH-
     &      1D0/16D0*SH/SQMW*(EP1**2-EP2**2)**2*(BE2+EP1*EP2)**2)
            ASWIM=0D0
            AUWRE=XW1/SQMZ*SH/(UH-SQMW)*(-BE2*(EP2+EP1*CTH)*
     &      (EP1+EP2*CTH)*(BE2+EP1*EP2)+BE2*(EP2+EP1*CTH)*
     &      (BE2+EP1*EP2*CTH)*(2D0*EP2-EP2*CTH+EP1)-
     &      BE2*(EP2+EP1*CTH)**2*(BE2-EP2**2*CTH)-1D0/8D0*
     &      (BE2+EP1*EP2*CTH)**2*((EP1+EP2)**2+2D0*BE2*(1D0-CTH))+
     &      1D0/32D0*SH/SQMW*(BE2+EP1*EP2*CTH)**2*
     &      (EP1**2-EP2**2)**2-BE2*(EP1+EP2*CTH)*(EP2+EP1*CTH)*
     &      (BE2+EP1*EP2)+BE2*(EP1+EP2*CTH)*(BE2+EP1*EP2*CTH)*
     &      (2D0*EP1-EP1*CTH+EP2)-BE2*(EP1+EP2*CTH)**2*
     &      (BE2-EP1**2*CTH)-1D0/8D0*(BE2+EP1*EP2*CTH)**2*
     &      ((EP1+EP2)**2+2D0*BE2*(1D0-CTH))+1D0/32D0*SH/SQMW*
     &      (BE2+EP1*EP2*CTH)**2*(EP1**2-EP2**2)**2)
            AUWIM=0D0
            A4RE=XW1/SQMZ*(EP1**2*EP2**2*(CTH**2-1D0)-
     &      2D0*BE2*(EP1**2+EP2**2+EP1*EP2)*CTH-2D0*BE2*EP1*EP2)
            A4IM=0D0
            FACZW=COMFAC*1D0/(4096D0*PARU(1)**2*4D0*XW1)*(AEM/XW)**4*
     &      (SH/SQMW)**2*SQRT(SQMZ/SQMW)*SH2
            IF(MSTP(46).LE.0) FACZW=0D0
            IF(MSTP(46).EQ.1) FACZW=FACZW*((ATHRE+ASWRE+AUWRE+A4RE)**2+
     &      (ATHIM+ASWIM+AUWIM+A4IM)**2)
            IF(MSTP(46).EQ.2) FACZW=FACZW*((ASWRE+AUWRE+A4RE)**2+
     &      (ASWIM+AUWIM+A4IM)**2)
 
          ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
            FACZW=COMFAC*AEM**2/(64D0*PARU(1)**2*XW**2*XW1)*16D0*
     &      ABS(A20U+3D0*A11U*DBLE(CTH))**2
          ENDIF
          FACZW=FACZW*WIDS(23,2)
 
          DO 1080 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1080
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            AVI=AI**2+VI**2
            KCHWI=ISIGN(1,KCHG(IABS(I),1)*ISIGN(1,I))
            DO 1070 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1070
              EJ=KCHG(IABS(J),1)/3D0
              AJ=SIGN(1D0,EJ)
              VJ=AI-4D0*EJ*XWV
              AVJ=AJ**2+VJ**2
              KCHWJ=ISIGN(1,KCHG(IABS(J),1)*ISIGN(1,J))
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZW*AVI*VINT(180+J)*WIDS(24,(5-KCHWJ)/2)
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=2
              SIGH(NCHN)=FACZW*VINT(180+I)*WIDS(24,(5-KCHWI)/2)*AVJ
 1070       CONTINUE
 1080     CONTINUE
 1090     CONTINUE
 
        ELSEIF(ISUB.EQ.75) THEN
C...W+ + W- -> gamma + gamma
 
        ELSEIF(ISUB.EQ.76) THEN
C...W+ + W- -> Z0 + Z0
          IF(SH.LE.4.01D0*SQMZ) GOTO 1120
 
          IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons
            BE2=SQRT((1D0-4D0*SQMW/SH)*(1D0-4D0*SQMZ/SH))
            CTH2=CTH**2
            TH=-0.5D0*SH*(1D0-2D0*(SQMW+SQMZ)/SH-BE2*CTH)
            UH=-0.5D0*SH*(1D0-2D0*(SQMW+SQMZ)/SH+BE2*CTH)
            IF(MAX(TH,UH).GT.-1D0) GOTO 1120
            SHANG=4D0*SQRT(SQMW/(SQMZ*XW1))*(1D0-2D0*SQMW/SH)*
     &      (1D0-2D0*SQMZ/SH)
            ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
            ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
            ATWRE=XW1/SQMZ*SH/(TH-SQMW)*((CTH-BE2)**2*(3D0/2D0+BE2/2D0*
     &      CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4D0*
     &      ((SQMW+SQMZ)/SH*(1D0-3D0*CTH2)+8D0*SQMW*SQMZ/SH2*
     &      (2D0*CTH2-1D0)+4D0*(SQMW**2+SQMZ**2)/SH2*CTH2+
     &      2D0*(SQMW+SQMZ)/SH*BE2*CTH))
            ATWIM=0D0
            AUWRE=XW1/SQMZ*SH/(UH-SQMW)*((CTH+BE2)**2*(3D0/2D0-BE2/2D0*
     &      CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4D0*
     &      ((SQMW+SQMZ)/SH*(1D0-3D0*CTH2)+8D0*SQMW*SQMZ/SH2*
     &      (2D0*CTH2-1D0)+4D0*(SQMW**2+SQMZ**2)/SH2*CTH2-
     &      2D0*(SQMW+SQMZ)/SH*BE2*CTH))
            AUWIM=0D0
            A4RE=2D0*XW1/SQMZ*(3D0-CTH2-4D0*(SQMW+SQMZ)/SH)
            A4IM=0D0
            FACZZ=COMFAC*1D0/(4096D0*PARU(1)**2)*(AEM/XW)**4*
     &      (SH/SQMW)**2*SH2
            IF(MSTP(46).LE.0) FACZZ=FACZZ*(ASHRE**2+ASHIM**2)
            IF(MSTP(46).EQ.1) FACZZ=FACZZ*((ASHRE+ATWRE+AUWRE+A4RE)**2+
     &      (ASHIM+ATWIM+AUWIM+A4IM)**2)
            IF(MSTP(46).EQ.2) FACZZ=FACZZ*((ATWRE+AUWRE+A4RE)**2+
     &      (ATWIM+AUWIM+A4IM)**2)
 
          ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
            FACZZ=COMFAC*(AEM/(4D0*PARU(1)*XW))**2*(64D0/9D0)*
     &      ABS(A00U-A20U)**2
          ENDIF
          FACZZ=FACZZ*WIDS(23,1)
 
          DO 1110 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1110
            EI=SIGN(1D0,DBLE(I))*KCHG(IABS(I),1)
            DO 1100 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1100
              EJ=SIGN(1D0,DBLE(J))*KCHG(IABS(J),1)
              IF(EI*EJ.GT.0D0) GOTO 1100
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=0.5D0*FACZZ*VINT(180+I)*VINT(180+J)
 1100       CONTINUE
 1110     CONTINUE
 1120     CONTINUE
 
        ELSEIF(ISUB.EQ.77) THEN
C...W+/- + W+/- -> W+/- + W+/-
          IF(SH.LE.4.01D0*SQMW) GOTO 1150
 
          IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons
            BE2=1D0-4D0*SQMW/SH
            BE4=BE2**2
            CTH2=CTH**2
            CTH3=CTH**3
            TH=-0.5D0*SH*BE2*(1D0-CTH)
            UH=-0.5D0*SH*BE2*(1D0+CTH)
            IF(MAX(TH,UH).GT.-1D0) GOTO 1150
            SHANG=(1D0+BE2)**2
            ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
            ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
            THANG=(BE2-CTH)**2
            ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
            ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
            UHANG=(BE2+CTH)**2
            AUHRE=(UH-SQMH)/((UH-SQMH)**2+GMMH**2)*UHANG
            AUHIM=-GMMH/((UH-SQMH)**2+GMMH**2)*UHANG
            SGZANG=1D0/SQMW*BE2*(3D0-BE2)**2*CTH
            ASGRE=XW*SGZANG
            ASGIM=0D0
            ASZRE=XW1*SH/(SH-SQMZ)*SGZANG
            ASZIM=0D0
            TGZANG=1D0/SQMW*(BE2*(4D0-2D0*BE2+BE4)+BE2*(4D0-10D0*BE2+
     &      BE4)*CTH+(2D0-11D0*BE2+10D0*BE4)*CTH2+BE2*CTH3)
            ATGRE=0.5D0*XW*SH/TH*TGZANG
            ATGIM=0D0
            ATZRE=0.5D0*XW1*SH/(TH-SQMZ)*TGZANG
            ATZIM=0D0
            UGZANG=1D0/SQMW*(BE2*(4D0-2D0*BE2+BE4)-BE2*(4D0-10D0*BE2+
     &      BE4)*CTH+(2D0-11D0*BE2+10D0*BE4)*CTH2-BE2*CTH3)
            AUGRE=0.5D0*XW*SH/UH*UGZANG
            AUGIM=0D0
            AUZRE=0.5D0*XW1*SH/(UH-SQMZ)*UGZANG
            AUZIM=0D0
            A4ARE=1D0/SQMW*(1D0+2D0*BE2-6D0*BE2*CTH-CTH2)
            A4AIM=0D0
            A4SRE=2D0/SQMW*(1D0+2D0*BE2-CTH2)
            A4SIM=0D0
            FWW=COMFAC*1D0/(4096D0*PARU(1)**2)*(AEM/XW)**4*
     &      (SH/SQMW)**2*SH2
            IF(MSTP(46).LE.0) THEN
              AWWARE=ASHRE
              AWWAIM=ASHIM
              AWWSRE=0D0
              AWWSIM=0D0
            ELSEIF(MSTP(46).EQ.1) THEN
              AWWARE=ASHRE+ATHRE+ASGRE+ASZRE+ATGRE+ATZRE+A4ARE
              AWWAIM=ASHIM+ATHIM+ASGIM+ASZIM+ATGIM+ATZIM+A4AIM
              AWWSRE=-ATHRE-AUHRE+ATGRE+ATZRE+AUGRE+AUZRE+A4SRE
              AWWSIM=-ATHIM-AUHIM+ATGIM+ATZIM+AUGIM+AUZIM+A4SIM
            ELSE
              AWWARE=ASGRE+ASZRE+ATGRE+ATZRE+A4ARE
              AWWAIM=ASGIM+ASZIM+ATGIM+ATZIM+A4AIM
              AWWSRE=ATGRE+ATZRE+AUGRE+AUZRE+A4SRE
              AWWSIM=ATGIM+ATZIM+AUGIM+AUZIM+A4SIM
            ENDIF
            AWWA2=AWWARE**2+AWWAIM**2
            AWWS2=AWWSRE**2+AWWSIM**2
 
          ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron
            FWWA=COMFAC*(AEM/(4D0*PARU(1)*XW))**2*(64D0/9D0)*
     &      ABS(A00U+0.5D0*A20U+4.5D0*A11U*DBLE(CTH))**2
            FWWS=COMFAC*(AEM/(4D0*PARU(1)*XW))**2*64D0*ABS(A20U)**2
          ENDIF
 
          DO 1140 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1140
            EI=SIGN(1D0,DBLE(I))*KCHG(IABS(I),1)
            DO 1130 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1130
              EJ=SIGN(1D0,DBLE(J))*KCHG(IABS(J),1)
              IF(EI*EJ.LT.0D0) THEN
C...W+W-
                IF(MSTP(45).EQ.1) GOTO 1130
                IF(MSTP(46).LE.2) FACWW=FWW*AWWA2*WIDS(24,1)
                IF(MSTP(46).GE.3) FACWW=FWWA*WIDS(24,1)
              ELSE
C...W+W+/W-W-
                IF(MSTP(45).EQ.2) GOTO 1130
                IF(MSTP(46).LE.2) FACWW=FWW*AWWS2
                IF(MSTP(46).GE.3) FACWW=FWWS
                IF(EI.GT.0D0) FACWW=FACWW*WIDS(24,4)
                IF(EI.LT.0D0) FACWW=FACWW*WIDS(24,5)
              ENDIF
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWW*VINT(180+I)*VINT(180+J)
              IF(EI*EJ.GT.0D0) SIGH(NCHN)=0.5D0*SIGH(NCHN)
 1130       CONTINUE
 1140     CONTINUE
 1150     CONTINUE
 
        ELSEIF(ISUB.EQ.78) THEN
C...W+/- + h0 -> W+/- + h0
 
        ELSEIF(ISUB.EQ.79) THEN
C...h0 + h0 -> h0 + h0
 
        ELSEIF(ISUB.EQ.80) THEN
C...q + gamma -> q' + pi+/-
          FQPI=COMFAC*(2D0*AEM/9D0)*(-SH/TH)*(1D0/SH2+1D0/TH2)
          ASSH=PYALPS(MAX(0.5D0,0.5D0*SH))
          Q2FPSH=0.55D0/LOG(MAX(2D0,2D0*SH))
          DELSH=UH*SQRT(ASSH*Q2FPSH)
          ASUH=PYALPS(MAX(0.5D0,-0.5D0*UH))
          Q2FPUH=0.55D0/LOG(MAX(2D0,-2D0*UH))
          DELUH=SH*SQRT(ASUH*Q2FPUH)
          DO 1170 I=MAX(-2,MMINA),MIN(2,MMAXA)
            IF(I.EQ.0) GOTO 1170
            EI=KCHG(IABS(I),1)/3D0
            EJ=SIGN(1D0-ABS(EI),EI)
            DO 1160 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 1160
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 1160
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FQPI*(EI*DELSH+EJ*DELUH)**2
 1160       CONTINUE
 1170     CONTINUE
 
        ENDIF
 
C...C: 2 -> 2, tree diagrams with masses
 
      ELSEIF(ISUB.LE.90) THEN
        IF(ISUB.EQ.81) THEN
C...q + qbar -> Q + Qbar
          SQMAVG=0.5D0*(SQM3+SQM4)-0.25D0*(SQM3-SQM4)**2/SH
          THQ=-0.5D0*SH*(1D0-BE34*CTH)
          UHQ=-0.5D0*SH*(1D0+BE34*CTH)
          FACQQB=COMFAC*AS**2*4D0/9D0*((THQ**2+UHQ**2)/SH2+
     &    2D0*SQMAVG/SH)
          IF(MSTP(5).GE.5) FACQQB=FACQQB*SH2*SQDQTS
          IF(MSTP(35).GE.1) FACQQB=FACQQB*PYHFTH(SH,SQMAVG,0D0)
          WID2=1D0
          IF(MINT(55).EQ.6) WID2=WIDS(6,1)
          IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=WIDS(MINT(55),1)
          FACQQB=FACQQB*WID2
          DO 1180 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1180
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQB
 1180     CONTINUE
 
        ELSEIF(ISUB.EQ.82) THEN
C...g + g -> Q + Qbar
          SQMAVG=0.5D0*(SQM3+SQM4)-0.25D0*(SQM3-SQM4)**2/SH
          THQ=-0.5D0*SH*(1D0-BE34*CTH)
          UHQ=-0.5D0*SH*(1D0+BE34*CTH)
          THUHQ=THQ*UHQ-SQMAVG*SH
          IF(MSTP(34).EQ.0) THEN
            FACQQ1=UHQ/THQ-2D0*UHQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/THQ**2
            FACQQ2=THQ/UHQ-2D0*THQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/UHQ**2
          ELSE
            FACQQ1=UHQ/THQ-2.25D0*UHQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &      THQ**2+0.5D0*SQMAVG*(THQ+SQMAVG)/THQ**2-SQMAVG**2/(SH*THQ)
            FACQQ2=THQ/UHQ-2.25D0*THQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &      UHQ**2+0.5D0*SQMAVG*(UHQ+SQMAVG)/UHQ**2-SQMAVG**2/(SH*UHQ)
          ENDIF
          IF(MSTP(5).GE.5) THEN
            FACQQ1=FACQQ1+2.25D0*SQMAVG*(THQ-UHQ)/(SH*THQ)*REDGGS+
     &      2.25D0*THQ*UHQ/SH2*SQDLGS
            FACQQ2=FACQQ2+2.25D0*SQMAVG*(UHQ-THQ)/(SH*UHQ)*REDGGS+
     &      2.25D0*THQ*UHQ/SH2*SQDLGS
          ENDIF
          FACQQ1=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ1
          FACQQ2=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ2
          IF(MSTP(35).GE.1) THEN
            FATRE=PYHFTH(SH,SQMAVG,2D0/7D0)
            FACQQ1=FACQQ1*FATRE
            FACQQ2=FACQQ2*FATRE
          ENDIF
          WID2=1D0
          IF(MINT(55).EQ.6) WID2=WIDS(6,1)
          IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=WIDS(MINT(55),1)
          FACQQ1=FACQQ1*WID2
          FACQQ2=FACQQ2*WID2
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1190
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQQ2
 1190     CONTINUE
 
        ELSEIF(ISUB.EQ.83) THEN
C...f + q -> f' + Q
          FACQQS=COMFAC*(0.5D0*AEM/XW)**2*SH*(SH-SQM3)/(SQMW-TH)**2
          FACQQU=COMFAC*(0.5D0*AEM/XW)**2*UH*(UH-SQM3)/(SQMW-TH)**2
          DO 1210 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1210
            DO 1200 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1200
              IF(I*J.GT.0.AND.MOD(IABS(I+J),2).EQ.0) GOTO 1200
              IF(I*J.LT.0.AND.MOD(IABS(I+J),2).EQ.1) GOTO 1200
              IF(IABS(I).LT.MINT(55).AND.MOD(IABS(I+MINT(55)),2).EQ.1)
     &        THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(MOD(MINT(55),2).EQ.0) FACCKM=VCKM(MINT(55)/2,
     &          (IABS(I)+1)/2)*VINT(180+J)
                IF(MOD(MINT(55),2).EQ.1) FACCKM=VCKM(IABS(I)/2,
     &          (MINT(55)+1)/2)*VINT(180+J)
                WID2=1D0
                IF(I.GT.0) THEN
                  IF(MINT(55).EQ.6) WID2=WIDS(6,2)
                  IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=
     &            WIDS(MINT(55),2)
                ELSE
                  IF(MINT(55).EQ.6) WID2=WIDS(6,3)
                  IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=
     &            WIDS(MINT(55),3)
                ENDIF
                IF(I*J.GT.0) SIGH(NCHN)=FACQQS*FACCKM*WID2
                IF(I*J.LT.0) SIGH(NCHN)=FACQQU*FACCKM*WID2
              ENDIF
              IF(IABS(J).LT.MINT(55).AND.MOD(IABS(J+MINT(55)),2).EQ.1)
     &        THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(MOD(MINT(55),2).EQ.0) FACCKM=VCKM(MINT(55)/2,
     &          (IABS(J)+1)/2)*VINT(180+I)
                IF(MOD(MINT(55),2).EQ.1) FACCKM=VCKM(IABS(J)/2,
     &          (MINT(55)+1)/2)*VINT(180+I)
                IF(J.GT.0) THEN
                  IF(MINT(55).EQ.6) WID2=WIDS(6,2)
                  IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=
     &            WIDS(MINT(55),2)
                ELSE
                  IF(MINT(55).EQ.6) WID2=WIDS(6,3)
                  IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=
     &            WIDS(MINT(55),3)
                ENDIF
                IF(I*J.GT.0) SIGH(NCHN)=FACQQS*FACCKM*WID2
                IF(I*J.LT.0) SIGH(NCHN)=FACQQU*FACCKM*WID2
              ENDIF
 1200       CONTINUE
 1210     CONTINUE
 
        ELSEIF(ISUB.EQ.84) THEN
C...g + gamma -> Q + Qbar
          SQMAVG=0.5D0*(SQM3+SQM4)-0.25D0*(SQM3-SQM4)**2/SH
          THQ=-0.5D0*SH*(1D0-BE34*CTH)
          UHQ=-0.5D0*SH*(1D0+BE34*CTH)
          FACQQ=COMFAC*AS*AEM*(KCHG(IABS(MINT(55)),1)/3D0)**2*
     &    (THQ**2+UHQ**2+4D0*SQMAVG*SH*(1D0-SQMAVG*SH/(THQ*UHQ)))/
     &    (THQ*UHQ)
          IF(MSTP(35).GE.1) FACQQ=FACQQ*PYHFTH(SH,SQMAVG,0D0)
          WID2=1D0
          IF(MINT(55).EQ.6) WID2=WIDS(6,1)
          IF(MINT(55).EQ.7.OR.MINT(55).EQ.8) WID2=WIDS(MINT(55),1)
          FACQQ=FACQQ*WID2
          IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
          IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
 
        ELSEIF(ISUB.EQ.85) THEN
C...gamma + gamma -> F + Fbar (heavy fermion, quark or lepton)
          SQMAVG=0.5D0*(SQM3+SQM4)-0.25D0*(SQM3-SQM4)**2/SH
          THQ=-0.5D0*SH*(1D0-BE34*CTH)
          UHQ=-0.5D0*SH*(1D0+BE34*CTH)
          FACFF=COMFAC*AEM**2*(KCHG(IABS(MINT(56)),1)/3D0)**4*2D0*
     &    ((1D0-PARJ(131)*PARJ(132))*(THQ*UHQ-SQMAVG*SH)*
     &    (UHQ**2+THQ**2+2D0*SQMAVG*SH)+(1D0+PARJ(131)*PARJ(132))*
     &    SQMAVG*SH**2*(SH-2D0*SQMAVG))/(THQ*UHQ)**2
          IF(IABS(MINT(56)).LT.10) FACFF=3D0*FACFF
          IF(IABS(MINT(56)).LT.10.AND.MSTP(35).GE.1)
     &    FACFF=FACFF*PYHFTH(SH,SQMAVG,1D0)
          WID2=1D0
          IF(MINT(56).EQ.6) WID2=WIDS(6,1)
          IF(MINT(56).EQ.7.OR.MINT(56).EQ.8) WID2=WIDS(MINT(56),1)
          IF(MINT(56).EQ.17) WID2=WIDS(17,1)
          FACFF=FACFF*WID2
          IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACFF
          ENDIF
 
        ELSEIF(ISUB.EQ.86) THEN
C...g + g -> J/Psi + g
          FACQQG=COMFAC*AS**3*(5D0/9D0)*PARP(38)*SQRT(SQM3)*
     &    (((SH*(SH-SQM3))**2+(TH*(TH-SQM3))**2+(UH*(UH-SQM3))**2)/
     &    ((TH-SQM3)*(UH-SQM3))**2)/(SH-SQM3)**2
          IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQG
          ENDIF
 
        ELSEIF(ISUB.EQ.87) THEN
C...g + g -> chi_0c + g
          PGTW=(SH*TH+TH*UH+UH*SH)/SH2
          QGTW=(SH*TH*UH)/SH**3
          RGTW=SQM3/SH
          FACQQG=COMFAC*AS**3*4D0*(PARP(39)/SQRT(SQM3))*(1D0/SH)*
     &    (9D0*RGTW**2*PGTW**4*(RGTW**4-2D0*RGTW**2*PGTW+PGTW**2)-
     &    6D0*RGTW*PGTW**3*QGTW*(2D0*RGTW**4-5D0*RGTW**2*PGTW+PGTW**2)-
     &    PGTW**2*QGTW**2*(RGTW**4+2D0*RGTW**2*PGTW-PGTW**2)+
     &    2D0*RGTW*PGTW*QGTW**3*(RGTW**2-PGTW)+6D0*RGTW**2*QGTW**4)/
     &    (QGTW*(QGTW-RGTW*PGTW)**4)
          IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQG
          ENDIF
 
        ELSEIF(ISUB.EQ.88) THEN
C...g + g -> chi_1c + g
          PGTW=(SH*TH+TH*UH+UH*SH)/SH2
          QGTW=(SH*TH*UH)/SH**3
          RGTW=SQM3/SH
          FACQQG=COMFAC*AS**3*12D0*(PARP(39)/SQRT(SQM3))*(1D0/SH)*
     &    PGTW**2*(RGTW*PGTW**2*(RGTW**2-4D0*PGTW)+2D0*QGTW*(-RGTW**4+
     &    5D0*RGTW**2*PGTW+PGTW**2)-15D0*RGTW*QGTW**2)/
     &    (QGTW-RGTW*PGTW)**4
          IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQG
          ENDIF
 
        ELSEIF(ISUB.EQ.89) THEN
C...g + g -> chi_2c + g
          PGTW=(SH*TH+TH*UH+UH*SH)/SH2
          QGTW=(SH*TH*UH)/SH**3
          RGTW=SQM3/SH
          FACQQG=COMFAC*AS**3*4D0*(PARP(39)/SQRT(SQM3))*(1D0/SH)*
     &    (12D0*RGTW**2*PGTW**4*(RGTW**4-2D0*RGTW**2*PGTW+PGTW**2)-
     &    3D0*RGTW*PGTW**3*QGTW*(8D0*RGTW**4-RGTW**2*PGTW+4D0*PGTW**2)+
     &    2D0*PGTW**2*QGTW**2*(-7D0*RGTW**4+43D0*RGTW**2*PGTW+PGTW**2)+
     &    RGTW*PGTW*QGTW**3*(16D0*RGTW**2-61D0*PGTW)+12D0*RGTW**2*
     &    QGTW**4)/(QGTW*(QGTW-RGTW*PGTW)**4)
          IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQG
          ENDIF
        ENDIF
 
C...D: Mimimum bias processes
 
      ELSEIF(ISUB.LE.100) THEN
        IF(ISUB.EQ.91) THEN
C...Elastic scattering
          SIGS=VINT(315)*VINT(316)*SIGT(0,0,1)
 
        ELSEIF(ISUB.EQ.92) THEN
C...Single diffractive scattering (first side, i.e. XB)
          SIGS=VINT(315)*VINT(316)*SIGT(0,0,2)
 
        ELSEIF(ISUB.EQ.93) THEN
C...Single diffractive scattering (second side, i.e. AX)
          SIGS=VINT(315)*VINT(316)*SIGT(0,0,3)
 
        ELSEIF(ISUB.EQ.94) THEN
C...Double diffractive scattering
          SIGS=VINT(315)*VINT(316)*SIGT(0,0,4)
 
        ELSEIF(ISUB.EQ.95) THEN
C...Low-pT scattering
          SIGS=VINT(315)*VINT(316)*SIGT(0,0,5)
 
        ELSEIF(ISUB.EQ.96) THEN
C...Multiple interactions: sum of QCD processes
          CALL PYWIDT(21,SH,WDTP,WDTE)
 
C...q + q' -> q + q'
          FACQQ1=COMFAC*AS**2*4D0/9D0*(SH2+UH2)/TH2
          FACQQB=COMFAC*AS**2*4D0/9D0*((SH2+UH2)/TH2*FACA-
     &    MSTP(34)*2D0/3D0*UH2/(SH*TH))
          FACQQ2=COMFAC*AS**2*4D0/9D0*(SH2+TH2)/UH2
          FACQQI=-COMFAC*AS**2*4D0/9D0*MSTP(34)*2D0/3D0*SH2/(TH*UH)
          RATQQI=(FACQQ1+FACQQ2+FACQQI)/(FACQQ1+FACQQ2)
          DO 1230 I=-5,5
            IF(I.EQ.0) GOTO 1230
            DO 1220 J=-5,5
              IF(J.EQ.0) GOTO 1220
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=111
              SIGH(NCHN)=FACQQ1
              IF(I.EQ.-J) SIGH(NCHN)=FACQQB
              IF(I.EQ.J) THEN
                SIGH(NCHN)=0.5D0*FACQQ1*RATQQI
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=112
                SIGH(NCHN)=0.5D0*FACQQ2*RATQQI
              ENDIF
 1220       CONTINUE
 1230     CONTINUE
 
C...q + qbar -> q' + qbar' or g + g
          FACQQB=COMFAC*AS**2*4D0/9D0*(TH2+UH2)/SH2*
     &    (WDTE(0,1)+WDTE(0,2)+WDTE(0,3)+WDTE(0,4))
          FACGG1=COMFAC*AS**2*32D0/27D0*(UH/TH-(2D0+MSTP(34)*1D0/4D0)*
     &    UH2/SH2)
          FACGG2=COMFAC*AS**2*32D0/27D0*(TH/UH-(2D0+MSTP(34)*1D0/4D0)*
     &    TH2/SH2)
          DO 1240 I=-5,5
            IF(I.EQ.0) GOTO 1240
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=121
            SIGH(NCHN)=FACQQB
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=131
            SIGH(NCHN)=0.5D0*FACGG1
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=132
            SIGH(NCHN)=0.5D0*FACGG2
 1240     CONTINUE
 
C...q + g -> q + g
          FACQG1=COMFAC*AS**2*4D0/9D0*((2D0+MSTP(34)*1D0/4D0)*UH2/TH2-
     &    UH/SH)*FACA
          FACQG2=COMFAC*AS**2*4D0/9D0*((2D0+MSTP(34)*1D0/4D0)*SH2/TH2-
     &    SH/UH)
          DO 1260 I=-5,5
            IF(I.EQ.0) GOTO 1260
            DO 1250 ISDE=1,2
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=281
              SIGH(NCHN)=FACQG1
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=282
              SIGH(NCHN)=FACQG2
 1250       CONTINUE
 1260     CONTINUE
 
C...g + g -> q + qbar (only d, u, s)
          IDC0=MDCY(21,2)-1
          FLAVWT=0D0
          IF(MDME(IDC0+1,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(1,1)**2/SH))
          IF(MDME(IDC0+2,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(2,1)**2/SH))
          IF(MDME(IDC0+3,1).GE.1) FLAVWT=FLAVWT+
     &    SQRT(MAX(0D0,1D0-4D0*PMAS(3,1)**2/SH))
          FACQQ1=COMFAC*AS**2*1D0/6D0*(UH/TH-(2D0+MSTP(34)*1D0/4D0)*
     &    UH2/SH2)*FLAVWT*FACA
          FACQQ2=COMFAC*AS**2*1D0/6D0*(TH/UH-(2D0+MSTP(34)*1D0/4D0)*
     &    TH2/SH2)*FLAVWT*FACA
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=531
          SIGH(NCHN)=FACQQ1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=532
          SIGH(NCHN)=FACQQ2
 
C...g + g -> c + cbar, b + bbar: modified that/uhat for fixed
C...cos(theta-hat)
          DO 1270 IFL=4,5
          SQMAVG=PMAS(IFL,1)**2
          IF(MDME(IDC0+IFL,1).GE.1.AND.SH.GT.4.04D0*SQMAVG) THEN
            BE34=SQRT(1D0-4D0*SQMAVG/SH)
            THQ=-0.5D0*SH*(1D0-BE34*CTH)
            UHQ=-0.5D0*SH*(1D0+BE34*CTH)
            THUHQ=THQ*UHQ-SQMAVG*SH
            IF(MSTP(34).EQ.0) THEN
              FACQQ1=UHQ/THQ-2D0*UHQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/THQ**2
              FACQQ2=THQ/UHQ-2D0*THQ**2/SH2+4D0*(SQMAVG/SH)*THUHQ/UHQ**2
            ELSE
              FACQQ1=UHQ/THQ-2.25D0*UHQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &        THQ**2+0.5D0*SQMAVG*(THQ+SQMAVG)/THQ**2-SQMAVG**2/(SH*THQ)
              FACQQ2=THQ/UHQ-2.25D0*THQ**2/SH2+4.5D0*(SQMAVG/SH)*THUHQ/
     &        UHQ**2+0.5D0*SQMAVG*(UHQ+SQMAVG)/UHQ**2-SQMAVG**2/(SH*UHQ)
            ENDIF
            IF(MSTP(5).GE.5) THEN
              FACQQ1=FACQQ1+2.25D0*SQMAVG*(THQ-UHQ)/(SH*THQ)*REDGGS+
     &        2.25D0*THQ*UHQ/SH2*SQDLGS
              FACQQ2=FACQQ2+2.25D0*SQMAVG*(UHQ-THQ)/(SH*UHQ)*REDGGS+
     &        2.25D0*THQ*UHQ/SH2*SQDLGS
            ENDIF
            FACQQ1=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ1*BE34
            FACQQ2=COMFAC*FACA*AS**2*(1D0/6D0)*FACQQ2*BE34
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=531+2*(IFL-3)
            SIGH(NCHN)=FACQQ1
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=532+2*(IFL-3)
            SIGH(NCHN)=FACQQ2
          ENDIF
 1270     CONTINUE
 
C...g + g -> g + g
          FACGG1=COMFAC*AS**2*9D0/4D0*(SH2/TH2+2D0*SH/TH+3D0+
     &    2D0*TH/SH+TH2/SH2)*FACA
          FACGG2=COMFAC*AS**2*9D0/4D0*(UH2/SH2+2D0*UH/SH+3D0+
     &    2D0*SH/UH+SH2/UH2)*FACA
          FACGG3=COMFAC*AS**2*9D0/4D0*(TH2/UH2+2D0*TH/UH+3+
     &    2D0*UH/TH+UH2/TH2)
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=681
          SIGH(NCHN)=0.5D0*FACGG1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=682
          SIGH(NCHN)=0.5D0*FACGG2
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=683
          SIGH(NCHN)=0.5D0*FACGG3
 
        ELSEIF(ISUB.EQ.99) THEN
C...f + gamma* -> f.
          IF(MINT(107).EQ.4) THEN
            Q2GA=VINT(307)
            P2GA=VINT(308)
            ISDE=2
          ELSE
            Q2GA=VINT(308)
            P2GA=VINT(307)
            ISDE=1
          ENDIF
          COMFAC=PARU(5)*4D0*PARU(1)**2*PARU(101)*VINT(315)*VINT(316)
          PM2RHO=PMAS(PYCOMP(113),1)**2
          IF(MSTP(19).EQ.0) THEN
            COMFAC=COMFAC/Q2GA
          ELSEIF(MSTP(19).EQ.1) THEN
            COMFAC=COMFAC/(Q2GA+PM2RHO)
          ELSEIF(MSTP(19).EQ.2) THEN
            COMFAC=COMFAC*Q2GA/(Q2GA+PM2RHO)**2
          ELSE
            COMFAC=COMFAC*Q2GA/(Q2GA+PM2RHO)**2
            W2GA=VINT(2)
            IF(MINT(11).EQ.22.AND.MINT(12).EQ.22) THEN
              RDRDS=4.1D-3*W2GA**2.167D0/((Q2GA+0.15D0*W2GA)**2*
     &        Q2GA**0.75D0)*(1D0+0.11D0*Q2GA*P2GA/(1D0+0.02D0*P2GA**2))
              XGA=Q2GA/(W2GA+VINT(307)+VINT(308))
            ELSE
              RDRDS=1.5D-4*W2GA**2.167D0/((Q2GA+0.041D0*W2GA)**2*
     &        Q2GA**0.57D0)
              XGA=Q2GA/(W2GA+Q2GA-PMAS(PYCOMP(MINT(10+ISDE)),1)**2)
            ENDIF
            COMFAC=COMFAC*EXP(-MAX(1D-10,RDRDS))
            IF(MSTP(19).EQ.4) COMFAC=COMFAC/MAX(1D-2,1D0-XGA)
          ENDIF
          DO 1280 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(ISDE,I).EQ.0) GOTO 1280
            IF(IABS(I).LT.10.AND.IABS(I).GT.MSTP(58)) GOTO 1280
            EI=KCHG(IABS(I),1)/3D0
            NCHN=NCHN+1
            ISIG(NCHN,ISDE)=I
            ISIG(NCHN,3-ISDE)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=COMFAC*EI**2
 1280     CONTINUE
        ENDIF
 
C...E: 2 -> 1, loop diagrams
 
      ELSEIF(ISUB.LE.110) THEN
        IF(ISUB.EQ.101) THEN
C...g + g -> gamma*/Z0
 
        ELSEIF(ISUB.EQ.102) THEN
C...g + g -> h0 (or H0, or A0)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=4D0*COMFAC/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          HI=SHR*WDTP(13)/32D0
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1290
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=HI*FACBW*HF
 1290     CONTINUE
 
        ELSEIF(ISUB.EQ.103) THEN
C...gamma + gamma -> h0 (or H0, or A0)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=4D0*COMFAC/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          HI=SHR*WDTP(14)*2D0
          IF(KFAC(1,22)*KFAC(2,22).EQ.0) GOTO 1300
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=HI*FACBW*HF
 1300     CONTINUE
 
      ELSEIF(ISUB.EQ.104) THEN
C...g + g -> chi_c0.
        KC=PYCOMP(10441)
        FACBW=COMFAC*12D0*AS**2*PARP(39)*PMAS(KC,2)/
     &  ((SH-PMAS(KC,1)**2)**2+(PMAS(KC,1)*PMAS(KC,2))**2)
        IF(ABS(SQRT(SH)-PMAS(KC,1)).GT.50D0*PMAS(KC,2)) FACBW=0D0
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACBW
        ENDIF
 
      ELSEIF(ISUB.EQ.105) THEN
C...g + g -> chi_c2.
        KC=PYCOMP(445)
        FACBW=COMFAC*16D0*AS**2*PARP(39)*PMAS(KC,2)/
     &  ((SH-PMAS(KC,1)**2)**2+(PMAS(KC,1)*PMAS(KC,2))**2)
        IF(ABS(SQRT(SH)-PMAS(KC,1)).GT.50D0*PMAS(KC,2)) FACBW=0D0
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACBW
        ENDIF
 
C...Continuation C: 2 -> 2, tree diagrams with masses.
 
      ELSEIF(ISUB.EQ.106) THEN
C...g + g -> J/Psi + gamma.
        EQ=2D0/3D0
        FACQQG=COMFAC*AEM*EQ**2*AS**2*(4D0/3D0)*PARP(38)*SQRT(SQM3)*
     &  (((SH*(SH-SQM3))**2+(TH*(TH-SQM3))**2+(UH*(UH-SQM3))**2)/
     &  ((TH-SQM3)*(UH-SQM3))**2)/(SH-SQM3)**2
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
      ELSEIF(ISUB.EQ.107) THEN
C...g + gamma -> J/Psi + g.
        EQ=2D0/3D0
        FACQQG=COMFAC*AEM*EQ**2*AS**2*(32D0/3D0)*PARP(38)*SQRT(SQM3)*
     &  (((SH*(SH-SQM3))**2+(TH*(TH-SQM3))**2+(UH*(UH-SQM3))**2)/
     &  ((TH-SQM3)*(UH-SQM3))**2)/(SH-SQM3)**2
        IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
        IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
      ELSEIF(ISUB.EQ.108) THEN
C...gamma + gamma -> J/Psi + gamma.
        EQ=2D0/3D0
        FACQQG=COMFAC*AEM**3*EQ**6*384D0*PARP(38)*SQRT(SQM3)*
     &  (((SH*(SH-SQM3))**2+(TH*(TH-SQM3))**2+(UH*(UH-SQM3))**2)/
     &  ((TH-SQM3)*(UH-SQM3))**2)/(SH-SQM3)**2
        IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
C...F: 2 -> 2, box diagrams
 
        ELSEIF(ISUB.EQ.110) THEN
C...f + fbar -> gamma + h0
          THUH=MAX(TH*UH,SH*CKIN(3)**2)
          FACHG=COMFAC*(3D0*AEM**4)/(2D0*PARU(1)**2*XW*SQMW)*SH*THUH
          FACHG=FACHG*WIDS(KFHIGG,2)
C...Calculate loop contributions for intermediate gamma* and Z0
          CIGTOT=DCMPLX(0D0,0D0)
          CIZTOT=DCMPLX(0D0,0D0)
          JMAX=3*MSTP(1)+1
          DO 1310 J=1,JMAX
            IF(J.LE.2*MSTP(1)) THEN
              FNC=1D0
              EJ=KCHG(J,1)/3D0
              AJ=SIGN(1D0,EJ+0.1D0)
              VJ=AJ-4D0*EJ*XWV
              BALP=SQM4/(2D0*PMAS(J,1))**2
              BBET=SH/(2D0*PMAS(J,1))**2
            ELSEIF(J.LE.3*MSTP(1)) THEN
              FNC=3D0
              JL=2*(J-2*MSTP(1))-1
              EJ=KCHG(10+JL,1)/3D0
              AJ=SIGN(1D0,EJ+0.1D0)
              VJ=AJ-4D0*EJ*XWV
              BALP=SQM4/(2D0*PMAS(10+JL,1))**2
              BBET=SH/(2D0*PMAS(10+JL,1))**2
            ELSE
              BALP=SQM4/(2D0*PMAS(24,1))**2
              BBET=SH/(2D0*PMAS(24,1))**2
            ENDIF
            BABI=1D0/(BALP-BBET)
            IF(BALP.LT.1D0) THEN
              F0ALP=DCMPLX(DBLE(ASIN(SQRT(BALP))),0D0)
              F1ALP=F0ALP**2
            ELSE
              F0ALP=DCMPLX(DBLE(LOG(SQRT(BALP)+SQRT(BALP-1D0))),
     &        -DBLE(0.5D0*PARU(1)))
              F1ALP=-F0ALP**2
            ENDIF
            F2ALP=DBLE(SQRT(ABS(BALP-1D0)/BALP))*F0ALP
            IF(BBET.LT.1D0) THEN
              F0BET=DCMPLX(DBLE(ASIN(SQRT(BBET))),0D0)
              F1BET=F0BET**2
            ELSE
              F0BET=DCMPLX(DBLE(LOG(SQRT(BBET)+SQRT(BBET-1D0))),
     &        -DBLE(0.5D0*PARU(1)))
              F1BET=-F0BET**2
            ENDIF
            F2BET=DBLE(SQRT(ABS(BBET-1D0)/BBET))*F0BET
            IF(J.LE.3*MSTP(1)) THEN
              FIF=DBLE(0.5D0*BABI)+DBLE(BABI**2)*(DBLE(0.5D0*(1D0-BALP+
     &        BBET))*(F1BET-F1ALP)+DBLE(BBET)*(F2BET-F2ALP))
              CIGTOT=CIGTOT+DBLE(FNC*EJ**2)*FIF
              CIZTOT=CIZTOT+DBLE(FNC*EJ*VJ)*FIF
            ELSE
              TXW=XW/XW1
              CIGTOT=CIGTOT-0.5*(DBLE(BABI*(1.5D0+BALP))+DBLE(BABI**2)*
     &        (DBLE(1.5D0-3D0*BALP+4D0*BBET)*(F1BET-F1ALP)+
     &        DBLE(BBET*(2D0*BALP+3D0))*(F2BET-F2ALP)))
              CIZTOT=CIZTOT-DBLE(0.5D0*BABI*XW1)*(DBLE(5D0-TXW+2D0*BALP*
     &        (1D0-TXW))*(1D0+DBLE(2D0*BABI*BBET)*(F2BET-F2ALP))+
     &        DBLE(BABI*(4D0*BBET*(3D0-TXW)-(2D0*BALP-1D0)*(5D0-TXW)))*
     &        (F1BET-F1ALP))
            ENDIF
 1310     CONTINUE
          CIGTOT=CIGTOT/DBLE(SH)
          CIZTOT=CIZTOT*DBLE(XWC)/DCMPLX(DBLE(SH-SQMZ),DBLE(GMMZ))
C...Loop over initial flavours
          DO 1320 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1320
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACHG*FCOI*(ABS(DBLE(EI)*CIGTOT+DBLE(VI)*
     &      CIZTOT)**2+AI**2*ABS(CIZTOT)**2)
 1320     CONTINUE
 
        ENDIF
 
      ELSEIF(ISUB.LE.120) THEN
        IF(ISUB.EQ.111) THEN
C...f + fbar -> g + h0 (q + qbar -> g + h0 only)
          IF(MSTP(38).NE.0) THEN
C...Simple case: only do gg <-> h exactly.
          CALL PYWIDT(KFHIGG,SQM4,WDTP,WDTE)
          FACGH=COMFAC*FACA*(2D0/9D0)*AS*(WDTP(13)/SQRT(SQM4))*
     &    (TH**2+UH**2)/(SH*SQM4)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMH/((SQM4-SQMH)**2+GMMH**2)
          GMMHC=SQRT(SQM4)*WDTP(0)
          HBW4C=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/
     &    ((SQM4-SQMH)**2+GMMHC**2)
          FACGH=FACGH*HBW4C/HBW4
          ELSE
C...Messy case: do full loop integrals
          A5STUR=0D0
          A5STUI=0D0
          DO 1330 I=1,2*MSTP(1)
            SQMQ=PMAS(I,1)**2
            EPSS=4D0*SQMQ/SH
            EPSH=4D0*SQMQ/SQMH
            CALL PYWAUX(1,EPSS,W1SR,W1SI)
            CALL PYWAUX(1,EPSH,W1HR,W1HI)
            CALL PYWAUX(2,EPSS,W2SR,W2SI)
            CALL PYWAUX(2,EPSH,W2HR,W2HI)
            A5STUR=A5STUR+EPSH*(1D0+SH/(TH+UH)*(W1SR-W1HR)+
     &      (0.25D0-SQMQ/(TH+UH))*(W2SR-W2HR))
            A5STUI=A5STUI+EPSH*(SH/(TH+UH)*(W1SI-W1HI)+
     &      (0.25D0-SQMQ/(TH+UH))*(W2SI-W2HI))
 1330     CONTINUE
          FACGH=COMFAC*FACA/(144D0*PARU(1)**2)*AEM/XW*AS**3*SQMH/SQMW*
     &    SQMH/SH*(UH**2+TH**2)/(UH+TH)**2*(A5STUR**2+A5STUI**2)
          FACGH=FACGH*WIDS(25,2)
          ENDIF
          DO 1340 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1340
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACGH
 1340     CONTINUE
 
        ELSEIF(ISUB.EQ.112) THEN
C...f + g -> f + h0 (q + g -> q + h0 only)
          IF(MSTP(38).NE.0) THEN
C...Simple case: only do gg <-> h exactly.
          CALL PYWIDT(KFHIGG,SQM4,WDTP,WDTE)
          FACQH=COMFAC*FACA*(1D0/12D0)*AS*(WDTP(13)/SQRT(SQM4))*
     &    (SH**2+UH**2)/(-TH*SQM4)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMH/((SQM4-SQMH)**2+GMMH**2)
          GMMHC=SQRT(SQM4)*WDTP(0)
          HBW4C=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/
     &    ((SQM4-SQMH)**2+GMMHC**2)
          FACQH=FACQH*HBW4C/HBW4
          ELSE
C...Messy case: do full loop integrals
          A5TSUR=0D0
          A5TSUI=0D0
          DO 1350 I=1,2*MSTP(1)
            SQMQ=PMAS(I,1)**2
            EPST=4D0*SQMQ/TH
            EPSH=4D0*SQMQ/SQMH
            CALL PYWAUX(1,EPST,W1TR,W1TI)
            CALL PYWAUX(1,EPSH,W1HR,W1HI)
            CALL PYWAUX(2,EPST,W2TR,W2TI)
            CALL PYWAUX(2,EPSH,W2HR,W2HI)
            A5TSUR=A5TSUR+EPSH*(1D0+TH/(SH+UH)*(W1TR-W1HR)+
     &      (0.25D0-SQMQ/(SH+UH))*(W2TR-W2HR))
            A5TSUI=A5TSUI+EPSH*(TH/(SH+UH)*(W1TI-W1HI)+
     &      (0.25D0-SQMQ/(SH+UH))*(W2TI-W2HI))
 1350     CONTINUE
          FACQH=COMFAC*FACA/(384D0*PARU(1)**2)*AEM/XW*AS**3*SQMH/SQMW*
     &    SQMH/(-TH)*(UH**2+SH**2)/(UH+SH)**2*(A5TSUR**2+A5TSUI**2)
          FACQH=FACQH*WIDS(25,2)
          ENDIF
          DO 1370 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 1370
            DO 1360 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1360
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1360
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACQH
 1360       CONTINUE
 1370     CONTINUE
 
        ELSEIF(ISUB.EQ.113) THEN
C...g + g -> g + h0
          IF(MSTP(38).NE.0) THEN
C...Simple case: only do gg <-> h exactly.
          CALL PYWIDT(KFHIGG,SQM4,WDTP,WDTE)
          FACGH=COMFAC*FACA*(3D0/16D0)*AS*(WDTP(13)/SQRT(SQM4))*
     &    (SH**4+TH**4+UH**4+SQM4**4)/(SH*TH*UH*SQM4)
C...Propagators: as simulated in PYOFSH and as desired
          HBW4=GMMH/((SQM4-SQMH)**2+GMMH**2)
          GMMHC=SQRT(SQM4)*WDTP(0)
          HBW4C=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/
     &    ((SQM4-SQMH)**2+GMMHC**2)
          FACGH=FACGH*HBW4C/HBW4
          ELSE
C...Messy case: do full loop integrals
          A2STUR=0D0
          A2STUI=0D0
          A2USTR=0D0
          A2USTI=0D0
          A2TUSR=0D0
          A2TUSI=0D0
          A4STUR=0D0
          A4STUI=0D0
          DO 1380 I=1,2*MSTP(1)
            SQMQ=PMAS(I,1)**2
            EPSS=4D0*SQMQ/SH
            EPST=4D0*SQMQ/TH
            EPSU=4D0*SQMQ/UH
            EPSH=4D0*SQMQ/SQMH
            IF(EPSH.LT.1D-6) GOTO 1380
            CALL PYWAUX(1,EPSS,W1SR,W1SI)
            CALL PYWAUX(1,EPST,W1TR,W1TI)
            CALL PYWAUX(1,EPSU,W1UR,W1UI)
            CALL PYWAUX(1,EPSH,W1HR,W1HI)
            CALL PYWAUX(2,EPSS,W2SR,W2SI)
            CALL PYWAUX(2,EPST,W2TR,W2TI)
            CALL PYWAUX(2,EPSU,W2UR,W2UI)
            CALL PYWAUX(2,EPSH,W2HR,W2HI)
            CALL PYI3AU(EPSS,TH/UH,Y3STUR,Y3STUI)
            CALL PYI3AU(EPSS,UH/TH,Y3SUTR,Y3SUTI)
            CALL PYI3AU(EPST,SH/UH,Y3TSUR,Y3TSUI)
            CALL PYI3AU(EPST,UH/SH,Y3TUSR,Y3TUSI)
            CALL PYI3AU(EPSU,SH/TH,Y3USTR,Y3USTI)
            CALL PYI3AU(EPSU,TH/SH,Y3UTSR,Y3UTSI)
            CALL PYI3AU(EPSH,SQMH/SH*TH/UH,YHSTUR,YHSTUI)
            CALL PYI3AU(EPSH,SQMH/SH*UH/TH,YHSUTR,YHSUTI)
            CALL PYI3AU(EPSH,SQMH/TH*SH/UH,YHTSUR,YHTSUI)
            CALL PYI3AU(EPSH,SQMH/TH*UH/SH,YHTUSR,YHTUSI)
            CALL PYI3AU(EPSH,SQMH/UH*SH/TH,YHUSTR,YHUSTI)
            CALL PYI3AU(EPSH,SQMH/UH*TH/SH,YHUTSR,YHUTSI)
            W3STUR=YHSTUR-Y3STUR-Y3UTSR
            W3STUI=YHSTUI-Y3STUI-Y3UTSI
            W3SUTR=YHSUTR-Y3SUTR-Y3TUSR
            W3SUTI=YHSUTI-Y3SUTI-Y3TUSI
            W3TSUR=YHTSUR-Y3TSUR-Y3USTR
            W3TSUI=YHTSUI-Y3TSUI-Y3USTI
            W3TUSR=YHTUSR-Y3TUSR-Y3SUTR
            W3TUSI=YHTUSI-Y3TUSI-Y3SUTI
            W3USTR=YHUSTR-Y3USTR-Y3TSUR
            W3USTI=YHUSTI-Y3USTI-Y3TSUI
            W3UTSR=YHUTSR-Y3UTSR-Y3STUR
            W3UTSI=YHUTSI-Y3UTSI-Y3STUI
            B2STUR=SQMQ/SQMH**2*(SH*(UH-SH)/(SH+UH)+2D0*TH*UH*
     &      (UH+2D0*SH)/(SH+UH)**2*(W1TR-W1HR)+(SQMQ-SH/4D0)*
     &      (0.5D0*W2SR+0.5D0*W2HR-W2TR+W3STUR)+SH2*(2D0*SQMQ/
     &      (SH+UH)**2-0.5D0/(SH+UH))*(W2TR-W2HR)+0.5D0*TH*UH/SH*
     &      (W2HR-2D0*W2TR)+0.125D0*(SH-12D0*SQMQ-4D0*TH*UH/SH)*W3TSUR)
            B2STUI=SQMQ/SQMH**2*(2D0*TH*UH*(UH+2D0*SH)/(SH+UH)**2*
     &      (W1TI-W1HI)+(SQMQ-SH/4D0)*(0.5D0*W2SI+0.5D0*W2HI-W2TI+
     &      W3STUI)+SH2*(2D0*SQMQ/(SH+UH)**2-0.5D0/(SH+UH))*
     &      (W2TI-W2HI)+0.5D0*TH*UH/SH*(W2HI-2D0*W2TI)+0.125D0*
     &      (SH-12D0*SQMQ-4D0*TH*UH/SH)*W3TSUI)
            B2SUTR=SQMQ/SQMH**2*(SH*(TH-SH)/(SH+TH)+2D0*UH*TH*
     &      (TH+2D0*SH)/(SH+TH)**2*(W1UR-W1HR)+(SQMQ-SH/4D0)*
     &      (0.5D0*W2SR+0.5D0*W2HR-W2UR+W3SUTR)+SH2*(2D0*SQMQ/
     &      (SH+TH)**2-0.5D0/(SH+TH))*(W2UR-W2HR)+0.5D0*UH*TH/SH*
     &      (W2HR-2D0*W2UR)+0.125D0*(SH-12D0*SQMQ-4D0*UH*TH/SH)*W3USTR)
            B2SUTI=SQMQ/SQMH**2*(2D0*UH*TH*(TH+2D0*SH)/(SH+TH)**2*
     &      (W1UI-W1HI)+(SQMQ-SH/4D0)*(0.5D0*W2SI+0.5D0*W2HI-W2UI+
     &      W3SUTI)+SH2*(2D0*SQMQ/(SH+TH)**2-0.5D0/(SH+TH))*
     &      (W2UI-W2HI)+0.5D0*UH*TH/SH*(W2HI-2D0*W2UI)+0.125D0*
     &      (SH-12D0*SQMQ-4D0*UH*TH/SH)*W3USTI)
            B2TSUR=SQMQ/SQMH**2*(TH*(UH-TH)/(TH+UH)+2D0*SH*UH*
     &      (UH+2D0*TH)/(TH+UH)**2*(W1SR-W1HR)+(SQMQ-TH/4D0)*
     &      (0.5D0*W2TR+0.5D0*W2HR-W2SR+W3TSUR)+TH2*(2D0*SQMQ/
     &      (TH+UH)**2-0.5D0/(TH+UH))*(W2SR-W2HR)+0.5D0*SH*UH/TH*
     &      (W2HR-2D0*W2SR)+0.125D0*(TH-12D0*SQMQ-4D0*SH*UH/TH)*W3STUR)
            B2TSUI=SQMQ/SQMH**2*(2D0*SH*UH*(UH+2D0*TH)/(TH+UH)**2*
     &      (W1SI-W1HI)+(SQMQ-TH/4D0)*(0.5D0*W2TI+0.5D0*W2HI-W2SI+
     &      W3TSUI)+TH2*(2D0*SQMQ/(TH+UH)**2-0.5D0/(TH+UH))*
     &      (W2SI-W2HI)+0.5D0*SH*UH/TH*(W2HI-2D0*W2SI)+0.125D0*
     &      (TH-12D0*SQMQ-4D0*SH*UH/TH)*W3STUI)
            B2TUSR=SQMQ/SQMH**2*(TH*(SH-TH)/(TH+SH)+2D0*UH*SH*
     &      (SH+2D0*TH)/(TH+SH)**2*(W1UR-W1HR)+(SQMQ-TH/4D0)*
     &      (0.5D0*W2TR+0.5D0*W2HR-W2UR+W3TUSR)+TH2*(2D0*SQMQ/
     &      (TH+SH)**2-0.5D0/(TH+SH))*(W2UR-W2HR)+0.5D0*UH*SH/TH*
     &      (W2HR-2D0*W2UR)+0.125D0*(TH-12D0*SQMQ-4D0*UH*SH/TH)*W3UTSR)
            B2TUSI=SQMQ/SQMH**2*(2D0*UH*SH*(SH+2D0*TH)/(TH+SH)**2*
     &      (W1UI-W1HI)+(SQMQ-TH/4D0)*(0.5D0*W2TI+0.5D0*W2HI-W2UI+
     &      W3TUSI)+TH2*(2D0*SQMQ/(TH+SH)**2-0.5D0/(TH+SH))*
     &      (W2UI-W2HI)+0.5D0*UH*SH/TH*(W2HI-2D0*W2UI)+0.125D0*
     &      (TH-12D0*SQMQ-4D0*UH*SH/TH)*W3UTSI)
            B2USTR=SQMQ/SQMH**2*(UH*(TH-UH)/(UH+TH)+2D0*SH*TH*
     &      (TH+2D0*UH)/(UH+TH)**2*(W1SR-W1HR)+(SQMQ-UH/4D0)*
     &      (0.5D0*W2UR+0.5D0*W2HR-W2SR+W3USTR)+UH2*(2D0*SQMQ/
     &      (UH+TH)**2-0.5D0/(UH+TH))*(W2SR-W2HR)+0.5D0*SH*TH/UH*
     &      (W2HR-2D0*W2SR)+0.125D0*(UH-12D0*SQMQ-4D0*SH*TH/UH)*W3SUTR)
            B2USTI=SQMQ/SQMH**2*(2D0*SH*TH*(TH+2D0*UH)/(UH+TH)**2*
     &      (W1SI-W1HI)+(SQMQ-UH/4D0)*(0.5D0*W2UI+0.5D0*W2HI-W2SI+
     &      W3USTI)+UH2*(2D0*SQMQ/(UH+TH)**2-0.5D0/(UH+TH))*
     &      (W2SI-W2HI)+0.5D0*SH*TH/UH*(W2HI-2D0*W2SI)+0.125D0*
     &      (UH-12D0*SQMQ-4D0*SH*TH/UH)*W3SUTI)
            B2UTSR=SQMQ/SQMH**2*(UH*(SH-UH)/(UH+SH)+2D0*TH*SH*
     &      (SH+2D0*UH)/(UH+SH)**2*(W1TR-W1HR)+(SQMQ-UH/4D0)*
     &      (0.5D0*W2UR+0.5D0*W2HR-W2TR+W3UTSR)+UH2*(2D0*SQMQ/
     &      (UH+SH)**2-0.5D0/(UH+SH))*(W2TR-W2HR)+0.5D0*TH*SH/UH*
     &      (W2HR-2D0*W2TR)+0.125D0*(UH-12D0*SQMQ-4D0*TH*SH/UH)*W3TUSR)
            B2UTSI=SQMQ/SQMH**2*(2D0*TH*SH*(SH+2D0*UH)/(UH+SH)**2*
     &      (W1TI-W1HI)+(SQMQ-UH/4D0)*(0.5D0*W2UI+0.5D0*W2HI-W2TI+
     &      W3UTSI)+UH2*(2D0*SQMQ/(UH+SH)**2-0.5D0/(UH+SH))*
     &      (W2TI-W2HI)+0.5D0*TH*SH/UH*(W2HI-2D0*W2TI)+0.125D0*
     &      (UH-12D0*SQMQ-4D0*TH*SH/UH)*W3TUSI)
            B4STUR=0.25D0*EPSH*(-2D0/3D0+0.25D0*(EPSH-1D0)*
     &      (W2SR-W2HR+W3STUR))
            B4STUI=0.25D0*EPSH*0.25D0*(EPSH-1D0)*(W2SI-W2HI+W3STUI)
            B4TUSR=0.25D0*EPSH*(-2D0/3D0+0.25D0*(EPSH-1D0)*
     &      (W2TR-W2HR+W3TUSR))
            B4TUSI=0.25D0*EPSH*0.25D0*(EPSH-1D0)*(W2TI-W2HI+W3TUSI)
            B4USTR=0.25D0*EPSH*(-2D0/3D0+0.25D0*(EPSH-1D0)*
     &      (W2UR-W2HR+W3USTR))
            B4USTI=0.25D0*EPSH*0.25D0*(EPSH-1D0)*(W2UI-W2HI+W3USTI)
            A2STUR=A2STUR+B2STUR+B2SUTR
            A2STUI=A2STUI+B2STUI+B2SUTI
            A2USTR=A2USTR+B2USTR+B2UTSR
            A2USTI=A2USTI+B2USTI+B2UTSI
            A2TUSR=A2TUSR+B2TUSR+B2TSUR
            A2TUSI=A2TUSI+B2TUSI+B2TSUI
            A4STUR=A4STUR+B4STUR+B4USTR+B4TUSR
            A4STUI=A4STUI+B4STUI+B4USTI+B4TUSI
 1380     CONTINUE
          FACGH=COMFAC*FACA*3D0/(128D0*PARU(1)**2)*AEM/XW*AS**3*
     &    SQMH/SQMW*SQMH**3/(SH*TH*UH)*(A2STUR**2+A2STUI**2+A2USTR**2+
     &    A2USTI**2+A2TUSR**2+A2TUSI**2+A4STUR**2+A4STUI**2)
          FACGH=FACGH*WIDS(25,2)
          ENDIF
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1390
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACGH
 1390     CONTINUE
 
        ELSEIF(ISUB.EQ.114.OR.ISUB.EQ.115) THEN
C...g + g -> gamma + gamma or g + g -> g + gamma
          A0STUR=0D0
          A0STUI=0D0
          A0TSUR=0D0
          A0TSUI=0D0
          A0UTSR=0D0
          A0UTSI=0D0
          A1STUR=0D0
          A1STUI=0D0
          A2STUR=0D0
          A2STUI=0D0
          ALST=LOG(-SH/TH)
          ALSU=LOG(-SH/UH)
          ALTU=LOG(TH/UH)
          IMAX=2*MSTP(1)
          IF(MSTP(38).GE.1.AND.MSTP(38).LE.8) IMAX=MSTP(38)
          DO 1400 I=1,IMAX
            EI=KCHG(IABS(I),1)/3D0
            EIWT=EI**2
            IF(ISUB.EQ.115) EIWT=EI
            SQMQ=PMAS(I,1)**2
            EPSS=4D0*SQMQ/SH
            EPST=4D0*SQMQ/TH
            EPSU=4D0*SQMQ/UH
            IF((MSTP(38).GE.1.AND.MSTP(38).LE.8).OR.EPSS.LT.1D-4) THEN
              B0STUR=1D0+(TH-UH)/SH*ALTU+0.5D0*(TH2+UH2)/SH2*(ALTU**2+
     &        PARU(1)**2)
              B0STUI=0D0
              B0TSUR=1D0+(SH-UH)/TH*ALSU+0.5D0*(SH2+UH2)/TH2*ALSU**2
              B0TSUI=-PARU(1)*((SH-UH)/TH+(SH2+UH2)/TH2*ALSU)
              B0UTSR=1D0+(SH-TH)/UH*ALST+0.5D0*(SH2+TH2)/UH2*ALST**2
              B0UTSI=-PARU(1)*((SH-TH)/UH+(SH2+TH2)/UH2*ALST)
              B1STUR=-1D0
              B1STUI=0D0
              B2STUR=-1D0
              B2STUI=0D0
            ELSE
              CALL PYWAUX(1,EPSS,W1SR,W1SI)
              CALL PYWAUX(1,EPST,W1TR,W1TI)
              CALL PYWAUX(1,EPSU,W1UR,W1UI)
              CALL PYWAUX(2,EPSS,W2SR,W2SI)
              CALL PYWAUX(2,EPST,W2TR,W2TI)
              CALL PYWAUX(2,EPSU,W2UR,W2UI)
              CALL PYI3AU(EPSS,TH/UH,Y3STUR,Y3STUI)
              CALL PYI3AU(EPSS,UH/TH,Y3SUTR,Y3SUTI)
              CALL PYI3AU(EPST,SH/UH,Y3TSUR,Y3TSUI)
              CALL PYI3AU(EPST,UH/SH,Y3TUSR,Y3TUSI)
              CALL PYI3AU(EPSU,SH/TH,Y3USTR,Y3USTI)
              CALL PYI3AU(EPSU,TH/SH,Y3UTSR,Y3UTSI)
              B0STUR=1D0+(1D0+2D0*TH/SH)*W1TR+(1D0+2D0*UH/SH)*W1UR+
     &        0.5D0*((TH2+UH2)/SH2-EPSS)*(W2TR+W2UR)-
     &        0.25D0*EPST*(1D0-0.5D0*EPSS)*(Y3SUTR+Y3TUSR)-
     &        0.25D0*EPSU*(1D0-0.5D0*EPSS)*(Y3STUR+Y3UTSR)+
     &        0.25D0*(-2D0*(TH2+UH2)/SH2+4D0*EPSS+EPST+EPSU+
     &        0.5D0*EPST*EPSU)*(Y3TSUR+Y3USTR)
              B0STUI=(1D0+2D0*TH/SH)*W1TI+(1D0+2D0*UH/SH)*W1UI+
     &        0.5D0*((TH2+UH2)/SH2-EPSS)*(W2TI+W2UI)-
     &        0.25D0*EPST*(1D0-0.5D0*EPSS)*(Y3SUTI+Y3TUSI)-
     &        0.25D0*EPSU*(1D0-0.5D0*EPSS)*(Y3STUI+Y3UTSI)+
     &        0.25D0*(-2D0*(TH2+UH2)/SH2+4D0*EPSS+EPST+EPSU+
     &        0.5D0*EPST*EPSU)*(Y3TSUI+Y3USTI)
              B0TSUR=1D0+(1D0+2D0*SH/TH)*W1SR+(1D0+2D0*UH/TH)*W1UR+
     &        0.5D0*((SH2+UH2)/TH2-EPST)*(W2SR+W2UR)-
     &        0.25D0*EPSS*(1D0-0.5D0*EPST)*(Y3TUSR+Y3SUTR)-
     &        0.25D0*EPSU*(1D0-0.5D0*EPST)*(Y3TSUR+Y3USTR)+
     &        0.25D0*(-2D0*(SH2+UH2)/TH2+4D0*EPST+EPSS+EPSU+
     &        0.5D0*EPSS*EPSU)*(Y3STUR+Y3UTSR)
              B0TSUI=(1D0+2D0*SH/TH)*W1SI+(1D0+2D0*UH/TH)*W1UI+
     &        0.5D0*((SH2+UH2)/TH2-EPST)*(W2SI+W2UI)-
     &        0.25D0*EPSS*(1D0-0.5D0*EPST)*(Y3TUSI+Y3SUTI)-
     &        0.25D0*EPSU*(1D0-0.5D0*EPST)*(Y3TSUI+Y3USTI)+
     &        0.25D0*(-2D0*(SH2+UH2)/TH2+4D0*EPST+EPSS+EPSU+
     &        0.5D0*EPSS*EPSU)*(Y3STUI+Y3UTSI)
              B0UTSR=1D0+(1D0+2D0*TH/UH)*W1TR+(1D0+2D0*SH/UH)*W1SR+
     &        0.5D0*((TH2+SH2)/UH2-EPSU)*(W2TR+W2SR)-
     &        0.25D0*EPST*(1D0-0.5D0*EPSU)*(Y3USTR+Y3TSUR)-
     &        0.25D0*EPSS*(1D0-0.5D0*EPSU)*(Y3UTSR+Y3STUR)+
     &        0.25D0*(-2D0*(TH2+SH2)/UH2+4D0*EPSU+EPST+EPSS+
     &        0.5D0*EPST*EPSS)*(Y3TUSR+Y3SUTR)
              B0UTSI=(1D0+2D0*TH/UH)*W1TI+(1D0+2D0*SH/UH)*W1SI+
     &        0.5D0*((TH2+SH2)/UH2-EPSU)*(W2TI+W2SI)-
     &        0.25D0*EPST*(1D0-0.5D0*EPSU)*(Y3USTI+Y3TSUI)-
     &        0.25D0*EPSS*(1D0-0.5D0*EPSU)*(Y3UTSI+Y3STUI)+
     &        0.25D0*(-2D0*(TH2+SH2)/UH2+4D0*EPSU+EPST+EPSS+
     &        0.5D0*EPST*EPSS)*(Y3TUSI+Y3SUTI)
              B1STUR=-1D0-0.25D0*(EPSS+EPST+EPSU)*(W2SR+W2TR+W2UR)+
     &        0.25D0*(EPSU+0.5D0*EPSS*EPST)*(Y3SUTR+Y3TUSR)+
     &        0.25D0*(EPST+0.5D0*EPSS*EPSU)*(Y3STUR+Y3UTSR)+
     &        0.25D0*(EPSS+0.5D0*EPST*EPSU)*(Y3TSUR+Y3USTR)
              B1STUI=-0.25D0*(EPSS+EPST+EPSU)*(W2SI+W2TI+W2UI)+
     &        0.25D0*(EPSU+0.5D0*EPSS*EPST)*(Y3SUTI+Y3TUSI)+
     &        0.25D0*(EPST+0.5D0*EPSS*EPSU)*(Y3STUI+Y3UTSI)+
     &        0.25D0*(EPSS+0.5D0*EPST*EPSU)*(Y3TSUI+Y3USTI)
              B2STUR=-1D0+0.125D0*EPSS*EPST*(Y3SUTR+Y3TUSR)+
     &        0.125D0*EPSS*EPSU*(Y3STUR+Y3UTSR)+
     &        0.125D0*EPST*EPSU*(Y3TSUR+Y3USTR)
              B2STUI=0.125D0*EPSS*EPST*(Y3SUTI+Y3TUSI)+
     &        0.125D0*EPSS*EPSU*(Y3STUI+Y3UTSI)+
     &        0.125D0*EPST*EPSU*(Y3TSUI+Y3USTI)
            ENDIF
            A0STUR=A0STUR+EIWT*B0STUR
            A0STUI=A0STUI+EIWT*B0STUI
            A0TSUR=A0TSUR+EIWT*B0TSUR
            A0TSUI=A0TSUI+EIWT*B0TSUI
            A0UTSR=A0UTSR+EIWT*B0UTSR
            A0UTSI=A0UTSI+EIWT*B0UTSI
            A1STUR=A1STUR+EIWT*B1STUR
            A1STUI=A1STUI+EIWT*B1STUI
            A2STUR=A2STUR+EIWT*B2STUR
            A2STUI=A2STUI+EIWT*B2STUI
 1400     CONTINUE
          ASQSUM=A0STUR**2+A0STUI**2+A0TSUR**2+A0TSUI**2+A0UTSR**2+
     &    A0UTSI**2+4D0*A1STUR**2+4D0*A1STUI**2+A2STUR**2+A2STUI**2
          FACGG=COMFAC*FACA/(16D0*PARU(1)**2)*AS**2*AEM**2*ASQSUM
          FACGP=COMFAC*FACA*5D0/(192D0*PARU(1)**2)*AS**3*AEM*ASQSUM
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1410
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          IF(ISUB.EQ.114) SIGH(NCHN)=0.5D0*FACGG
          IF(ISUB.EQ.115) SIGH(NCHN)=FACGP
 1410     CONTINUE
 
        ELSEIF(ISUB.EQ.116) THEN
C...g + g -> gamma + Z0
 
        ELSEIF(ISUB.EQ.117) THEN
C...g + g -> Z0 + Z0
 
        ELSEIF(ISUB.EQ.118) THEN
C...g + g -> W+ + W-
 
        ENDIF
 
C...G: 2 -> 3, tree diagrams
 
      ELSEIF(ISUB.LE.140) THEN
        IF(ISUB.EQ.121) THEN
C...g + g -> Q + Qbar + h0
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1420
          IA=KFPR(ISUBSV,2)
          PMF=PYMRUN(IA,SH)
          FACQQH=COMFAC*(4D0*PARU(1)*AEM/XW)*(4D0*PARU(1)*AS)**2*
     &    (0.5D0*PMF/PMAS(24,1))**2
          WID2=1D0
          IF(IA.EQ.6.OR.IA.EQ.7.OR.IA.EQ.8) WID2=WIDS(IA,1)
          FACQQH=FACQQH*WID2
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            IKFI=1
            IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
            IF(IA.GT.10) IKFI=3
            FACQQH=FACQQH*PARU(150+10*IHIGG+IKFI)**2
            IF(IMSS(1).NE.0.AND.IA.EQ.5) THEN
              FACQQH=FACQQH/(1D0+RMSS(41))**2
              IF(IHIGG.NE.3) THEN
                FACQQH=FACQQH*(1D0+RMSS(41)*PARU(152+10*IHIGG)/
     &          PARU(151+10*IHIGG))**2
              ENDIF
            ENDIF
          ENDIF
          CALL PYQQBH(WTQQBH)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=(1D0/PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQH*WTQQBH*FACBW
 1420     CONTINUE
 
        ELSEIF(ISUB.EQ.122) THEN
C...q + qbar -> Q + Qbar + h0
          IA=KFPR(ISUBSV,2)
          PMF=PYMRUN(IA,SH)
          FACQQH=COMFAC*(4D0*PARU(1)*AEM/XW)*(4D0*PARU(1)*AS)**2*
     &    (0.5D0*PMF/PMAS(24,1))**2
          WID2=1D0
          IF(IA.EQ.6.OR.IA.EQ.7.OR.IA.EQ.8) WID2=WIDS(IA,1)
          FACQQH=FACQQH*WID2
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            IKFI=1
            IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
            IF(IA.GT.10) IKFI=3
            FACQQH=FACQQH*PARU(150+10*IHIGG+IKFI)**2
            IF(IMSS(1).NE.0.AND.IA.EQ.5) THEN
              FACQQH=FACQQH/(1D0+RMSS(41))**2
              IF(IHIGG.NE.3) THEN
                FACQQH=FACQQH*(1D0+RMSS(41)*PARU(152+10*IHIGG)/
     &          PARU(151+10*IHIGG))**2
              ENDIF
            ENDIF
          ENDIF
          CALL PYQQBH(WTQQBH)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=(1D0/PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          DO 1430 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1430
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQH*WTQQBH*FACBW
 1430     CONTINUE
 
        ELSEIF(ISUB.EQ.123) THEN
C...f + f' -> f + f' + h0 (or H0, or A0) (Z0 + Z0 -> h0 as
C...inner process)
          FACNOR=COMFAC*(4D0*PARU(1)*AEM/(XW*XW1))**3*SQMZ/32D0
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACNOR=FACNOR*
     &    PARU(154+10*IHIGG)**2
          FACPRP=1D0/((VINT(215)-VINT(204)**2)*
     &    (VINT(216)-VINT(209)**2))**2
          FACZZ1=FACNOR*FACPRP*(0.5D0*TAUP*VINT(2))*VINT(219)
          FACZZ2=FACNOR*FACPRP*VINT(217)*VINT(218)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=(1D0/PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          DO 1450 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1450
            IA=IABS(I)
            DO 1440 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1440
              JA=IABS(J)
              EI=KCHG(IA,1)*ISIGN(1,I)/3D0
              AI=SIGN(1D0,KCHG(IA,1)+0.5D0)*ISIGN(1,I)
              VI=AI-4D0*EI*XWV
              EJ=KCHG(JA,1)*ISIGN(1,J)/3D0
              AJ=SIGN(1D0,KCHG(JA,1)+0.5D0)*ISIGN(1,J)
              VJ=AJ-4D0*EJ*XWV
              FACLR1=(VI**2+AI**2)*(VJ**2+AJ**2)+4D0*VI*AI*VJ*AJ
              FACLR2=(VI**2+AI**2)*(VJ**2+AJ**2)-4D0*VI*AI*VJ*AJ
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=(FACLR1*FACZZ1+FACLR2*FACZZ2)*FACBW
 1440       CONTINUE
 1450     CONTINUE
 
        ELSEIF(ISUB.EQ.124) THEN
C...f + f' -> f" + f"' + h0 (or H0, or A0) (W+ + W- -> h0 as
C...inner process)
          FACNOR=COMFAC*(4D0*PARU(1)*AEM/XW)**3*SQMW
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACNOR=FACNOR*
     &    PARU(155+10*IHIGG)**2
          FACPRP=1D0/((VINT(215)-VINT(204)**2)*
     &    (VINT(216)-VINT(209)**2))**2
          FACWW=FACNOR*FACPRP*(0.5D0*TAUP*VINT(2))*VINT(219)
          CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACBW=(1D0/PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFHIGG,1)).GT.PARP(48)*PMAS(KFHIGG,2))
     &    FACBW=0D0
          DO 1470 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1470
            EI=SIGN(1D0,DBLE(I))*KCHG(IABS(I),1)
            DO 1460 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1460
              EJ=SIGN(1D0,DBLE(J))*KCHG(IABS(J),1)
              IF(EI*EJ.GT.0D0) GOTO 1460
              FACLR=VINT(180+I)*VINT(180+J)
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACLR*FACWW*FACBW
 1460       CONTINUE
 1470     CONTINUE
 
        ELSEIF(ISUB.EQ.131.OR.ISUB.EQ.132) THEN
C...f + gamma*_(T,L) -> f + g (q + gamma*_(T,L) -> q + g only)
          PH=0D0
          IF(MINT(15).EQ.22.AND.MINT(107).EQ.0.AND.VINT(3).LT.0D0)
     &    PH=VINT(3)**2
          IF(MINT(16).EQ.22.AND.MINT(108).EQ.0.AND.VINT(4).LT.0D0)
     &    PH=VINT(4)**2
          IF(ISUB.EQ.131) THEN
            FGQ=COMFAC*AS*AEM*8D0/3D0*SH**2/(SH+PH)**2*
     &      ((SH2+UH2-2D0*PH*TH)/(-SH*UH)-2D0*PH*TH/(SH+PH)**2)
          ELSE
            FGQ=COMFAC*AS*AEM*8D0/3D0*SH**2/(SH+PH)**4*(-4D0*PH*TH)
          ENDIF
          DO 1490 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 1490
            EI=KCHG(IABS(I),1)/3D0
            FACGQ=FGQ*EI**2
            DO 1480 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 1480
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 1480
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGQ
 1480       CONTINUE
 1490     CONTINUE
 
        ELSEIF(ISUB.EQ.133.OR.ISUB.EQ.134) THEN
C...f + gamma*_(T,L) -> f + gamma
          PH=0D0
          IF(MINT(15).EQ.22.AND.MINT(107).EQ.0.AND.VINT(3).LT.0D0)
     &    PH=VINT(3)**2
          IF(MINT(16).EQ.22.AND.MINT(108).EQ.0.AND.VINT(4).LT.0D0)
     &    PH=VINT(4)**2
          IF(ISUB.EQ.133) THEN
            FGQ=COMFAC*AEM**2*2D0*SH**2/(SH+PH)**2*
     &      ((SH2+UH2-2D0*PH*TH)/(-SH*UH)-2D0*PH*TH/(SH+PH)**2)
          ELSE
            FGQ=COMFAC*AEM**2*2D0*SH**2/(SH+PH)**4*(-4D0*PH*TH)
          ENDIF
          DO 1510 I=MMINA,MMAXA
            IF(I.EQ.0) GOTO 1510
            EI=KCHG(IABS(I),1)/3D0
            FACGQ=FGQ*EI**4
            DO 1500 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 1500
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 1500
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGQ
 1500       CONTINUE
 1510     CONTINUE
 
        ELSEIF(ISUB.EQ.135.OR.ISUB.EQ.136) THEN
C...g + gamma*_(T,L) -> f + fbar (g + gamma*_(T,L) -> q + qbar only)
          PH=0D0
          IF(MINT(15).EQ.22.AND.MINT(107).EQ.0.AND.VINT(3).LT.0D0)
     &    PH=VINT(3)**2
          IF(MINT(16).EQ.22.AND.MINT(108).EQ.0.AND.VINT(4).LT.0D0)
     &    PH=VINT(4)**2
          CALL PYWIDT(21,SH,WDTP,WDTE)
          WDTESU=0D0
          DO 1520 I=1,MIN(8,MDCY(21,3))
            EF=KCHG(I,1)/3D0
            WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+
     &      WDTE(I,4))
 1520     CONTINUE
          IF(ISUB.EQ.135) THEN
            FACQQ=COMFAC*AEM*AS*WDTESU*SH**2/(SH+PH)**2*
     &      ((TH2+UH2-2D0*PH*SH)/(TH*UH)+4D0*PH*SH/(SH+PH)**2)
          ELSE
            FACQQ=COMFAC*AEM*AS*WDTESU*SH**2/(SH+PH)**4*8D0*PH*SH
          ENDIF
          IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
          IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ
          ENDIF
 
        ELSEIF(ISUB.GE.137.AND.ISUB.LE.140) THEN
C...gamma*_(T,L) + gamma*_(T,L) -> f + fbar
          PH1=0D0
          IF(VINT(3).LT.0D0) PH1=VINT(3)**2
          PH2=0D0
          IF(VINT(4).LT.0D0) PH2=VINT(4)**2
          CALL PYWIDT(22,SH,WDTP,WDTE)
          WDTESU=0D0
          DO 1530 I=1,MIN(12,MDCY(22,3))
            IF(I.LE.8) EF= KCHG(I,1)/3D0
            IF(I.GE.9) EF= KCHG(9+2*(I-8),1)/3D0
            WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+
     &      WDTE(I,4))
 1530     CONTINUE
          DLAMB2=(TH+UH)**2-4D0*PH1*PH2
          IF(ISUB.EQ.137) THEN
            FPARAM=-SH*(TH+UH)/DLAMB2
            FACFF=COMFAC*AEM**2*WDTESU*2D0*SH2/(DLAMB2*TH2*UH2)*
     &      (TH*UH-PH1*PH2)*((TH2+UH2)*(1D0-2D0*FPARAM*(1D0-FPARAM))-
     &      2D0*PH1*PH2*FPARAM**2)
          ELSEIF(ISUB.EQ.138) THEN
            FACFF=COMFAC*AEM**2*WDTESU*4D0*SH2*SH/(DLAMB2**2*TH2*UH2)*
     &      PH2*(4D0*(TH*UH-PH1*PH2)*(TH*UH+PH1*SH*(TH-UH)**2/DLAMB2)+
     &      2D0*PH1**2*(TH-UH)**2)
          ELSEIF(ISUB.EQ.139) THEN
            FACFF=COMFAC*AEM**2*WDTESU*4D0*SH2*SH/(DLAMB2**2*TH2*UH2)*
     &      PH1*(4D0*(TH*UH-PH1*PH2)*(TH*UH+PH2*SH*(TH-UH)**2/DLAMB2)+
     &      2D0*PH2**2*(TH-UH)**2)
          ELSE
            FACFF=COMFAC*AEM**2*WDTESU*32D0*SH2**2/(DLAMB2**3*TH2*UH2)*
     &      PH1*PH2*(TH*UH-PH1*PH2)*(TH-UH)**2
          ENDIF
          IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=22
            ISIG(NCHN,2)=22
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACFF
          ENDIF
 
        ENDIF
 
C...H: 2 -> 1, tree diagrams, non-standard model processes
 
      ELSEIF(ISUB.LE.160) THEN
        IF(ISUB.EQ.141) THEN
C...f + fbar -> gamma*/Z0/Z'0
          SQMZP=PMAS(32,1)**2
          MINT(61)=2
          CALL PYWIDT(32,SH,WDTP,WDTE)
          HP0=AEM/3D0*SH
          HP1=AEM/3D0*XWC*SH
          HP2=HP1
          HS=SHR*VINT(117)
          HSP=SHR*WDTP(0)
          FACZP=4D0*COMFAC*3D0
          DO 1540 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1540
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI)
            VI=AI-4D0*EI*XWV
            IA=IABS(I)
            IF(IA.LT.10) THEN
              IF(IA.LE.2) THEN
                VPI=PARU(123-2*MOD(IABS(I),2))
                API=PARU(124-2*MOD(IABS(I),2))
              ELSEIF(IA.LE.4) THEN
                VPI=PARJ(182-2*MOD(IABS(I),2))
                API=PARJ(183-2*MOD(IABS(I),2))
              ELSE
                VPI=PARJ(190-2*MOD(IABS(I),2))
                API=PARJ(191-2*MOD(IABS(I),2))
              ENDIF
            ELSE
              IF(IA.LE.12) THEN
                VPI=PARU(127-2*MOD(IABS(I),2))
                API=PARU(128-2*MOD(IABS(I),2))
              ELSEIF(IA.LE.14) THEN
                VPI=PARJ(186-2*MOD(IABS(I),2))
                API=PARJ(187-2*MOD(IABS(I),2))
              ELSE
                VPI=PARJ(194-2*MOD(IABS(I),2))
                API=PARJ(195-2*MOD(IABS(I),2))
              ENDIF
            ENDIF
            HI0=HP0
            IF(IABS(I).LE.10) HI0=HI0*FACA/3D0
            HI1=HP1
            IF(IABS(I).LE.10) HI1=HI1*FACA/3D0
            HI2=HP2
            IF(IABS(I).LE.10) HI2=HI2*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACZP*(EI**2/SH2*HI0*HP0*VINT(111)+EI*VI*
     &      (1D0-SQMZ/SH)/((SH-SQMZ)**2+HS**2)*(HI0*HP1+HI1*HP0)*
     &      VINT(112)+EI*VPI*(1D0-SQMZP/SH)/((SH-SQMZP)**2+HSP**2)*
     &      (HI0*HP2+HI2*HP0)*VINT(113)+(VI**2+AI**2)/
     &      ((SH-SQMZ)**2+HS**2)*HI1*HP1*VINT(114)+(VI*VPI+AI*API)*
     &      ((SH-SQMZ)*(SH-SQMZP)+HS*HSP)/(((SH-SQMZ)**2+HS**2)*
     &      ((SH-SQMZP)**2+HSP**2))*(HI1*HP2+HI2*HP1)*VINT(115)+
     &      (VPI**2+API**2)/((SH-SQMZP)**2+HSP**2)*HI2*HP2*VINT(116))
 1540     CONTINUE
 
        ELSEIF(ISUB.EQ.142) THEN
C...f + fbar' -> W'+/-
          SQMWP=PMAS(34,1)**2
          CALL PYWIDT(34,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMWP)**2+HS**2)*3D0
          HP=AEM/(24D0*XW)*SH
          DO 1560 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1560
            IA=IABS(I)
            DO 1550 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1550
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1550
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 1550
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP*(PARU(133)**2+PARU(134)**2)
              IF(IA.LE.10) HI=HP*(PARU(131)**2+PARU(132)**2)*
     &        VCKM((IA+1)/2,(JA+1)/2)*FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))
              SIGH(NCHN)=HI*FACBW*HF
 1550       CONTINUE
 1560     CONTINUE
 
        ELSEIF(ISUB.EQ.143) THEN
C...f + fbar' -> H+/-
          SQMHC=PMAS(37,1)**2
          CALL PYWIDT(37,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMHC)**2+HS**2)
          HP=AEM/(8D0*XW)*SH/SQMW*SH
          DO 1580 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1580
            IA=IABS(I)
            IM=(MOD(IA,10)+1)/2
            DO 1570 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1570
              JA=IABS(J)
              JM=(MOD(JA,10)+1)/2
              IF(I*J.GT.0.OR.IA.EQ.JA.OR.IM.NE.JM) GOTO 1570
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 1570
              IF(MOD(IA,2).EQ.0) THEN
                IU=IA
                IL=JA
              ELSE
                IU=JA
                IL=IA
              ENDIF
              RML=PYMRUN(IL,SH)**2/SH
              RMU=PYMRUN(IU,SH)**2/SH
              HI=HP*(RML*PARU(141)**2+RMU/PARU(141)**2)
              IF(IA.LE.10) HI=HI*FACA/3D0
              KCHHC=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHHC)/2)+WDTE(0,4))
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1570       CONTINUE
 1580     CONTINUE
 
        ELSEIF(ISUB.EQ.144) THEN
C...f + fbar' -> R
          SQMR=PMAS(41,1)**2
          CALL PYWIDT(41,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMR)**2+HS**2)*3D0
          HP=AEM/(12D0*XW)*SH
          DO 1600 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1600
            IA=IABS(I)
            DO 1590 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1590
              JA=IABS(J)
              IF(I*J.GT.0.OR.IABS(IA-JA).NE.2) GOTO 1590
              HI=HP
              IF(IA.LE.10) HI=HI*FACA/3D0
              HF=SHR*(WDTE(0,1)+WDTE(0,(10-(I+J))/4)+WDTE(0,4))
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1590       CONTINUE
 1600     CONTINUE
 
        ELSEIF(ISUB.EQ.145) THEN
C...q + l -> LQ (leptoquark)
          SQMLQ=PMAS(42,1)**2
          CALL PYWIDT(42,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMLQ)**2+HS**2)
          IF(ABS(SHR-PMAS(42,1)).GT.PARP(48)*PMAS(42,2)) FACBW=0D0
          HP=AEM/4D0*SH
          KFLQQ=KFDP(MDCY(42,2),1)
          KFLQL=KFDP(MDCY(42,2),2)
          DO 1620 I=MMIN1,MMAX1
            IF(KFAC(1,I).EQ.0) GOTO 1620
            IA=IABS(I)
            IF(IA.NE.KFLQQ.AND.IA.NE.IABS(KFLQL)) GOTO 1620
            DO 1610 J=MMIN2,MMAX2
              IF(KFAC(2,J).EQ.0) GOTO 1610
              JA=IABS(J)
              IF(JA.NE.KFLQQ.AND.JA.NE.IABS(KFLQL)) GOTO 1610
              IF(I*J.NE.KFLQQ*KFLQL) GOTO 1610
              IF(JA.EQ.IA) GOTO 1610
              IF(IA.EQ.KFLQQ) KCHLQ=ISIGN(1,I)
              IF(JA.EQ.KFLQQ) KCHLQ=ISIGN(1,J)
              HI=HP*PARU(151)
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHLQ)/2)+WDTE(0,4))
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1610       CONTINUE
 1620     CONTINUE
 
        ELSEIF(ISUB.EQ.146) THEN
C...e + gamma* -> e* (excited lepton)
          KFQSTR=KFPR(ISUB,1)
          KCQSTR=PYCOMP(KFQSTR)
          KFQEXC=MOD(KFQSTR,KEXCIT)
          CALL PYWIDT(KFQSTR,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=COMFAC/((SH-PMAS(KCQSTR,1)**2)**2+HS**2)
          QF=-PARU(157)/2D0-PARU(158)/2D0
          FACBW=FACBW*AEM*QF**2*SH/PARU(155)**2
          IF(ABS(SHR-PMAS(KCQSTR,1)).GT.PARP(48)*PMAS(KCQSTR,2))
     &    FACBW=0D0
          HP=SH
          DO 1640 I=-KFQEXC,KFQEXC,2*KFQEXC
            DO 1630 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 1630
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 1630
              HI=HP
              IF(I.GT.0) HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
              IF(I.LT.0) HF=SHR*(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1630       CONTINUE
 1640     CONTINUE
 
        ELSEIF(ISUB.EQ.147.OR.ISUB.EQ.148) THEN
C...d + g -> d* and u + g -> u* (excited quarks)
          KFQSTR=KFPR(ISUB,1)
          KCQSTR=PYCOMP(KFQSTR)
          KFQEXC=MOD(KFQSTR,KEXCIT)
          CALL PYWIDT(KFQSTR,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=COMFAC/((SH-PMAS(KCQSTR,1)**2)**2+HS**2)
          FACBW=FACBW*AS*PARU(159)**2*SH/(3D0*PARU(155)**2)
          IF(ABS(SHR-PMAS(KCQSTR,1)).GT.PARP(48)*PMAS(KCQSTR,2))
     &    FACBW=0D0
          HP=SH
          DO 1660 I=-KFQEXC,KFQEXC,2*KFQEXC
            DO 1650 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1650
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1650
              HI=HP
              IF(I.GT.0) HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
              IF(I.LT.0) HF=SHR*(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1650       CONTINUE
 1660     CONTINUE
 
        ELSEIF(ISUB.EQ.149) THEN
C...g + g -> eta_tc
          KCTC=PYCOMP(KTECHN+331)
          CALL PYWIDT(KTECHN+331,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=COMFAC*0.5D0/((SH-PMAS(KCTC,1)**2)**2+HS**2)
          IF(ABS(SHR-PMAS(KCTC,1)).GT.PARP(48)*PMAS(KCTC,2)) FACBW=0D0
          HP=SH
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1670
          HI=HP*WDTP(3)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=HI*FACBW*HF
 1670     CONTINUE
 
        ENDIF
 
C...I: 2 -> 2, tree diagrams, non-standard model processes
 
      ELSEIF(ISUB.LE.200) THEN
        IF(ISUB.EQ.161) THEN
C...f + g -> f' + H+/- (b + g -> t + H+/- only)
C...(choice of only b and t to avoid kinematics problems)
          FHCQ=COMFAC*FACA*AS*AEM/XW*1D0/24
C...H propagator: as simulated in PYOFSH and as desired
          SQMHC=PMAS(37,1)**2
          GMMHC=PMAS(37,1)*PMAS(37,2)
          HBW4=GMMHC/((SQM4-SQMHC)**2+GMMHC**2)
          CALL PYWIDT(37,SQM4,WDTP,WDTE)
          GMMHCC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMHCC/((SQM4-SQMHC)**2+GMMHCC**2)
          FHCQ=FHCQ*HBW4C/HBW4
          DO 1690 I=MMINA,MMAXA
            IA=IABS(I)
            IF(IA.NE.5) GOTO 1690
            SQML=PYMRUN(IA,SH)**2
            IUA=IA+MOD(IA,2)
            SQMQ=PYMRUN(IUA,SH)**2
            FACHCQ=FHCQ*(SQML*PARU(141)**2+SQMQ/PARU(141)**2)/SQMW*
     &      (SH/(SQMQ-UH)+2D0*SQMQ*(SQMHC-UH)/(SQMQ-UH)**2+(SQMQ-UH)/SH+
     &      2D0*SQMQ/(SQMQ-UH)+2D0*(SQMHC-UH)/(SQMQ-UH)*
     &      (SQMHC-SQMQ-SH)/SH)
            KCHHC=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
            DO 1680 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1680
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,1).EQ.0) GOTO 1680
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACHCQ*WIDS(37,(5-KCHHC)/2)
 1680       CONTINUE
 1690     CONTINUE
 
        ELSEIF(ISUB.EQ.162) THEN
C...q + g -> LQ + lbar; LQ=leptoquark
          SQMLQ=PMAS(42,1)**2
          FACLQ=COMFAC*FACA*PARU(151)*(AS*AEM/6D0)*(-TH/SH)*
     &    (UH2+SQMLQ**2)/(UH-SQMLQ)**2
          KFLQQ=KFDP(MDCY(42,2),1)
          DO 1710 I=MMINA,MMAXA
            IF(IABS(I).NE.KFLQQ) GOTO 1710
            KCHLQ=ISIGN(1,I)
            DO 1700 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1700
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1700
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACLQ*WIDS(42,(5-KCHLQ)/2)
 1700       CONTINUE
 1710     CONTINUE
 
        ELSEIF(ISUB.EQ.163) THEN
C...g + g -> LQ + LQbar; LQ=leptoquark
          SQMLQ=PMAS(42,1)**2
          FACLQ=COMFAC*FACA*WIDS(42,1)*(AS**2/2D0)*
     &    (7D0/48D0+3D0*(UH-TH)**2/(16D0*SH2))*(1D0+2D0*SQMLQ*TH/
     &    (TH-SQMLQ)**2+2D0*SQMLQ*UH/(UH-SQMLQ)**2+4D0*SQMLQ**2/
     &    ((TH-SQMLQ)*(UH-SQMLQ)))
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1720
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
C...Since don't know proper colour flow, randomize between alternatives
          ISIG(NCHN,3)=INT(1.5D0+PYR(0))
          SIGH(NCHN)=FACLQ
 1720     CONTINUE
 
        ELSEIF(ISUB.EQ.164) THEN
C...q + qbar -> LQ + LQbar; LQ=leptoquark
          DELTA=0.25D0*(SQM3-SQM4)**2/SH
          SQMLQ=0.5D0*(SQM3+SQM4)-DELTA
          TH=TH-DELTA
          UH=UH-DELTA
C          SQMLQ=PMAS(42,1)**2
          FACLQA=COMFAC*WIDS(42,1)*(AS**2/9D0)*
     &    (SH*(SH-4D0*SQMLQ)-(UH-TH)**2)/SH2
          FACLQS=COMFAC*WIDS(42,1)*((PARU(151)**2*AEM**2/8D0)*
     &    (-SH*TH-(SQMLQ-TH)**2)/TH2+(PARU(151)*AEM*AS/18D0)*
     &    ((SQMLQ-TH)*(UH-TH)+SH*(SQMLQ+TH))/(SH*TH))
          KFLQQ=KFDP(MDCY(42,2),1)
          DO 1730 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1730
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACLQA
            IF(IABS(I).EQ.KFLQQ) SIGH(NCHN)=FACLQA+FACLQS
 1730     CONTINUE
 
        ELSEIF(ISUB.EQ.165) THEN
C...q + qbar -> l+ + l- (including contact term for compositeness)
          ZRATR=XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+GMMZ**2)
          ZRATI=XWC*SH*GMMZ/((SH-SQMZ)**2+GMMZ**2)
          KFF=IABS(KFPR(ISUB,1))
          EF=KCHG(KFF,1)/3D0
          AF=SIGN(1D0,EF+0.1D0)
          VF=AF-4D0*EF*XWV
          VALF=VF+AF
          VARF=VF-AF
          FCOF=1D0
          IF(KFF.LE.10) FCOF=3D0
          WID2=1D0
          IF(KFF.EQ.6) WID2=WIDS(6,1)
          IF(KFF.EQ.7.OR.KFF.EQ.8) WID2=WIDS(KFF,1)
          IF(KFF.EQ.17.OR.KFF.EQ.18) WID2=WIDS(KFF,1)
          DO 1740 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1740
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=VI+AI
            VARI=VI-AI
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            IF((MSTP(5).EQ.1.AND.IABS(I).LE.2).OR.MSTP(5).EQ.2) THEN
              FGZA=(EI*EF+VALI*VALF*ZRATR+PARU(156)*SH/
     &        (AEM*PARU(155)**2))**2+(VALI*VALF*ZRATI)**2+
     &        (EI*EF+VARI*VARF*ZRATR)**2+(VARI*VARF*ZRATI)**2
            ELSE
              FGZA=(EI*EF+VALI*VALF*ZRATR)**2+(VALI*VALF*ZRATI)**2+
     &        (EI*EF+VARI*VARF*ZRATR)**2+(VARI*VARF*ZRATI)**2
            ENDIF
            FGZB=(EI*EF+VALI*VARF*ZRATR)**2+(VALI*VARF*ZRATI)**2+
     &      (EI*EF+VARI*VALF*ZRATR)**2+(VARI*VALF*ZRATI)**2
            FGZAB=AEM**2*(FGZA*UH2/SH2+FGZB*TH2/SH2)
            IF((MSTP(5).EQ.3.AND.IABS(I).EQ.2).OR.(MSTP(5).EQ.4.AND.
     &      MOD(IABS(I),2).EQ.0)) FGZAB=FGZAB+SH2/(2D0*PARU(155)**4)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=COMFAC*FCOI*FCOF*FGZAB*WID2
 1740     CONTINUE
 
        ELSEIF(ISUB.EQ.166) THEN
C...q + q'bar -> l + nu_l (including contact term for compositeness)
          WFAC=(1D0/4D0)*(AEM/XW)**2*UH2/((SH-SQMW)**2+GMMW**2)
          WCIFAC=WFAC+SH2/(4D0*PARU(155)**4)
          KFF=IABS(KFPR(ISUB,1))
          FCOF=1D0
          IF(KFF.LE.10) FCOF=3D0
          DO 1760 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1760
            IA=IABS(I)
            DO 1750 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1750
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1750
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 1750
              FCOI=1D0
              IF(IA.LE.10) FCOI=VCKM((IA+1)/2,(JA+1)/2)*FACA/3D0
              WID2=1D0
              IF((I.GT.0.AND.MOD(I,2).EQ.0).OR.(J.GT.0.AND.
     &        MOD(J,2).EQ.0)) THEN
                IF(KFF.EQ.5) WID2=WIDS(6,2)
                IF(KFF.EQ.7) WID2=WIDS(8,2)*WIDS(7,3)
                IF(KFF.EQ.17) WID2=WIDS(18,2)*WIDS(17,3)
              ELSE
                IF(KFF.EQ.5) WID2=WIDS(6,3)
                IF(KFF.EQ.7) WID2=WIDS(8,3)*WIDS(7,2)
                IF(KFF.EQ.17) WID2=WIDS(18,3)*WIDS(17,2)
              ENDIF
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=COMFAC*FCOI*FCOF*WFAC*WID2
              IF((MSTP(5).EQ.3.AND.IA.LE.2.AND.JA.LE.2).OR.MSTP(5).EQ.4)
     &        SIGH(NCHN)=COMFAC*FCOI*FCOF*WCIFAC*WID2
 1750       CONTINUE
 1760     CONTINUE
 
        ELSEIF(ISUB.EQ.167.OR.ISUB.EQ.168) THEN
C...q + q' -> q" + d* and q + q' -> q" + u* (excited quarks)
          KFQSTR=KFPR(ISUB,2)
          KCQSTR=PYCOMP(KFQSTR)
          KFQEXC=MOD(KFQSTR,KEXCIT)
          FACQSA=COMFAC*(SH/PARU(155)**2)**2*(1D0-SQM4/SH)
          FACQSB=COMFAC*0.25D0*(SH/PARU(155)**2)**2*(1D0-SQM4/SH)*
     &    (1D0+SQM4/SH)*(1D0+CTH)*(1D0+((SH-SQM4)/(SH+SQM4))*CTH)
C...Propagators: as simulated in PYOFSH and as desired
          GMMQ=PMAS(KCQSTR,1)*PMAS(KCQSTR,2)
          HBW4=GMMQ/((SQM4-PMAS(KCQSTR,1)**2)**2+GMMQ**2)
          CALL PYWIDT(KFQSTR,SQM4,WDTP,WDTE)
          GMMQC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMQC/((SQM4-PMAS(KCQSTR,1)**2)**2+GMMQC**2)
          FACQSA=FACQSA*HBW4C/HBW4
          FACQSB=FACQSB*HBW4C/HBW4
C...Branching ratios.
          BRPOS=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
          BRNEG=(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))/WDTP(0)
          DO 1780 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.6.OR.KFAC(1,I).EQ.0) GOTO 1780
            DO 1770 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.6.OR.KFAC(2,J).EQ.0) GOTO 1770
              IF(IA.EQ.KFQEXC.AND.I.EQ.J) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(I.GT.0) SIGH(NCHN)=(4D0/3D0)*FACQSA*BRPOS
                IF(I.LT.0) SIGH(NCHN)=(4D0/3D0)*FACQSA*BRNEG
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(J.GT.0) SIGH(NCHN)=(4D0/3D0)*FACQSA*BRPOS
                IF(J.LT.0) SIGH(NCHN)=(4D0/3D0)*FACQSA*BRNEG
              ELSEIF((IA.EQ.KFQEXC.OR.JA.EQ.KFQEXC).AND.I*J.GT.0) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(JA.EQ.KFQEXC) ISIG(NCHN,3)=2
                IF(ISIG(NCHN,ISIG(NCHN,3)).GT.0) SIGH(NCHN)=FACQSA*BRPOS
                IF(ISIG(NCHN,ISIG(NCHN,3)).LT.0) SIGH(NCHN)=FACQSA*BRNEG
              ELSEIF(IA.EQ.KFQEXC.AND.I.EQ.-J) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(I.GT.0) SIGH(NCHN)=(8D0/3D0)*FACQSB*BRPOS
                IF(I.LT.0) SIGH(NCHN)=(8D0/3D0)*FACQSB*BRNEG
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(J.GT.0) SIGH(NCHN)=(8D0/3D0)*FACQSB*BRPOS
                IF(J.LT.0) SIGH(NCHN)=(8D0/3D0)*FACQSB*BRNEG
              ELSEIF(I.EQ.-J) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(I.GT.0) SIGH(NCHN)=FACQSB*BRPOS
                IF(I.LT.0) SIGH(NCHN)=FACQSB*BRNEG
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(J.GT.0) SIGH(NCHN)=FACQSB*BRPOS
                IF(J.LT.0) SIGH(NCHN)=FACQSB*BRNEG
              ELSEIF(IA.EQ.KFQEXC.OR.JA.EQ.KFQEXC) THEN
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=1
                IF(JA.EQ.KFQEXC) ISIG(NCHN,3)=2
                IF(ISIG(NCHN,ISIG(NCHN,3)).GT.0) SIGH(NCHN)=FACQSB*BRPOS
                IF(ISIG(NCHN,ISIG(NCHN,3)).LT.0) SIGH(NCHN)=FACQSB*BRNEG
              ENDIF
 1770       CONTINUE
 1780     CONTINUE
 
        ELSEIF(ISUB.EQ.169) THEN
C...q + qbar -> e + e* (excited lepton)
          KFQSTR=KFPR(ISUB,2)
          KCQSTR=PYCOMP(KFQSTR)
          KFQEXC=MOD(KFQSTR,KEXCIT)
          FACQSB=(COMFAC/6D0)*(SH/PARU(155)**2)**2*(1D0-SQM4/SH)*
     &    (1D0+SQM4/SH)*(1D0+CTH)*(1D0+((SH-SQM4)/(SH+SQM4))*CTH)
C...Propagators: as simulated in PYOFSH and as desired
          GMMQ=PMAS(KCQSTR,1)*PMAS(KCQSTR,2)
          HBW4=GMMQ/((SQM4-PMAS(KCQSTR,1)**2)**2+GMMQ**2)
          CALL PYWIDT(KFQSTR,SQM4,WDTP,WDTE)
          GMMQC=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMQC/((SQM4-PMAS(KCQSTR,1)**2)**2+GMMQC**2)
          FACQSB=FACQSB*HBW4C/HBW4
C...Branching ratios.
          BRPOS=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
          BRNEG=(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))/WDTP(0)
          DO 1790 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.6.OR.KFAC(1,I).EQ.0) GOTO 1790
            J=-I
            JA=IABS(J)
            IF(J.EQ.0.OR.JA.GT.6.OR.KFAC(2,J).EQ.0) GOTO 1790
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=J
            ISIG(NCHN,3)=1
            IF(I.GT.0) SIGH(NCHN)=FACQSB*BRPOS
            IF(I.LT.0) SIGH(NCHN)=FACQSB*BRNEG
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=J
            ISIG(NCHN,3)=2
            IF(J.GT.0) SIGH(NCHN)=FACQSB*BRPOS
            IF(J.LT.0) SIGH(NCHN)=FACQSB*BRNEG
 1790     CONTINUE
 
        ELSEIF(ISUB.EQ.191) THEN
C...q + qbar -> rho_tc0.
          KCTC=PYCOMP(KTECHN+113)
          SQMRHT=PMAS(KCTC,1)**2
          CALL PYWIDT(KTECHN+113,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=12D0*COMFAC/((SH-SQMRHT)**2+HS**2)
          IF(ABS(SHR-PMAS(KCTC,1)).GT.PARP(48)*PMAS(KCTC,2)) FACBW=0D0
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/6D0)*(AEM**2/ALPRHT)*(SQMRHT**2/SH)
          XWRHT=(1D0-2D0*XW)/(4D0*XW*(1D0-XW))
          BWZR=XWRHT*SH*(SH-SQMZ)/((SH-SQMZ)**2+GMMZ**2)
          BWZI=XWRHT*SH*GMMZ/((SH-SQMZ)**2+GMMZ**2)
          DO 1800 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1800
            IA=IABS(I)
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.5D0*(VI+AI)
            VARI=0.5D0*(VI-AI)
            HI=HP*((EI+VALI*BWZR)**2+(VALI*BWZI)**2+
     &      (EI+VARI*BWZR)**2+(VARI*BWZI)**2)
            IF(IA.LE.10) HI=HI*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=HI*FACBW*HF
 1800     CONTINUE
 
        ELSEIF(ISUB.EQ.192) THEN
C...q + qbar' -> rho_tc+/-.
          KCTC=PYCOMP(KTECHN+213)
          SQMRHT=PMAS(KCTC,1)**2
          CALL PYWIDT(KTECHN+213,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=12D0*COMFAC/((SH-SQMRHT)**2+HS**2)
          IF(ABS(SHR-PMAS(KCTC,1)).GT.PARP(48)*PMAS(KCTC,2)) FACBW=0D0
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/6D0)*(AEM**2/ALPRHT)*(SQMRHT**2/SH)*
     &    (0.25D0/XW**2)*SH**2/((SH-SQMW)**2+GMMW**2)
          DO 1820 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1820
            IA=IABS(I)
            DO 1810 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1810
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1810
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 1810
              KCHR=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHR)/2)+WDTE(0,4))
              HI=HP
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)*FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*FACBW*HF
 1810       CONTINUE
 1820     CONTINUE
 
        ELSEIF(ISUB.EQ.193) THEN
C...q + qbar -> omega_tc0.
          KCTC=PYCOMP(KTECHN+223)
          SQMOMT=PMAS(KCTC,1)**2
          CALL PYWIDT(KTECHN+223,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=12D0*COMFAC/((SH-SQMOMT)**2+HS**2)
          IF(ABS(SHR-PMAS(KCTC,1)).GT.PARP(48)*PMAS(KCTC,2)) FACBW=0D0
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/6D0)*(AEM**2/ALPRHT)*(SQMOMT**2/SH)*
     &    (2D0*PARP(143)-1D0)**2
          BWZR=(0.5D0/(1D0-XW))*SH*(SH-SQMZ)/((SH-SQMZ)**2+GMMZ**2)
          BWZI=(0.5D0/(1D0-XW))*SH*GMMZ/((SH-SQMZ)**2+GMMZ**2)
          DO 1830 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1830
            IA=IABS(I)
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.5D0*(VI+AI)
            VARI=0.5D0*(VI-AI)
            HI=HP*((EI-VALI*BWZR)**2+(VALI*BWZI)**2+
     &      (EI-VARI*BWZR)**2+(VARI*BWZI)**2)
            IF(IA.LE.10) HI=HI*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=HI*FACBW*HF
 1830     CONTINUE
 
        ELSEIF(ISUB.EQ.194) THEN
C...f + fbar -> f' + fbar' via s-channel rho_tc and omega_tc.
          KFA=KFPR(ISUBSV,1)
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=AEM**2*COMFAC
          TANW=SQRT(PARU(102)/(1D0-PARU(102)))
          CT2W=(1D0-2D0*PARU(102))/(2D0*PARU(102)/TANW)
 
          QUPD=2D0*PARP(143)-1D0
          FAR=SQRT(AEM/ALPRHT)
          FAO=FAR*QUPD
          FZR=FAR*CT2W
          FZO=-FAO*TANW
          SFAR=FAR**2
          SFAO=FAO**2
          SFZR=FZR**2
          SFZO=FZO**2
          CALL PYWIDT(23,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(23,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+113,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+113),1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+223,SH,WDTP,WDTE)
          SSMO=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+223),1)**2/SH,WDTP(0)/SHR)
          DETD=(FAR*FZO-FAO*FZR)**2+SSMZ*SSMR*SSMO-SFZR*SSMO-
     $    SFZO*SSMR-SFAR*SSMO*SSMZ-SFAO*SSMR*SSMZ
          DAA=(-Sfzr*SSMO - Sfzo*SSMR + SSMO*SSMR*SSMZ)/DETD/SH
          DZZ=(-Sfar*SSMO - Sfao*SSMR + SSMO*SSMR)/DETD/SH
          DAZ=(far*fzr*SSMO + fao*fzo*SSMR)/DETD/SH
 
          XWRHT=1D0/(4D0*XW*(1D0-XW))
          KFF=IABS(KFPR(ISUB,1))
          EF=KCHG(KFF,1)/3D0
          AF=SIGN(1D0,EF+0.1D0)
          VF=AF-4D0*EF*XWV
          VALF=0.5D0*(VF+AF)
          VARF=0.5D0*(VF-AF)
          FCOF=1D0
          IF(KFF.LE.10) FCOF=3D0
 
          WID2=1D0
          IF(KFF.GE.6.AND.KFF.LE.8) WID2=WIDS(KFF,1)
          IF(KFF.EQ.17.OR.KFF.EQ.18) WID2=WIDS(KFF,1)
          DZZ=DZZ*DCMPLX(XWRHT,0D0)
          DAZ=DAZ*DCMPLX(SQRT(XWRHT),0D0)
 
          DO 1840 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1840
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.5D0*(VI+AI)
            VARI=0.5D0*(VI-AI)
            FCOI=FCOF
            IF(IABS(I).LE.10) FCOI=FCOI/3D0
            DIFLL=ABS(EI*EF*DAA+VALI*VALF*DZZ+DAZ*(EI*VALF+EF*VALI))**2
            DIFRR=ABS(EI*EF*DAA+VARI*VARF*DZZ+DAZ*(EI*VARF+EF*VARI))**2
            DIFLR=ABS(EI*EF*DAA+VALI*VARF*DZZ+DAZ*(EI*VARF+EF*VALI))**2
            DIFRL=ABS(EI*EF*DAA+VARI*VALF*DZZ+DAZ*(EI*VALF+EF*VARI))**2
            FACSIG=(DIFLL+DIFRR)*((UH-SQM4)**2+SH*SQM4)+
     &      (DIFLR+DIFRL)*((TH-SQM3)**2+SH*SQM3)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=HP*FCOI*FACSIG*WID2
 1840     CONTINUE
 
        ELSEIF(ISUB.EQ.195) THEN
C...f + fbar' -> f'' + fbar''' via s-channel rho_tc+
          KFA=KFPR(ISUBSV,1)
          KFB=KFA+1
          ALPRHT=2.91D0*(3D0/PARP(144))
          FACTC=COMFAC*(AEM**2/12D0/XW**2)*(UH-SQM3)*(UH-SQM4)*3D0
 
          FWR=SQRT(AEM/ALPRHT)/(2D0*SQRT(XW))
          CALL PYWIDT(24,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(24,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+213,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+213),1)**2/SH,WDTP(0)/SHR)
 
          FCOF=1D0
          IF(KFA.LE.8) FCOF=3D0
          DETD=SSMZ*SSMR-DCMPLX(FWR**2,0D0)
          HP=FACTC*ABS(SSMR/DETD)**2/SH**2*FCOF
 
          DO 1860 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1860
            IA=IABS(I)
            DO 1850 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1850
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1850
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 1850
              KCHR=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*WIDS(KFA,(5-KCHR)/2)*WIDS(KFB,(5+KCHR)/2)
 1850       CONTINUE
 1860     CONTINUE
 
        ENDIF
 
CMRENNA++
C...J: 2 -> 2, tree diagrams, SUSY processes
 
      ELSEIF(ISUB.LE.210) THEN
        IF(ISUB.EQ.201) THEN
C...f + fbar -> e_L + e_Lbar
          COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          DO 1890 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1890
            EI=KCHG(IA,1)/3D0
            TT3I=SIGN(1D0,EI+1D-6)/2D0
            EJ=-1D0
            TT3J=-1D0/2D0
            FCOL=1D0
C...Color factor for e+ e-
            IF(IA.GE.11) FCOL=3D0
            IF(ISUBSV.EQ.301) THEN
              A1=1D0
              A2=0D0
            ELSEIF(ILR.EQ.1) THEN
              A1=SFMIX(KFID,3)**2
              A2=SFMIX(KFID,4)**2
            ELSEIF(ILR.EQ.0) THEN
              A1=SFMIX(KFID,1)**2
              A2=SFMIX(KFID,2)**2
            ENDIF
            XLQ=(TT3J-EJ*XW)*A1
            XRQ=(-EJ*XW)*A2
            XLF=(TT3I-EI*XW)
            XRF=(-EI*XW)
            TAA=(EI*EJ)**2*(POLL+POLR)
            TZZ=(XLF**2*POLL+XRF**2*POLR)*(XLQ+XRQ)**2/XW**2/XW1**2
            TZZ=TZZ/((1D0-SQMZ/SH)**2+SQMZ*ZWID/SH**2)
            TAZ=2D0*EI*EJ*(XLQ+XRQ)*(XLF*POLL+XRF*POLR)/XW/XW1
            TAZ=TAZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)*(1D0-SQMZ/SH)
            TNN=0.0D0
            TAN=0.0D0
            TZN=0.0D0
            IF(IA.GE.11.AND.IA.LE.18.AND.KFID.EQ.IA) THEN
              FAC2=SQRT(2D0)
              TNN1=0D0
              TNN2=0D0
              TNN3=0D0
              DO 1880 II=1,4
                DK=1D0/(TH-SMZ(II)**2)
                FLEK=-FAC2*(TT3I*ZMIX(II,2)-TANW*(TT3I-EI)*
     &          ZMIX(II,1))
                FREK=FAC2*TANW*EI*ZMIX(II,1)
                TNN1=TNN1+FLEK**2*DK
                TNN2=TNN2+FREK**2*DK
                DO 1870 JJ=1,4
                  DL=1D0/(TH-SMZ(JJ)**2)
                  FLEL=-FAC2*(TT3J*ZMIX(JJ,2)-TANW*(TT3J-EJ)*
     &            ZMIX(JJ,1))
                  FREL=FAC2*TANW*EJ*ZMIX(JJ,1)
                  TNN3=TNN3+FLEK*FREK*FLEL*FREL*DK*DL*SMZ(II)*SMZ(JJ)
 1870           CONTINUE
 1880         CONTINUE
              TNN=(UH*TH-SQM3*SQM4)*(A1**2*TNN1**2*POLL+
     &        A2**2*TNN2**2*POLR)
              TNN=(TNN+SH*A1*A2*TNN3*((1D0-PARJ(131))*(1D0-PARJ(132))+
     &        (1D0+PARJ(131))*(1D0+PARJ(132))))/4D0/XW**2
              TZN=(UH*TH-SQM3*SQM4)*(XLQ+XRQ)*
     &        (TNN1*XLF*A1*POLL+TNN2*XRF*A2*POLR)
              TZN=TZN/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)*
     &        (1D0-SQMZ/SH)/SH
              TZN=TZN/XW**2/XW1
              TAN=EI*EJ*(UH*TH-SQM3*SQM4)/SH*(A1*TNN1*POLL+
     &        A2*TNN2*POLR)/XW
            ENDIF
            FACQQ1=COMFAC*AEM**2*(TAA+TZZ+TAZ)*FCOL/3D0
            FACQQ1=FACQQ1*( UH*TH-SQM3*SQM4 )/SH**2
            FACQQ2=COMFAC*AEM**2*(TNN+TZN+TAN)*FCOL/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ1+FACQQ2
 1890     CONTINUE
 
        ELSEIF(ISUB.EQ.203) THEN
C...f + fbar -> e_L + e_Rbar
          DO 1920 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1920
            EI=KCHG(IABS(I),1)/3D0
            TT3I=SIGN(1D0,EI)/2D0
            EJ=-1
            TT3J=-1D0/2D0
            FCOL=1D0
C...Color factor for e+ e-
            IF(IA.GE.11) FCOL=3D0
            A1=SFMIX(KFID,1)**2
            A2=SFMIX(KFID,2)**2
            XLQ=(TT3J-EJ*XW)
            XRQ=(-EJ*XW)
            XLF=(TT3I-EI*XW)
            XRF=(-EI*XW)
            TZZ=(XLF**2*POLL+XRF**2*POLR)*(XLQ-XRQ)**2
     &      /XW**2/XW1**2*A1*A2
            TZZ=TZZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)
            TNN=0.0D0
            TZN=0.0D0
            TNNA=0D0
            TNNB=0D0
            IF(IA.GE.11.AND.IA.LE.18.AND.KFID.EQ.IA) THEN
              FAC2=SQRT(2D0)
              TNN1=0D0
              TNN2=0D0
              TNN3=0D0
              DO 1910 II=1,4
                DK=1D0/(TH-SMZ(II)**2)
                FLEK=-FAC2*(TT3I*ZMIX(II,2)-TANW*(TT3I-EI)*
     &          ZMIX(II,1))
                FREK=FAC2*TANW*EI*ZMIX(II,1)
                TNN1=TNN1+FLEK**2*DK
                TNN2=TNN2+FREK**2*DK
                DO 1900 JJ=1,4
                  DL=1D0/(TH-SMZ(JJ)**2)
                  FLEL=-FAC2*(TT3J*ZMIX(JJ,2)-TANW*(TT3J-EJ)*
     &            ZMIX(JJ,1))
                  FREL=FAC2*TANW*EJ*ZMIX(JJ,1)
                  TNN3=TNN3+FLEK*FREK*FLEL*FREL*DK*DL*SMZ(II)*SMZ(JJ)
 1900           CONTINUE
 1910         CONTINUE
              TNN=(UH*TH-SQM3*SQM4)*A1*A2*(TNN2**2*POLR+TNN1**2*POLL)
              TNNA=(TNN+SH*(A1**2*POLLL+A2**2*POLRR)*TNN3)/4D0
              TNNB=(TNN+SH*(A1**2*POLRR+A2**2*POLLL)*TNN3)/4D0
              TZN=(UH*TH-SQM3*SQM4)*A1*A2
              TZN=TZN*(XLQ-XRQ)*(XLF*TNN1*POLL-XRF*TNN2*POLR)/XW1
              TZN=TZN/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)*
     &        (1D0-SQMZ/SH)/SH
            ENDIF
            FACQQ0=COMFAC*AEM**2*TZZ*FCOL/3D0*(UH*TH-SQM3*SQM4)/SH2
            FACQQ2=COMFAC*AEM**2/XW**2*(TNNA+TZN)*FCOL/3D0
            FACQQ1=COMFAC*AEM**2/XW**2*(TNNB+TZN)*FCOL/3D0
C%%%%%%%%%%%
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=(FACQQ0+FACQQ1)*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),3)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=2
            SIGH(NCHN)=(FACQQ0+FACQQ2)*WIDS(PYCOMP(KFPR(ISUBSV,1)),3)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
 1920     CONTINUE
 
        ELSEIF(ISUB.EQ.210) THEN
C...q + qbar' -> W*- > ~l_L + ~nu_L
          FAC0=RKF*COMFAC*AEM**2/XW**2/12D0
          FAC1=(TH*UH-SQM3*SQM4)/((SH-SQMW)**2+WWID**2*SQMW)
          DO 1940 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.10.OR.KFAC(1,I).EQ.0) GOTO 1940
            DO 1930 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.10.OR.KFAC(2,J).EQ.0) GOTO 1930
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1930
              FCKM=3D0
              IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
              KCHSUM=KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J)
              KCHW=2
              IF(KCHSUM.LT.0) KCHW=3
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              IF(ISUBSV.EQ.297.OR.ISUBSV.EQ.298) THEN
                FACR=WIDS(PYCOMP(KFPR(ISUBSV,1)),5-KCHW)*
     &          WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
              ELSE
                FACR=WIDS(PYCOMP(KFPR(ISUBSV,1)),5-KCHW)*
     &          WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHW)
              ENDIF
              SIGH(NCHN)=FAC0*FAC1*FCKM*FACR
 1930       CONTINUE
 1940     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.220) THEN
        IF(ISUB.EQ.213) THEN
C...f + fbar -> ~nu_L + ~nu_Lbar
          IF(ISUBSV.EQ.299.OR.ISUBSV.EQ.300) THEN
            FACR=WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
          ELSE
            FACR=WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          ENDIF
          COMFAC=COMFAC*FACR
          PROPZ2=(SH-SQMZ)**2+ZWID**2*SQMZ
          XLL=0.5D0
          XLR=0.0D0
          DO 1950 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1950
            EI=KCHG(IA,1)/3D0
            FCOL=1D0
C...Color factor for e+ e-
            IF(IA.GE.11) FCOL=3D0
            XLQ=(SIGN(1D0,EI)-2D0*EI*XW)/2D0
            XRQ=-EI*XW
            TZC=0.0D0
            TCC=0.0D0
            IF(IA.GE.11.AND.KFID.EQ.IA+1) THEN
              TZC=VMIX(1,1)**2/(TH-SMW(1)**2)+VMIX(2,1)**2/
     &        (TH-SMW(2)**2)
              TCC=TZC**2
              TZC=TZC/XW1*(SH-SQMZ)/PROPZ2*XLQ*XLL
            ENDIF
            FACQQ1=(XLQ**2+XRQ**2)*(XLL+XLR)**2/XW1**2/PROPZ2
            FACQQ2=TZC+TCC/4D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=(FACQQ1+FACQQ2)*RKF*(UH*TH-SQM3*SQM4)*COMFAC
     &      *AEM**2*FCOL/3D0/XW**2
 1950     CONTINUE
 
        ELSEIF(ISUB.EQ.216) THEN
C...q + qbar -> ~chi0_1 + ~chi0_1
          IF(IZID1.EQ.IZID2) THEN
            COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          ELSE
            COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
          ENDIF
          FACXX=COMFAC*AEM**2/3D0/XW**2
          IF(IZID1.EQ.IZID2) FACXX=FACXX/2D0
          ZM12=SQM3
          ZM22=SQM4
          WU2 = (UH-ZM12)*(UH-ZM22)
          WT2 = (TH-ZM12)*(TH-ZM22)
          WS2 = SMZ(IZID1)*SMZ(IZID2)*SH
          PROPZ2 = (SH-SQMZ)**2 + SQMZ*ZWID**2
          PROPZ=DCMPLX(SH-SQMZ,-ZWID*PMAS(23,1))/DCMPLX(PROPZ2)
          DO 1960 I=1,4
            ZMIXC(IZID1,I)=DCMPLX(ZMIX(IZID1,I),ZMIXI(IZID1,I))
            IF(IZID2.NE.IZID1) THEN
              ZMIXC(IZID2,I)=DCMPLX(ZMIX(IZID2,I),ZMIXI(IZID2,I))
            ENDIF
 1960     CONTINUE
          OLPP=(ZMIXC(IZID1,3)*DCONJG(ZMIXC(IZID2,3))-
     &    ZMIXC(IZID1,4)*DCONJG(ZMIXC(IZID2,4)))/2D0
          ORPP=DCONJG(OLPP)
          DO 1970 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1970
            EI=KCHG(IABS(I),1)/3D0
            T3I=SIGN(1D0,EI+1D-6)/2D0
            XML2=PMAS(PYCOMP(KSUSY1+IABS(I)),1)**2
            XMR2=PMAS(PYCOMP(KSUSY2+IABS(I)),1)**2
            GLIJ=(T3I*ZMIXC(IZID1,2)-TANW*(T3I-EI)*ZMIXC(IZID1,1))*
     &      DCONJG(T3I*ZMIXC(IZID2,2)-TANW*(T3I-EI)*ZMIXC(IZID2,1))
            GRIJ=ZMIXC(IZID1,1)*DCONJG(ZMIXC(IZID2,1))*(EI*TANW)**2
            QLL=DCMPLX((T3I-EI*XW)/XW1)*OLPP*PROPZ-GLIJ/DCMPLX(UH-XML2)
            QLR=-DCMPLX((T3I-EI*XW)/XW1)*ORPP*PROPZ+DCONJG(GLIJ)
     &      /DCMPLX(TH-XML2)
            QRL=-DCMPLX((EI*XW)/XW1)*OLPP*PROPZ+GRIJ/DCMPLX(TH-XMR2)
            QRR=DCMPLX((EI*XW)/XW1)*ORPP*PROPZ
     &      -DCONJG(GRIJ)/DCMPLX(UH-XMR2)
            FCOL=1D0
            IF(IABS(I).GE.11) FCOL=3D0
            FACGG1=(ABS(QLL)**2*POLL+ABS(QRR)**2*POLR)*WU2+
     &      (ABS(QRL)**2*POLR+ABS(QLR)**2*POLL)*WT2+
     &      2D0*DBLE(QLR*DCONJG(QLL)*POLL+
     &      QRL*DCONJG(QRR)*POLR)*WS2
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACXX*FACGG1*FCOL
 1970     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.230) THEN
        IF(ISUB.EQ.226) THEN
C...f + fbar -> ~chi+_1 + ~chi-_1
          FACXX=COMFAC*AEM**2/3D0
          ZM12=SQM3
          ZM22=SQM4
          WU2 = (UH-ZM12)*(UH-ZM22)
          WT2 = (TH-ZM12)*(TH-ZM22)
          WS2 = SMW(IZID1)*SMW(IZID2)*SH
          PROPZ2 = (SH-SQMZ)**2 + SQMZ*ZWID**2
          PROPZ=DCMPLX(SH-SQMZ,-ZWID*PMAS(23,1))/DCMPLX(PROPZ2)
          DIFF=0D0
          IF(IZID1.EQ.IZID2) DIFF=1D0
          DO 1980 I=1,2
            VMIXC(IZID1,I)=DCMPLX(VMIX(IZID1,I),VMIXI(IZID1,I))
            UMIXC(IZID1,I)=DCMPLX(UMIX(IZID1,I),UMIXI(IZID1,I))
            IF(IZID2.NE.IZID1) THEN
              VMIXC(IZID2,I)=DCMPLX(VMIX(IZID2,I),VMIXI(IZID2,I))
              UMIXC(IZID2,I)=DCMPLX(UMIX(IZID2,I),UMIXI(IZID2,I))
            ENDIF
 1980     CONTINUE
          OLP=-VMIXC(IZID2,1)*DCONJG(VMIXC(IZID1,1))-
     &    VMIXC(IZID2,2)*DCONJG(VMIXC(IZID1,2))/2D0+DCMPLX(XW*DIFF)
          ORP=-UMIXC(IZID1,1)*DCONJG(UMIXC(IZID2,1))-
     &    UMIXC(IZID1,2)*DCONJG(UMIXC(IZID2,2))/2D0+DCMPLX(XW*DIFF)
          DO 1990 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1990
            EI=KCHG(IABS(I),1)/3D0
            T3I=SIGN(1D0,EI+1D-6)/2D0
            QRL=DCMPLX(-EI/SH*DIFF)-DCMPLX(EI/XW1)*PROPZ*ORP
            QLL=DCMPLX(-EI/SH*DIFF)+DCMPLX((T3I-XW*EI)/XW/XW1)*PROPZ*ORP
            QRR=DCMPLX(-EI/SH*DIFF)-DCMPLX(EI/XW1)*PROPZ*OLP
            IF(MOD(I,2).EQ.0) THEN
              XML2=PMAS(PYCOMP(KSUSY1+IABS(I)-1),1)**2
              QLR=DCMPLX(-EI/SH*DIFF)+DCMPLX((T3I-XW*EI)/XW/XW1)*
     &        PROPZ*OLP-UMIXC(IZID2,1)*DCONJG(UMIXC(IZID1,1))*
     &        DCMPLX(T3I/XW/(TH-XML2))
            ELSE
              XML2=PMAS(PYCOMP(KSUSY1+IABS(I)+1),1)**2
              QLR=DCMPLX(-EI/SH*DIFF)+DCMPLX((T3I-XW*EI)/XW/XW1)*
     &        PROPZ*OLP-VMIXC(IZID2,1)*DCONJG(VMIXC(IZID1,1))*
     &        DCMPLX(T3I/XW/(TH-XML2))
            ENDIF
            FCOL=1D0
            IF(IABS(I).GE.11) FCOL=3D0
            FACSUM=((ABS(QLL)**2*POLL+ABS(QRR)**2*POLR)*WU2+
     &      (ABS(QRL)**2*POLR+ABS(QLR)**2*POLL)*WT2+
     &      2D0*DBLE(QLR*DCONJG(QLL)*POLL+
     &      QRL*DCONJG(QRR)*POLR)*WS2)*FACXX*FCOL
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            IF(IZID1.EQ.IZID2) THEN
              SIGH(NCHN)=FACSUM*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
            ELSE
              SIGH(NCHN)=FACSUM*WIDS(PYCOMP(KFPR(ISUBSV,1)),3)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=-I
              ISIG(NCHN,3)=2
              SIGH(NCHN)=FACSUM*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),3)
            ENDIF
 1990     CONTINUE
 
        ELSEIF(ISUB.EQ.229) THEN
C...q + qbar' -> ~chi0_1 + ~chi+-_1
          FACXX=COMFAC*AEM**2/6D0/XW**2
          ZM12=SQM3
          ZM22=SQM4
          WU2 = (UH-ZM12)*(UH-ZM22)
          WT2 = (TH-ZM12)*(TH-ZM22)
          WS2 = SMW(IZID1)*SMZ(IZID2)*SH
          RT2I = 1D0/SQRT(2D0)
          PROPW = DCMPLX(SH-SQMW,-WWID*PMAS(24,1))/
     &    DCMPLX((SH-SQMW)**2+WWID**2*SQMW,0D0)
          DO 2000 I=1,2
            VMIXC(IZID1,I)=DCMPLX(VMIX(IZID1,I),VMIXI(IZID1,I))
            UMIXC(IZID1,I)=DCMPLX(UMIX(IZID1,I),UMIXI(IZID1,I))
 2000     CONTINUE
          DO 2010 I=1,4
            ZMIXC(IZID2,I)=DCMPLX(ZMIX(IZID2,I),ZMIXI(IZID2,I))
 2010     CONTINUE
          OL=(DCONJG(ZMIXC(IZID2,2))*VMIXC(IZID1,1)-
     &    DCONJG(ZMIXC(IZID2,4))*VMIXC(IZID1,2)*RT2I)*PROPW
          OR=(ZMIXC(IZID2,2)*DCONJG(UMIXC(IZID1,1))+
     &    ZMIXC(IZID2,3)*DCONJG(UMIXC(IZID1,2))*RT2I)*PROPW
 
          DO 2030 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 2030
            EI=KCHG(IA,1)/3D0
            T3I=SIGN(1D0,EI+1D-6)/2D0
            DO 2020 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(2,J).EQ.0) GOTO 2020
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 2020
              EJ=KCHG(JA,1)/3D0
              T3J=SIGN(1D0,EJ+1D-6)/2D0
              FCKM=3D0
              IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
              KCHSUM=KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J)
              KCHW=2
              IF(KCHSUM.LT.0) KCHW=3
              IF(MOD(IA,2).EQ.0) THEN
                ZMI2  = PMAS(PYCOMP(KSUSY1+IA),1)**2
                ZMJ2  = PMAS(PYCOMP(KSUSY1+JA),1)**2
                QLL=OL+VMIXC(IZID1,1)*DCONJG(ZMIXC(IZID2,1)*(EI-T3I)*
     &          TANW+ZMIXC(IZID2,2)*T3I)/DCMPLX(UH-ZMI2)
                QLR=OR-DCONJG(UMIXC(IZID1,1))*(
     &          ZMIXC(IZID2,1)*(EJ-T3J)*TANW+ZMIXC(IZID2,2)*T3J)
     &          /DCMPLX(TH-ZMJ2)
              ELSE
                ZMI2  = PMAS(PYCOMP(KSUSY1+JA),1)**2
                ZMJ2  = PMAS(PYCOMP(KSUSY1+IA),1)**2
                QLL=OL+VMIXC(IZID1,1)*DCONJG(ZMIXC(IZID2,1)*(EJ-T3J)*
     &          TANW+ZMIXC(IZID2,2)*T3J)/DCMPLX(UH-ZMJ2)
                QLR=OR-DCONJG(UMIXC(IZID1,1))*(
     &          ZMIXC(IZID2,1)*(EI-T3I)*TANW+ZMIXC(IZID2,2)*T3I)
     &          /DCMPLX(TH-ZMI2)
              ENDIF
              ZINTR=DBLE(QLR*DCONJG(QLL))
              FACGG1=FACXX*(ABS(QLL)**2*WU2+ABS(QLR)**2*WT2+
     &        2D0*ZINTR*WS2)
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACGG1*FCKM*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHW)
 2020       CONTINUE
 2030     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.240) THEN
        IF(ISUB.EQ.237) THEN
C...q + qbar -> gluino + ~chi0_1
          COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &    WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
          FAC0=COMFAC*AS*AEM*4D0/9D0/XW
          GM2=SQM3
          ZM2=SQM4
          DO 2040 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 2040
            EI=KCHG(IABS(I),1)/3D0
            IA=IABS(I)
            XLQC = -TANW*EI*ZMIX(IZID,1)
            XRQC =(SIGN(1D0,EI)*ZMIX(IZID,2)-TANW*
     &      (SIGN(1D0,EI)-2D0*EI)*ZMIX(IZID,1))/2D0
            XLQ2=XLQC**2
            XRQ2=XRQC**2
            XML2=PMAS(PYCOMP(KSUSY1+IA),1)**2
            XMR2=PMAS(PYCOMP(KSUSY2+IA),1)**2
            ATKIN=(TH-GM2)*(TH-ZM2)/(TH-XML2)**2
            AUKIN=(UH-GM2)*(UH-ZM2)/(UH-XML2)**2
            ATUKIN=SMZ(IZID)*SQRT(GM2)*SH/(TH-XML2)/(UH-XML2)
            SGCHIL=XLQ2*(ATKIN+AUKIN-2D0*ATUKIN)
            ATKIN=(TH-GM2)*(TH-ZM2)/(TH-XMR2)**2
            AUKIN=(UH-GM2)*(UH-ZM2)/(UH-XMR2)**2
            ATUKIN=SMZ(IZID)*SQRT(GM2)*SH/(TH-XMR2)/(UH-XMR2)
            SGCHIR=XRQ2*(ATKIN+AUKIN-2D0*ATUKIN)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FAC0*(SGCHIL+SGCHIR)
 2040     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.250) THEN
        IF(ISUB.EQ.241) THEN
C...q + qbar' -> ~chi+-_1 + gluino
          FACWG=COMFAC*AS*AEM/XW*2D0/9D0
          GM2=SQM3
          ZM2=SQM4
          FAC01=2D0*UMIX(IZID,1)*VMIX(IZID,1)
          FAC0=UMIX(IZID,1)**2
          FAC1=VMIX(IZID,1)**2
          DO 2060 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.10.OR.KFAC(1,I).EQ.0) GOTO 2060
            DO 2050 J=MMIN2,MMAX2
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.10.OR.KFAC(2,J).EQ.0) GOTO 2050
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 2050
              FCKM=1D0
              IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
              KCHSUM=KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J)
              KCHW=2
              IF(KCHSUM.LT.0) KCHW=3
              XMU2=PMAS(PYCOMP(KSUSY1+2),1)**2
              XMD2=PMAS(PYCOMP(KSUSY1+1),1)**2
              ATKIN=(TH-GM2)*(TH-ZM2)/(TH-XMU2)**2
              AUKIN=(UH-GM2)*(UH-ZM2)/(UH-XMD2)**2
              ATUKIN=SMW(IZID)*SQRT(GM2)*SH/(TH-XMU2)/(UH-XMD2)
              XMU2=PMAS(PYCOMP(KSUSY2+2),1)**2
              XMD2=PMAS(PYCOMP(KSUSY2+1),1)**2
              ATKIN=(ATKIN+(TH-GM2)*(TH-ZM2)/(TH-XMU2)**2)/2D0
              AUKIN=(AUKIN+(UH-GM2)*(UH-ZM2)/(UH-XMD2)**2)/2D0
              ATUKIN=(ATUKIN+SMW(IZID)*SQRT(GM2)*
     &        SH/(TH-XMU2)/(UH-XMD2))/2D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACWG*FCKM*(FAC0*ATKIN+FAC1*AUKIN-
     &        FAC01*ATUKIN)*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHW)
 2050       CONTINUE
 2060     CONTINUE
 
        ELSEIF(ISUB.EQ.243) THEN
C...q + qbar -> gluino + gluino
          COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          XMT=SQM3-TH
          XMU=SQM3-UH
          DO 2070 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2070
            NCHN=NCHN+1
            XSU=PMAS(PYCOMP(KSUSY1+IABS(I)),1)**2-UH
            XST=PMAS(PYCOMP(KSUSY1+IABS(I)),1)**2-TH
            FACGG1=COMFAC*AS**2*8D0/3D0*( (XMT**2+XMU**2+
     &      2D0*SQM3*SH)/SH2 +4D0/9D0*(XMT**2/XST**2+
     &      XMU**2/XSU**2) - (XMT**2+SH*SQM3)/SH/XST +
     &      SQM3*SH/XST/XSU/9D0- (XMU**2+SH*SQM3)/SH/XSU )
            XSU=PMAS(PYCOMP(KSUSY2+IABS(I)),1)**2-UH
            XST=PMAS(PYCOMP(KSUSY2+IABS(I)),1)**2-TH
            FACGG2=COMFAC*AS**2*8D0/3D0*( (XMT**2+XMU**2+
     &      2D0*SQM3*SH)/SH2 +4D0/9D0*(XMT**2/XST**2+
     &      XMU**2/XSU**2) - (XMT**2+SH*SQM3)/SH/XST +
     &      SQM3*SH/XST/XSU/9D0- (XMU**2+SH*SQM3)/SH/XSU )
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
C...1/2 for identical particles
            SIGH(NCHN)=0.25D0*(FACGG1+FACGG2)
 2070     CONTINUE
 
        ELSEIF(ISUB.EQ.244) THEN
C...g + g -> gluino + gluino
          COMFAC=COMFAC*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          XMT=SQM3-TH
          XMU=SQM3-UH
          FACQQ1=COMFAC*AS**2*9D0/4D0*(
     &    (XMT*XMU-2D0*SQM3*(TH+SQM3))/XMT**2 -
     &    (XMT*XMU+SQM3*(UH-TH))/SH/XMT )
          FACQQ2=COMFAC*AS**2*9D0/4D0*(
     &    (XMU*XMT-2D0*SQM3*(UH+SQM3))/XMU**2 -
     &    (XMU*XMT+SQM3*(TH-UH))/SH/XMU )
          FACQQ3=COMFAC*AS**2*9D0/4D0*(2D0*XMT*XMU/SH2 +
     &    SQM3*(SH-4D0*SQM3)/XMT/XMU)
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 2080
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ1/2D0
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQQ2/2D0
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=3
          SIGH(NCHN)=FACQQ3/2D0
 2080     CONTINUE
 
        ELSEIF(ISUB.EQ.246) THEN
C...g + q_j -> ~chi0_1 + ~q_j
          FAC0=COMFAC*AS*AEM/6D0/XW
          ZM2=SQM4
          QM2=SQM3
          FACZQ0=FAC0*( (ZM2-TH)/SH +
     &    (UH-ZM2)*(UH+QM2)/(UH-QM2)**2 -
     &    (SH*(UH+ZM2)+2D0*(QM2-ZM2)*(ZM2-UH))/SH/(UH-QM2) )
          KFNSQ=MOD(KFPR(ISUBSV,1),KSUSY1)
          DO 2100 I=-KFNSQ,KFNSQ,2*KFNSQ
            IF(I.LT.MMINA.OR.I.GT.MMAXA) GOTO 2100
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 2100
            EI=KCHG(IABS(I),1)/3D0
            IA=IABS(I)
            XRQZ = -TANW*EI*ZMIX(IZID,1)
            XLQZ =(SIGN(1D0,EI)*ZMIX(IZID,2)-TANW*
     &      (SIGN(1D0,EI)-2D0*EI)*ZMIX(IZID,1))/2D0
            IF(ILR.EQ.0) THEN
              BS=XLQZ**2*SFMIX(IA,1)**2+XRQZ**2*SFMIX(IA,2)**2
            ELSE
              BS=XLQZ**2*SFMIX(IA,3)**2+XRQZ**2*SFMIX(IA,4)**2
            ENDIF
            FACZQ=FACZQ0*BS
            KCHQ=2
            IF(I.LT.0) KCHQ=3
            DO 2090 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 2090
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 2090
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZQ*RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
 2090       CONTINUE
 2100     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.260) THEN
        IF(ISUB.EQ.254) THEN
C...g + q_j -> ~chi1_1 + ~q_i
          FAC0=COMFAC*AS*AEM/12D0/XW
          ZM2=SQM4
          QM2=SQM3
          AU=UMIX(IZID,1)**2
          AD=VMIX(IZID,1)**2
          FACZQ0=FAC0*( (ZM2-TH)/SH +
     &    (UH-ZM2)*(UH+QM2)/(UH-QM2)**2 -
     &    (SH*(UH+ZM2)+2D0*(QM2-ZM2)*(ZM2-UH))/SH/(UH-QM2) )
          KFNSQ1=MOD(KFPR(ISUBSV,1),KSUSY1)
          IF(MOD(KFNSQ1,2).EQ.0) THEN
            KFNSQ=KFNSQ1-1
            KCHW=2
          ELSE
            KFNSQ=KFNSQ1+1
            KCHW=3
          ENDIF
          DO 2120 I=-KFNSQ,KFNSQ,2*KFNSQ
            IF(I.LT.MMINA.OR.I.GT.MMAXA) GOTO 2120
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 2120
            IA=IABS(I)
            IF(MOD(IA,2).EQ.0) THEN
              FACZQ=FACZQ0*AU
            ELSE
              FACZQ=FACZQ0*AD
            ENDIF
            FACZQ=FACZQ*SFMIX(KFNSQ1,1+2*ILR)**2
            KCHQ=2
            IF(I.LT.0) KCHQ=3
            KCHWQ=KCHW
            IF(I.LT.0) KCHWQ=5-KCHW
            DO 2110 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 2110
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 2110
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACZQ*RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHWQ)
 2110       CONTINUE
 2120     CONTINUE
 
        ELSEIF(ISUB.EQ.258) THEN
C...g + q_j -> gluino + ~q_i
          XG2=SQM4
          XQ2=SQM3
          XMT=XG2-TH
          XMU=XG2-UH
          XST=XQ2-TH
          XSU=XQ2-UH
          FACQG1=0.5D0*4D0/9D0*XMT/SH + (XMT*SH+2D0*XG2*XST)/XMT**2 -
     &    ( (SH-XQ2+XG2)*(-XST)-SH*XG2 )/SH/(-XMT) +
     &    0.5D0*1D0/2D0*( XST*(TH+2D0*UH+XG2)-XMT*(SH-2D0*XST) +
     &    (-XMU)*(TH+XG2+2D0*XQ2) )/2D0/XMT/XSU
          FACQG2= 4D0/9D0*(-XMU)*(UH+XQ2)/XSU**2 + 1D0/18D0*
     &    (SH*(UH+XG2)
     &    +2D0*(XQ2-XG2)*XMU)/SH/(-XSU) + 0.5D0*4D0/9D0*XMT/SH +
     &    0.5D0*1D0/2D0*(XST*(TH+2D0*UH+XG2)-XMT*(SH-2D0*XST)+
     &    (-XMU)*(TH+XG2+2D0*XQ2))/2D0/XMT/XSU
          FACQG1=COMFAC*AS**2*FACQG1/2D0
          FACQG2=COMFAC*AS**2*FACQG2/2D0
          KFNSQ=MOD(KFPR(ISUBSV,1),KSUSY1)
          DO 2140 I=-KFNSQ,KFNSQ,2*KFNSQ
            IF(I.LT.MMINA.OR.I.GT.MMAXA) GOTO 2140
            IF(I.EQ.0.OR.IABS(I).GT.10) GOTO 2140
            KCHQ=2
            IF(I.LT.0) KCHQ=3
            FACSEL=RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
            DO 2130 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 2130
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 2130
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACQG1*FACSEL
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=2
              SIGH(NCHN)=FACQG2*FACSEL
 2130       CONTINUE
 2140     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.270) THEN
        IF(ISUB.EQ.261) THEN
C...q_i + q_ibar -> ~t_1 + ~t_1bar
          FACQQ1=COMFAC*( (UH*TH-SQM3*SQM4)/ SH**2 )*
     &    WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          KFNSQ=MOD(KFPR(ISUBSV,1),KSUSY1)
          FAC0=AS**2*4D0/9D0
          DO 2150 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2150
            IF(IA.GE.11.AND.IA.LE.18) THEN
              EI=KCHG(IA,1)/3D0
              EJ=KCHG(KFNSQ,1)/3D0
              T3I=SIGN(1D0,EI)/2D0
              T3J=SIGN(1D0,EJ)/2D0
              XLQ=2D0*(T3J-EJ*XW)*SFMIX(KFNSQ,2*ILR+1)**2
              XRQ=2D0*(-EJ*XW)*SFMIX(KFNSQ,2*ILR+2)**2
              XLF=2D0*(T3I-EI*XW)
              XRF=2D0*(-EI*XW)
              TAA=0.5D0*(EI*EJ)**2
              TZZ=(XLF**2+XRF**2)*(XLQ+XRQ)**2/64D0/XW**2/XW1**2
              TZZ=TZZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)
              TAZ=EI*EJ*(XLQ+XRQ)*(XLF+XRF)/8D0/XW/XW1
              TAZ=TAZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)*(1D0-SQMZ/SH)
              FAC0=AEM**2*12D0*(TAA+TZZ+TAZ)
            ENDIF
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ1*FAC0
 2150     CONTINUE
 
        ELSEIF(ISUB.EQ.263) THEN
C...f + fbar -> ~t1 + ~t2bar
          DO 2160 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2160
            EI=KCHG(IABS(I),1)/3D0
            TT3I=SIGN(1D0,EI)/2D0
            EJ=2D0/3D0
            TT3J=1D0/2D0
            FCOL=1D0
C...Color factor for e+ e-
            IF(IA.GE.11) FCOL=3D0
            XLQ=2D0*(TT3J-EJ*XW)
            XRQ=2D0*(-EJ*XW)
            XLF=2D0*(TT3I-EI*XW)
            XRF=2D0*(-EI*XW)
            TZZ=(XLF**2+XRF**2)*(XLQ-XRQ)**2/64D0/XW**2/XW1**2
            TZZ=TZZ*(SFMIX(6,1)*SFMIX(6,2))**2
            TZZ=TZZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)
C...Factor of 2 for t1 t2bar + t2 t1bar
            FACQQ1=2D0*COMFAC*AEM**2*TZZ*FCOL*4D0
            FACQQ1=FACQQ1*( UH*TH-SQM3*SQM4 )/SH2
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ1*WIDS(PYCOMP(KFPR(ISUBSV,1)),2)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),3)
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=2
            SIGH(NCHN)=FACQQ1*WIDS(PYCOMP(KFPR(ISUBSV,1)),3)*
     &      WIDS(PYCOMP(KFPR(ISUBSV,2)),2)
 2160     CONTINUE
 
        ELSEIF(ISUB.EQ.264) THEN
C...g + g -> ~t_1 + ~t_1bar
          XSU=SQM3-UH
          XST=SQM3-TH
          FAC0=COMFAC*AS**2*(7D0/48D0+3D0*(UH-TH)**2/16D0/SH2 )*0.5D0*
     &    WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          FACQQ1=FAC0*(0.5D0+2D0*SQM3*TH/XST**2 + 2D0*SQM3**2/XSU/XST)
          FACQQ2=FAC0*(0.5D0+2D0*SQM3*UH/XSU**2 + 2D0*SQM3**2/XSU/XST)
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 2170
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ1
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQQ2
 2170     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.280) THEN
        IF(ISUB.EQ.271) THEN
C...q + q' -> ~q + ~q' (~g exchange)
          XMG2=PMAS(PYCOMP(KSUSY1+21),1)**2
          XMT=XMG2-TH
          XMU=XMG2-UH
          XSU1=SQM3-UH
          XSU2=SQM4-UH
          XST1=SQM3-TH
          XST2=SQM4-TH
          IF(ILR.EQ.1) THEN
            FACQQ1=COMFAC*AS**2*4D0/9D0*( -(XST1*XST2+SH*TH)/XMT**2 )
            FACQQ2=COMFAC*AS**2*4D0/9D0*( -(XSU1*XSU2+SH*UH)/XMU**2 )
            FACQQB=0.0D0
          ELSE
            FACQQ1=0.5D0*COMFAC*AS**2*4D0/9D0*( SH*XMG2/XMT**2 )
            FACQQ2=0.5D0*COMFAC*AS**2*4D0/9D0*( SH*XMG2/XMU**2 )
            FACQQB=0.5D0*COMFAC*AS**2*4D0/9D0*( -2D0*SH*XMG2/3D0/
     &      XMT/XMU )
          ENDIF
          KFNSQI=MOD(KFPR(ISUBSV,1),KSUSY1)
          KFNSQJ=MOD(KFPR(ISUBSV,2),KSUSY1)
          DO 2190 I=-KFNSQI,KFNSQI,2*KFNSQI
            IF(I.LT.MMIN1.OR.I.GT.MMAX1) GOTO 2190
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.MSTP(58).OR.KFAC(1,I).EQ.0) GOTO 2190
            KCHQ=2
            IF(I.LT.0) KCHQ=3
            DO 2180 J=-KFNSQJ,KFNSQJ,2*KFNSQJ
              IF(J.LT.MMIN2.OR.J.GT.MMAX2) GOTO 2180
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.MSTP(58).OR.KFAC(2,J).EQ.0) GOTO 2180
              IF(I*J.LT.0) GOTO 2180
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACQQ1*RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHQ)
              IF(I.EQ.J) THEN
                IF(ILR.EQ.0) THEN
                  SIGH(NCHN)=0.5D0*(FACQQ1+0.5D0*FACQQB)*RKF*
     &            WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ+2)
                ELSE
                  SIGH(NCHN)=0.5D0*FACQQ1*RKF*
     &            WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &            WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHQ)
                ENDIF
                NCHN=NCHN+1
                ISIG(NCHN,1)=I
                ISIG(NCHN,2)=J
                ISIG(NCHN,3)=2
                IF(ILR.EQ.0) THEN
                  SIGH(NCHN)=0.5D0*(FACQQ2+0.5D0*FACQQB)*RKF*
     &            WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ+2)
                ELSE
                  SIGH(NCHN)=0.5D0*FACQQ2*RKF*
     &            WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &            WIDS(PYCOMP(KFPR(ISUBSV,2)),KCHQ)
                ENDIF
              ENDIF
 2180       CONTINUE
 2190     CONTINUE
 
        ELSEIF(ISUB.EQ.274) THEN
C...q + qbar' -> ~q + ~qbar'
          XMG2=PMAS(PYCOMP(KSUSY1+21),1)**2
          XMT=XMG2-TH
          XMU=XMG2-UH
          IF(ILR.EQ.0) THEN
C...Mrenna...Normalization.and.1/XMT
            FACQQ1=COMFAC*AS**2*2D0/9D0*(
     &      (UH*TH-SQM3*SQM4)/XMT**2 )
            FACQQB=COMFAC*AS**2*2D0/9D0*(
     &      (UH*TH-SQM3*SQM4)/SH2*(2D0-2D0/3D0*SH/XMT))
            FACQQB=FACQQB+FACQQ1
          ELSE
            FACQQ1=COMFAC*AS**2*4D0/9D0*( XMG2*SH/XMT**2 )
            FACQQB=FACQQ1
          ENDIF
          KFNSQI=MOD(KFPR(ISUBSV,1),KSUSY1)
          KFNSQJ=MOD(KFPR(ISUBSV,2),KSUSY1)
          DO 2210 I=-KFNSQI,KFNSQI,2*KFNSQI
            IF(I.LT.MMIN1.OR.I.GT.MMAX1) GOTO 2210
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.MSTP(58).OR.KFAC(1,I).EQ.0) GOTO 2210
            KCHQ=2
            IF(I.LT.0) KCHQ=3
            DO 2200 J=-KFNSQJ,KFNSQJ,2*KFNSQJ
              IF(J.LT.MMIN2.OR.J.GT.MMAX2) GOTO 2200
              JA=IABS(J)
              IF(J.EQ.0.OR.JA.GT.MSTP(58).OR.KFAC(2,J).EQ.0) GOTO 2200
              IF(I*J.GT.0) GOTO 2200
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACQQ1*RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),KCHQ)*
     &        WIDS(PYCOMP(KFPR(ISUBSV,2)),5-KCHQ)
              IF(I.EQ.-J) SIGH(NCHN)=FACQQB*RKF*
     &        WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
 2200       CONTINUE
 2210     CONTINUE
 
        ELSEIF(ISUB.EQ.277) THEN
C...q_i + q_ibar -> ~q_j + ~q_jbar ,i .ne. j
C...if i .eq. j covered in 274
          FACQQ1=COMFAC*( (UH*TH-SQM3*SQM4)/ SH**2 )
          KFNSQ=MOD(KFPR(ISUBSV,1),KSUSY1)
          FAC0=0D0
          DO 2220 I=MMIN1,MMAX1
            IA=IABS(I)
            IF(I.EQ.0.OR.IA.GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2220
            IF(IA.EQ.KFNSQ) GOTO 2220
            IF(IA.EQ.11.OR.IA.EQ.13.OR.IA.EQ.15) THEN
              EI=KCHG(IA,1)/3D0
              EJ=KCHG(KFNSQ,1)/3D0
              T3J=SIGN(0.5D0,EJ)
              T3I=SIGN(1D0,EI)/2D0
              IF(ILR.EQ.0) THEN
                XLQ=2D0*(T3J-EJ*XW)*SFMIX(KFNSQ,1)
                XRQ=2D0*(-EJ*XW)*SFMIX(KFNSQ,2)
              ELSE
                XLQ=2D0*(T3J-EJ*XW)*SFMIX(KFNSQ,3)
                XRQ=2D0*(-EJ*XW)*SFMIX(KFNSQ,4)
              ENDIF
              XLF=2D0*(T3I-EI*XW)
              XRF=2D0*(-EI*XW)
              IF(ILR.EQ.0) THEN
                XRQ=0D0
              ELSE
                XLQ=0D0
              ENDIF
              TAA=0.5D0*(EI*EJ)**2
              TZZ=(XLF**2+XRF**2)*(XLQ+XRQ)**2/64D0/XW**2/XW1**2
              TZZ=TZZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)
              TAZ=EI*EJ*(XLQ+XRQ)*(XLF+XRF)/8D0/XW/XW1
              TAZ=TAZ/((1D0-SQMZ/SH)**2+SQMZ*(ZWID/SH)**2)*(1D0-SQMZ/SH)
              FAC0=AEM**2*12D0*(TAA+TZZ+TAZ)
            ELSEIF(IA.LE.6) THEN
              FAC0=AS**2*8D0/9D0/2D0
            ENDIF
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACQQ1*FAC0*RKF*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
 2220     CONTINUE
 
        ELSEIF(ISUB.EQ.279) THEN
C...g + g -> ~q_j + ~q_jbar
          XSU=SQM3-UH
          XST=SQM3-TH
C...5=RKF because ~t ~tbar treated separately
          FAC0=RKF*COMFAC*AS**2*( 7D0/48D0+3D0*(UH-TH)**2/16D0/SH2 )
          FACQQ1=FAC0*(0.5D0+2D0*SQM3*TH/XST**2 + 2D0*SQM3**2/XSU/XST)
          FACQQ2=FAC0*(0.5D0+2D0*SQM3*UH/XSU**2 + 2D0*SQM3**2/XSU/XST)
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 2230
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ1/2D0*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQQ2/2D0*WIDS(PYCOMP(KFPR(ISUBSV,1)),1)
 2230     CONTINUE
 
        ENDIF
CMRENNA--
 
      ELSEIF(ISUB.LE.340) THEN
 
      ELSEIF(ISUB.LE.360) THEN
 
        IF(ISUB.EQ.341.OR.ISUB.EQ.342) THEN
C...l + l -> H_L++/-- or H_R++/--.
          KFRES=KFPR(ISUB,1)
          KFREC=PYCOMP(KFRES)
          CALL PYWIDT(KFRES,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=8D0*COMFAC/((SH-PMAS(KFREC,1)**2)**2+HS**2)
          DO 2250 I=MMIN1,MMAX1
            IA=IABS(I)
            IF((IA.NE.11.AND.IA.NE.13.AND.IA.NE.15).OR.KFAC(1,I).EQ.0)
     &      GOTO 2250
            DO 2240 J=MMIN2,MMAX2
              JA=IABS(J)
              IF((JA.NE.11.AND.JA.NE.13.AND.JA.NE.15).OR.KFAC(2,J).EQ.0)
     &        GOTO 2240
              IF(I*J.LT.0) GOTO 2240
              KCHH=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              HI=SH*PARP(181+3*((IA-11)/2)+(JA-11)/2)**2/(8D0*PARU(1))
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHH/2)/2)+WDTE(0,4))
              SIGH(NCHN)=HI*FACBW*HF
 2240       CONTINUE
 2250     CONTINUE
 
        ELSEIF(ISUB.GE.343.AND.ISUB.LE.348) THEN
C...l + gamma -> H_L++/-- l' or l + gamma -> H_R++/-- l'.
          KFRES=KFPR(ISUB,1)
          KFREC=PYCOMP(KFRES)
C...Propagators: as simulated in PYOFSH and as desired
          HBW3=PMAS(KFREC,1)*PMAS(KFREC,2)/((SQM3-PMAS(KFREC,1)**2)**2+
     &    (PMAS(KFREC,1)*PMAS(KFREC,2))**2)
          CALL PYWIDT(KFRES,SQM3,WDTP,WDTE)
          GMMC=SQRT(SQM3)*WDTP(0)
          HBW3C=GMMC/((SQM3-PMAS(KFREC,1)**2)**2+GMMC**2)
          FHCC=COMFAC*AEM*HBW3C/HBW3
          DO 2270 I=MMINA,MMAXA
            IA=IABS(I)
            IF(IA.NE.11.AND.IA.NE.13.AND.IA.NE.15) GOTO 2270
            SQML=PMAS(IA,1)**2
            J=ISIGN(KFPR(ISUB,2),-I)
            KCHH=ISIGN(2,KCHG(IA,1)*ISIGN(1,I))
            WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHH/2)/2)+WDTE(0,4))/WDTP(0)
            SMM1=8D0*(SH+TH-SQM3)*(SH+TH-2D0*SQM3-SQML-SQM4)/
     &      (UH-SQM3)**2
            SMM2=2D0*((2D0*SQM3-3D0*SQML)*SQM4+(SQML-2D0*SQM4)*TH-
     &      (TH-SQM4)*SH)/(TH-SQM4)**2
            SMM3=2D0*((2D0*SQM3-3D0*SQM4+TH)*SQML-(2D0*SQML-SQM4+TH)*
     &      SH)/(SH-SQML)**2
            SMM12=4D0*((2D0*SQML-SQM4-2D0*SQM3+TH)*SH+(TH-3D0*SQM3-
     &      3D0*SQM4)*TH+(2D0*SQM3-2D0*SQML+3D0*SQM4)*SQM3)/
     &      ((UH-SQM3)*(TH-SQM4))
            SMM13=-4D0*((TH+SQML-2D0*SQM4)*TH-(SQM3+3D0*SQML-2D0*SQM4)*
     &      SQM3+(SQM3+3D0*SQML+TH)*SH-(TH-SQM3+SH)**2)/
     &      ((UH-SQM3)*(SH-SQML))
            SMM23=-4D0*((SQML-SQM4+SQM3)*TH-SQM3**2+SQM3*(SQML+SQM4)-
     &      3D0*SQML*SQM4-(SQML-SQM4-SQM3+TH)*SH)/
     &      ((SH-SQML)*(TH-SQM4))
            SMM=(SH/(SH-SQML))**2*(SMM1+SMM2+SMM3+SMM12+SMM13+SMM23)*
     &      PARP(181+3*((IA-11)/2)+(IABS(J)-11)/2)**2/(4D0*PARU(1))
            DO 2260 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 2260
              IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 2260
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=22
              ISIG(NCHN,3)=0
              SIGH(NCHN)=FHCC*SMM*WIDSC
 2260       CONTINUE
 2270     CONTINUE
 
        ELSEIF(ISUB.EQ.349.OR.ISUB.EQ.350) THEN
C...f + fbar -> H_L++ + H_L-- or H_R++ + H_R--
          KFRES=KFPR(ISUB,1)
          KFREC=PYCOMP(KFRES)
          SQMH=PMAS(KFREC,1)**2
          GMMH=PMAS(KFREC,1)*PMAS(KFREC,2)
C...Propagators: H++/-- as simulated in PYOFSH and as desired
          HBW3=GMMH/((SQM3-SQMH)**2+GMMH**2)
          CALL PYWIDT(KFRES,SQM3,WDTP,WDTE)
          GMMH3=SQRT(SQM3)*WDTP(0)
          HBW3C=GMMH3/((SQM3-SQMH)**2+GMMH3**2)
          HBW4=GMMH/((SQM4-SQMH)**2+GMMH**2)
          CALL PYWIDT(KFRES,SQM4,WDTP,WDTE)
          GMMH4=SQRT(SQM4)*WDTP(0)
          HBW4C=GMMH4/((SQM4-SQMH)**2+GMMH4**2)
C...Kinematical and coupling functions
          FACHH=COMFAC*(HBW3C/HBW3)*(HBW4C/HBW4)*(TH*UH-SQM3*SQM4)
          XWHH=(1D0-2D0*XWV)/(8D0*XWV*(1D0-XWV))
C...Loop over allowed flavours
          DO 2280 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2280
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            FCOI=1D0
            IF(IABS(I).LE.10) FCOI=FACA/3D0
            IF(ISUB.EQ.349) THEN
              HBWZ=1D0/((SH-SQMZ)**2+GMMZ**2)
              IF(IABS(I).LT.10) THEN
                DSIGHH=8D0*AEM**2*(EI**2/SH2+
     &          2D0*EI*VI*XWHH*(SH-SQMZ)*HBWZ/SH+
     &          (VI**2+AI**2)*XWHH**2*HBWZ)
              ELSE
                IAOFF=181+3*((IABS(I)-11)/2)
                HSUM=(PARP(IAOFF)**2+PARP(IAOFF+1)**2+PARP(IAOFF+2)**2)/
     &          (4D0*PARU(1))
                DSIGHH=8D0*AEM**2*(EI**2/SH2+
     &          2D0*EI*VI*XWHH*(SH-SQMZ)*HBWZ/SH+
     &          (VI**2+AI**2)*XWHH**2*HBWZ)+
     &          8D0*AEM*(EI*HSUM/(SH*TH)+
     &          (VI+AI)*XWHH*HSUM*(SH-SQMZ)*HBWZ/TH)+
     &          4D0*HSUM**2/TH2
              ENDIF
            ELSE
              IF(IABS(I).LT.10) THEN
                DSIGHH=8D0*AEM**2*EI**2/SH2
              ELSE
                IAOFF=181+3*((IABS(I)-11)/2)
                HSUM=(PARP(IAOFF)**2+PARP(IAOFF+1)**2+PARP(IAOFF+2)**2)/
     &          (4D0*PARU(1))
                DSIGHH=8D0*AEM**2*EI**2/SH2+8D0*AEM*EI*HSUM/(SH*TH)+
     &          4D0*HSUM**2/TH2
              ENDIF
            ENDIF
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACHH*FCOI*DSIGHH
 2280     CONTINUE
 
        ELSEIF(ISUB.EQ.351.OR.ISUB.EQ.352) THEN
C...f + f' -> f" + f"' + H++/-- (W+/- + W+/- -> H++/-- as inner process)
          KFRES=KFPR(ISUB,1)
          KFREC=PYCOMP(KFRES)
          SQMH=PMAS(KFREC,1)**2
          IF(ISUB.EQ.351) FACNOR=PARP(190)**8*PARP(192)**2
          IF(ISUB.EQ.352) FACNOR=PARP(191)**6*2D0*
     &    PMAS(PYCOMP(9900024),1)**2
          FACWW=COMFAC*FACNOR*TAUP*VINT(2)*VINT(219)
          FACPRT=1D0/((VINT(204)**2-VINT(215))*
     &    (VINT(209)**2-VINT(216)))
          FACPRU=1D0/((VINT(204)**2+2D0*VINT(217))*
     &    (VINT(209)**2+2D0*VINT(218)))
          CALL PYWIDT(KFRES,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=(1D0/PARU(1))*VINT(2)/((SH-SQMH)**2+HS**2)
          IF(ABS(SHR-PMAS(KFREC,1)).GT.PARP(48)*PMAS(KFREC,2))
     &    FACBW=0D0
          DO 2300 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 2300
            IF(ISUB.EQ.352.AND.IABS(I).GT.10) GOTO 2300
            KCHWI=(1-2*MOD(IABS(I),2))*ISIGN(1,I)
            DO 2290 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 2290
              IF(ISUB.EQ.352.AND.IABS(J).GT.10) GOTO 2290
              KCHWJ=(1-2*MOD(IABS(J),2))*ISIGN(1,J)
              KCHH=KCHWI+KCHWJ
              IF(IABS(KCHH).NE.2) GOTO 2290
              FACLR=VINT(180+I)*VINT(180+J)
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHH/2)/2)+WDTE(0,4))
              IF(I.EQ.J.AND.IABS(I).GT.10) THEN
                FACPRP=0.5D0*(FACPRT+FACPRU)**2
              ELSE
                FACPRP=FACPRT**2
              ENDIF
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACLR*FACWW*FACPRP*FACBW*HF
 2290       CONTINUE
 2300     CONTINUE
 
        ELSEIF(ISUB.EQ.353) THEN
C...f + fbar -> Z_R0
          SQMZR=PMAS(PYCOMP(KFPR(ISUB,1)),1)**2
          CALL PYWIDT(KFPR(ISUB,1),SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMZR)**2+HS**2)*3D0
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HP=(AEM/(3D0*(1D0-2D0*XW)))*XWC*SH
          DO 2310 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2310
            IF(IABS(I).LE.8) THEN
              EI=KCHG(IABS(I),1)/3D0
              AI=SIGN(1D0,EI+0.1D0)*(1D0-2D0*XW)
              VI=SIGN(1D0,EI+0.1D0)-4D0*EI*XW
            ELSE
              AI=-(1D0-2D0*XW)
              VI=-1D0+4D0*XW
            ENDIF
            HI=HP*(VI**2+AI**2)
            IF(IABS(I).LE.10) HI=HI*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=HI*FACBW*HF
 2310     CONTINUE
 
        ELSEIF(ISUB.EQ.354) THEN
C...f + fbar' -> W_R+/-
          SQMWR=PMAS(PYCOMP(KFPR(ISUB,1)),1)**2
          CALL PYWIDT(KFPR(ISUB,1),SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          FACBW=4D0*COMFAC/((SH-SQMWR)**2+HS**2)*3D0
          HP=AEM/(24D0*XW)*SH
          DO 2330 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 2330
            IA=IABS(I)
            DO 2320 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 2320
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 2320
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 2320
              KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP*2D0
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)*FACA/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              HF=SHR*(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))
              SIGH(NCHN)=HI*FACBW*HF
 2320       CONTINUE
 2330     CONTINUE
        ENDIF
 
      ELSEIF(ISUB.LE.380) THEN
 
        IF(ISUB.EQ.361) THEN
C...f + fbar -> W_L W_L, W_L pi_tc, pi_tc pi_tc
          FACA=(SH**2*BE34**2-(TH-UH)**2)
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/12D0)*AEM**2*CAB2*COMFAC*FACA*3D0
          FAR=SQRT(AEM/ALPRHT)
          FAO=FAR*QUPD
          FZR=FAR*CT2W
          FZO=-FAO*TANW
          SFAR=FAR**2
          SFAO=FAO**2
          SFZR=FZR**2
          SFZO=FZO**2
          CALL PYWIDT(23,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(23,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+113,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+113),1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+223,SH,WDTP,WDTE)
          SSMO=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+223),1)**2/SH,WDTP(0)/SHR)
          DETD=(FAR*FZO-FAO*FZR)**2+SSMZ*SSMR*SSMO-SFZR*SSMO-
     $    SFZO*SSMR-SFAR*SSMO*SSMZ-SFAO*SSMR*SSMZ
          DARHO=-(-FAR*SFZO+FAO*FZO*FZR+FAR*SSMO*SSMZ)/DETD/SH
          DZRHO=-(-FZR*SFAO+FAO*FZO*FAR+FZR*SSMO)/DETD/SH
          DAA=-(SFZO*SSMR+SFZR*SSMO-SSMO*SSMR*SSMZ)/DETD/SH
          DZZ=-(SFAO*SSMR+SFAR*SSMO-SSMO*SSMR)/DETD/SH
          DAZ=(FAR*FZR*SSMO+FAO*FZO*SSMR)/DETD/SH
 
          DO 2340 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2340
            IA=IABS(I)
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.25D0*(VI+AI)
            VARI=0.25D0*(VI-AI)
            F2L=EI*(DARHO/FAR+DAA+CT2W*DAZ)+
     $      VALI*(CT2W*DZRHO/FZR+CT2W*DZZ+DAZ)/SQRT(XW*XW1)
            F2R=EI*(DARHO/FAR+DAA+CT2W*DAZ)+
     $      VARI*(CT2W*DZRHO/FZR+CT2W*DZZ+DAZ)/SQRT(XW*XW1)
            HI=ABS(F2L)**2+ABS(F2R)**2
            IF(IA.LE.10) HI=HI/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            IF(KFA.EQ.KFB) THEN
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),1)
            ELSE
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),2)*WIDS(PYCOMP(KFB),3)
               NCHN=NCHN+1
               ISIG(NCHN,1)=I
               ISIG(NCHN,2)=-I
               ISIG(NCHN,3)=2
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),3)*WIDS(PYCOMP(KFB),2)
            ENDIF
 2340     CONTINUE
 
        ELSEIF(ISUB.EQ.364) THEN
C...f + fbar -> gamma pi_tc, gamma pi_tc', Z pi_tc, Z pi_tc',
C...W pi_tc
          VFAC=(TH**2+UH**2-2D0*SQM3*SQM4)
          AFAC=(TH**2+UH**2-2D0*SQM3*SQM4+4D0*SH*SQM3)
          FANOM=SQRT(PARU(1)*AEM)*PARP(144)/PARU(2)**2/PARP(142)
 
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/24D0)*AEM**2*COMFAC*3D0*SH
          FAR=SQRT(AEM/ALPRHT)
          FAO=FAR*QUPD
          FZR=FAR*CT2W
          FZO=-FAO*TANW
          SFAR=FAR**2
          SFAO=FAO**2
          SFZR=FZR**2
          SFZO=FZO**2
          CALL PYWIDT(23,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(23,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+113,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+113),1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+223,SH,WDTP,WDTE)
          SSMO=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+223),1)**2/SH,WDTP(0)/SHR)
          DETD=(FAR*FZO-FAO*FZR)**2+SSMZ*SSMR*SSMO-SFZR*SSMO-
     $    SFZO*SSMR-SFAR*SSMO*SSMZ-SFAO*SSMR*SSMZ
          DARHO=(-FAR*SFZO+FAO*FZO*FZR+FAR*SSMO*SSMZ)/DETD/SH
          DZRHO=(-FZR*SFAO+FAO*FZO*FAR+FZR*SSMO)/DETD/SH
          DAOME=(-FAO*SFZR+FAR*FZO*FZR+FAO*SSMR*SSMZ)/DETD/SH
          DZOME=(-FZO*SFAR+FAR*FAO*FZR+FZO*SSMR)/DETD/SH
          DAA=(SFZO*SSMR+SFZR*SSMO-SSMO*SSMR*SSMZ)/DETD/SH
          DZZ=(SFAO*SSMR+SFAR*SSMO-SSMO*SSMR)/DETD/SH
          DAZ=(FAR*FZR*SSMO+FAO*FZO*SSMR)/DETD/SH
 
          DO 2350 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2350
            IA=IABS(I)
            EI=KCHG(IABS(I),1)/3D0
            AI=SIGN(1D0,EI+0.1D0)
            VI=AI-4D0*EI*XWV
            VALI=0.25D0*(VI+AI)
            VARI=0.25D0*(VI-AI)
C...........Add in anomaly contribution
            F2L=(EI*DARHO+VALI*DZRHO/SQRT(XW*XW1))*VRGP
            F2L=F2L+(EI*DAOME+VALI*DZOME/SQRT(XW*XW1))*VOGP
            F2L=F2L+FANOM*(VAGP*(EI*DAA+VALI*DAZ/SQRT(XW*XW1))+
     $                    VZGP*(EI*DAZ+VALI*DZZ/SQRT(XW*XW1)))
            F2R=(EI*DARHO+VARI*DZRHO/SQRT(XW*XW1))*VRGP
            F2R=F2R+(EI*DAOME+VARI*DZOME/SQRT(XW*XW1))*VOGP
            F2R=F2R+FANOM*(VAGP*(EI*DAA+VARI*DAZ/SQRT(XW*XW1))+
     $                    VZGP*(EI*DAZ+VARI*DZZ/SQRT(XW*XW1)))
            HI=(ABS(F2L)**2+ABS(F2R)**2)*VFAC
            F2L=(EI*DARHO+VALI*DZRHO/SQRT(XW*XW1))*ARGP
            F2L=F2L+(EI*DAOME+VALI*DZOME/SQRT(XW*XW1))*AOGP
            F2R=(EI*DARHO+VARI*DZRHO/SQRT(XW*XW1))*ARGP
            F2R=F2R+(EI*DAOME+VARI*DZOME/SQRT(XW*XW1))*AOGP
            HJ=(ABS(F2L)**2+ABS(F2R)**2)*AFAC
            HI=HI+HJ
            IF(IA.LE.10) HI=HI/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            IF(ISUBSV.NE.368) THEN
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),2)*WIDS(PYCOMP(KFB),2)
            ELSE
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),2)*WIDS(PYCOMP(KFB),3)
               NCHN=NCHN+1
               ISIG(NCHN,1)=I
               ISIG(NCHN,2)=-I
               ISIG(NCHN,3)=2
               SIGH(NCHN)=HI*HP*WIDS(PYCOMP(KFA),3)*WIDS(PYCOMP(KFB),2)
            ENDIF
 2350     CONTINUE
 
        ELSEIF(ISUB.EQ.370) THEN
C...f + fbar' -> W_L Z_L, W_L pi_tc, Z_L pi_tc, pi_tc pi_tc
 
          FACA=(SH**2*BE34**2-(TH-UH)**2)
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/96D0)*AEM**2*CAB2*COMFAC*FACA*3D0/XW**2
          FWR=SQRT(AEM/ALPRHT)/(2D0*SQRT(XW))
          CALL PYWIDT(24,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(24,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+213,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+213),1)**2/SH,WDTP(0)/SHR)
          DETD=SSMZ*SSMR-DCMPLX(FWR**2,0D0)
          DWW=SSMR/DETD/SH
          DWRHO=-1D0/DETD/SH
          HP=HP*ABS(DWW+DWRHO)**2
          DO 2370 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 2370
            IA=IABS(I)
            DO 2360 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 2360
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 2360
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 2360
              KCHR=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*WIDS(PYCOMP(KFA),(5-KCHR)/2)*
     &        WIDS(PYCOMP(KFB),2)
 2360       CONTINUE
 2370     CONTINUE
 
        ELSEIF(ISUB.EQ.374) THEN
C...f + fbar' -> gamma pi_tc
          FANOM=SQRT(AEM)*PARP(144)/2D0/PARU(2)/PARP(142)
          VFAC=(TH**2+UH**2-2D0*SQM3*SQM4)
          AFAC=(TH**2+UH**2-2D0*SQM3*SQM4+4D0*SH*SQM3)/SQTA*ARGP**2
          ALPRHT=2.91D0*(3D0/PARP(144))
          HP=(1D0/48D0)*AEM**2/XW*COMFAC*3D0*SH
          FWR=SQRT(AEM/ALPRHT)/(2D0*SQRT(XW))
          CALL PYWIDT(24,SH,WDTP,WDTE)
          SSMZ=DCMPLX(1D0-PMAS(24,1)**2/SH,WDTP(0)/SHR)
          CALL PYWIDT(KTECHN+213,SH,WDTP,WDTE)
          SSMR=DCMPLX(1D0-PMAS(PYCOMP(KTECHN+213),1)**2/SH,WDTP(0)/SHR)
          DETD=SSMZ*SSMR-DCMPLX(FWR**2,0D0)
          DWW=SSMR/DETD/SH
          DWRHO=-DCMPLX(FWR,0D0)/DETD/SH
          HP=HP*(AFAC*ABS(DWRHO)**2+
     $    VFAC*ABS(FANOM*DWW*VWGP+DWRHO*VRGP/SQRT(SQTV))**2)
 
 
          DO 2390 I=MMIN1,MMAX1
            IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 2390
            IA=IABS(I)
            DO 2380 J=MMIN2,MMAX2
              IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 2380
              JA=IABS(J)
              IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 2380
              IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10))
     &        GOTO 2380
              KCHR=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
              HI=HP
              IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)/3D0
              NCHN=NCHN+1
              ISIG(NCHN,1)=I
              ISIG(NCHN,2)=J
              ISIG(NCHN,3)=1
              SIGH(NCHN)=HI*WIDS(PYCOMP(KFA),(5-KCHR)/2)*
     &        WIDS(PYCOMP(KFB),2)
 2380       CONTINUE
 2390     CONTINUE
 
        ENDIF
 
      ELSEIF(ISUB.LE.400) THEN
 
        IF(ISUB.EQ.391) THEN
C...f + fbar -> G*.
          KFGSTR=KFPR(ISUB,1)
          KCGSTR=PYCOMP(KFGSTR)
          CALL PYWIDT(KFGSTR,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACG=COMFAC*PARP(50)**2/(16D0*PARU(1))*SH*HF/
     &    ((SH-PMAS(KCGSTR,1)**2)**2+HS**2)
          DO 2400 I=MMINA,MMAXA
            IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2400
            HI=1D0
            IF(IABS(I).LE.10) HI=HI*FACA/3D0
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACG*HI
 2400     CONTINUE
 
        ELSEIF(ISUB.EQ.392) THEN
C...g + g -> G*.
          KFGSTR=KFPR(ISUB,1)
          KCGSTR=PYCOMP(KFGSTR)
          CALL PYWIDT(KFGSTR,SH,WDTP,WDTE)
          HS=SHR*WDTP(0)
          HF=SHR*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          FACG=COMFAC*PARP(50)**2/(32D0*PARU(1))*SH*HF/
     &    ((SH-PMAS(KCGSTR,1)**2)**2+HS**2)
          IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 2410
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACG
 2410     CONTINUE
 
        ELSEIF(ISUB.EQ.393) THEN
C...q + qbar -> g + G*.
          KFGSTR=KFPR(ISUB,2)
          KCGSTR=PYCOMP(KFGSTR)
          FACG=COMFAC*PARP(50)**2*AS*SH/(72D0*PARU(1)*SQM4)*
     &    (4D0*(TH2+UH2)/SH2+9D0*(TH+UH)/SH+(TH2/UH+UH2/TH)/SH+
     &    3D0*(4D0+TH/UH+UH/TH)+4D0*(SH/UH+SH/TH)+
     &    2D0*SH2/(TH*UH))
C...Propagators: as simulated in PYOFSH and as desired
          GMMG=PMAS(KCGSTR,1)*PMAS(KCGSTR,2)
          HBW4=GMMG/((SQM4-PMAS(KCGSTR,1)**2)**2+GMMG**2)
          CALL PYWIDT(KFGSTR,SQM4,WDTP,WDTE)
          HS=SQRT(SQM4)*WDTP(0)
          HF=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HBW4C=HF/((SQM4-PMAS(KCGSTR,1)**2)**2+HS**2)
          FACG=FACG*HBW4C/HBW4
          DO 2420 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &      KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 2420
            NCHN=NCHN+1
            ISIG(NCHN,1)=I
            ISIG(NCHN,2)=-I
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACG
 2420     CONTINUE
 
        ELSEIF(ISUB.EQ.394) THEN
C...q + g -> q + G*.
          KFGSTR=KFPR(ISUB,2)
          KCGSTR=PYCOMP(KFGSTR)
          FACG=-COMFAC*PARP(50)**2*AS*SH/(192D0*PARU(1)*SQM4)*
     &    (4D0*(SH2+UH2)/(TH*SH)+9D0*(SH+UH)/SH+SH/UH+UH2/SH2+
     &    3D0*TH*(4D0+SH/UH+UH/SH)/SH+4D0*TH2*(1D0/UH+1D0/SH)/SH+
     &    2D0*TH2*TH/(UH*SH2))
C...Propagators: as simulated in PYOFSH and as desired
          GMMG=PMAS(KCGSTR,1)*PMAS(KCGSTR,2)
          HBW4=GMMG/((SQM4-PMAS(KCGSTR,1)**2)**2+GMMG**2)
          CALL PYWIDT(KFGSTR,SQM4,WDTP,WDTE)
          HS=SQRT(SQM4)*WDTP(0)
          HF=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HBW4C=HF/((SQM4-PMAS(KCGSTR,1)**2)**2+HS**2)
          FACG=FACG*HBW4C/HBW4
          DO 2440 I=MMINA,MMAXA
            IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 2440
            DO 2430 ISDE=1,2
              IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 2430
              IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 2430
              NCHN=NCHN+1
              ISIG(NCHN,ISDE)=I
              ISIG(NCHN,3-ISDE)=21
              ISIG(NCHN,3)=1
              SIGH(NCHN)=FACG
 2430       CONTINUE
 2440     CONTINUE
 
        ELSEIF(ISUB.EQ.395) THEN
C...g + g -> g + G*.
          KFGSTR=KFPR(ISUB,2)
          KCGSTR=PYCOMP(KFGSTR)
          FACG=COMFAC*3D0*PARP(50)**2*AS*SH/(32D0*PARU(1)*SQM4)*
     &    ((TH2+TH*UH+UH2)**2/(SH2*TH*UH)+2D0*(TH2/UH+UH2/TH)/SH+
     &    3D0*(TH/UH+UH/TH)+2D0*(SH/UH+SH/TH)+SH2/(TH*UH))
C...Propagators: as simulated in PYOFSH and as desired
          GMMG=PMAS(KCGSTR,1)*PMAS(KCGSTR,2)
          HBW4=GMMG/((SQM4-PMAS(KCGSTR,1)**2)**2+GMMG**2)
          CALL PYWIDT(KFGSTR,SQM4,WDTP,WDTE)
          HS=SQRT(SQM4)*WDTP(0)
          HF=SQRT(SQM4)*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
          HBW4C=HF/((SQM4-PMAS(KCGSTR,1)**2)**2+HS**2)
          FACG=FACG*HBW4C/HBW4
          IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
            NCHN=NCHN+1
            ISIG(NCHN,1)=21
            ISIG(NCHN,2)=21
            ISIG(NCHN,3)=1
            SIGH(NCHN)=FACG
          ENDIF
 
        ENDIF
      ENDIF
 
C...Multiply with parton distributions
      IF(ISUB.LE.90.OR.ISUB.GE.96) THEN
        DO 2450 ICHN=1,NCHN
          IF(MINT(45).GE.2) THEN
            KFL1=ISIG(ICHN,1)
            SIGH(ICHN)=SIGH(ICHN)*XSFX(1,KFL1)
          ENDIF
          IF(MINT(46).GE.2) THEN
            KFL2=ISIG(ICHN,2)
            SIGH(ICHN)=SIGH(ICHN)*XSFX(2,KFL2)
          ENDIF
          SIGS=SIGS+SIGH(ICHN)
 2450   CONTINUE
      ENDIF
 
      RETURN
      END
