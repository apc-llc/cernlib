CDECK  ID>, X_OPT.
      SUBROUTINE X_OPT

C-    Process   +OPTION, +PARAM, +SHOW

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*30, COLS(30)*1
      EQUIVALENCE (LINE,SLLINE)
      EQUIVALENCE (COLS,LINE)

      PARAMETER   (NPOSS = 10,    NPARA = 3,     NSHOW=2)
      CHARACTER    POSS(NPOSS)*8, PARA(NPARA)*8, SHOW(NSHOW)*8

      PARAMETER   (NPOSSX = 3)
      CHARACTER    POSSX(NPOSSX)*8

      DATA POSSX  / 'OFF     '
     +,             'COM*PACT'
     +,             'EJE*CT  ' /

      DATA POSS   / 'OFF     '
     +,             'ALL     '
     +,             'BAC*KCOM'
     +,             'COM*PACT'
     +,             'EJE*CT  '
     +,             'FUL*L   '
     +,             'MAP*ASM '
     +,             'URE*F   '
     +,             'VER*BOSE'
     +,             'XCR*OSS ' /

      DATA PARA   / 'LIN*ES  '
     +,             'COL*UMNS'
     +,             'CLA*SH  ' /

      DATA SHOW   / 'ASM     '
     +,             'MEM*ORY ' /

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)

C----         Do +OPTION

      IF (JCCTYP.NE.MCCOPT)        GO TO 41
      IF (NCCPT.EQ.0)              GO TO 31
      IF (IFLAUX.EQ.0)  THEN
          CALL CCOPT (POSS,NPOSS)
          IF (JCCBAD.NE.0)         GO TO 91
        ELSE
          CALL CCOPT (POSSX,NPOSSX)
          IF (JCCBAD.NE.0)         RETURN
        ENDIF
      MASKOP = MCCPAR(JCCPT+1)

      IF (IAND(MASKOP,KMO).EQ.0)  THEN
          MOPTIO(33) = IOR (MOPTIO(33),MASKOP)
        ELSE
          MOPTIO(33) = IAND (MOPTIO(33),NOT(MASKOP))
        ENDIF

      CALL UPKBYT (MOPTIO(33),1,MOPTIO(1),32,0)

C--           print current state of options

   31 DO 34  J=1,LEN(LINE)
      IF (MOPTIO(J).EQ.0)  THEN
          COLS(J) = '.'
        ELSE
          COLS(J) = CQCETA(J:J)
        ENDIF
   34 CONTINUE

      NQLLBL = 0
      NQUSED = NQUSED + 1
      WRITE (IQPRNT,9034) LINE
 9034 FORMAT (10X,'Status of options:  ',A)
      RETURN

C----         Do +PARAM

   41 IF (JCCTYP.NE.MCCOP2)        GO TO 61
      IF (NCCPT.EQ.0)              RETURN
      CALL CCOPT (PARA,NPARA)
      IF (JCCBAD.NE.0)             GO TO 91
      MASKOP = MCCPAR(JCCPT+2)
      NVAL   = MCCPAR(JCCPN+1)

C--       +PARAM, LINES, N=n  - n lines per page

      IF (IAND(MASKOP,KM1).EQ.0)   GO TO 42
      NQLMAX = MAX (NVAL,24)
      CALL MQPAGE
      RETURN

C--       +PARAM, COL, N=n  - n columns per page

   42 IF (IAND(MASKOP,KM2).EQ.0)   GO TO 44
      NVAL   = MIN (NVAL,140)
      IF (IFLAUX.NE.0)             GO TO 43
      NQCMAX = MAX (NVAL,90)
      NQCPGH = NQCMAX
      IF (IAND(MU_GLOB,KM2).NE.0)  RETURN
      IF (NQCMAX.LT.102)           RETURN
      NQCPGH = 100 + (NQCMAX-100)/2
      RETURN

   43 NQCMAX = MAX (NVAL,100)
      NQCPGH = NQCMAX
      IF (NQCMAX.LT.102)           RETURN
      NQCPGH = 100 + (NQCMAX-100)/2
      RETURN

C--       +PARAM, CLASH, N=n  - print clash at or above n

   44 NCLASH = MAX (1,NVAL)
      RETURN

C----         Do +SHOW

   61 IF (NCCPT.EQ.0)              RETURN
      CALL CCOPT (SHOW,NSHOW)
      IF (JCCBAD.NE.0)             GO TO 91
      MASKOP = MCCPAR(JCCPT+2)

C--       +SHOW, ASM   - display the ASM data structure

      IF (IAND(MASKOP,KM1).NE.0)  THEN
          CALL ASMDUMP ('as requested')
          RETURN
        ENDIF

C--       +SHOW, MEMORY - show memory occupation

      CALL MQSHOW
      RETURN

C----         Trouble

   91 CALL FAILCC (0, 'Unknown option')
      RETURN
      END
