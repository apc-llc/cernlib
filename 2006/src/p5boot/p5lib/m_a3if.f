CDECK  ID>, M_A3IF.
      SUBROUTINE M_A3IF (IFSTEP)

C-    Content analysis stage 3:
C-    scan the PREP/MAT structure from the current +_IF to its
C-    terminating +_ENDIF and decide accept/reject.

C-    On entry LDOAN points to this +_IF, it has already been cracked;
C-    return LDOAN pointing to the end, and IFSTEP = 1 if LDOAN
C-    does point to the terminating +_ENDIF  (preset =1 by M_ANA3),
C-    but for some error LDOAN points to some c/line which must still
C-    be analysed in M_ANA3, in this case set IFSTEP = 0;
C-    also: JCCBAD non-zero signals c/line with syntax error
C.    started 6-july-93

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      IF (JCCTYP.GE.MCCELS)        GO TO 93
      CALL CCIFLV
      IQ(LDOAN) = IOR (IQ(LDOAN), KM17)
      MODEAN = 1
      MAXLEV = 36
      LEVIF  = 1
      INCLEV = LEVIF - JCCLEV
      IF (INCLEV.EQ.LEVAN)         GO TO 25
      IF (LEVAN.EQ.7)  THEN
          LEVAN = INCLEV
          GO TO 25
        ELSE
          CALL M_WNIF (LEVAN-INCLEV)
        ENDIF
      GO TO 25

C------            accepted material

   22 CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 94
      IF (JCCTYP.GE.MCCSEL)        GO TO 94
      IF (JCCTYP.GE.MCCTRU)        GO TO 24
      IF (JCCTYP.LT.MCCCDE)        GO TO 22

C--       handle accepted +CDE +SEQ

      IF (JCCTYP.NE.MCCXSQ)  THEN
          CALL M_KRAK (1)
          IF (JCCBAD.NE.0)         RETURN
          MEXAN = IOR (MEXAN,MXCCIF)
          IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  JCCIFV= 1
        ENDIF

      CALL M_A3SQ
      IF (JCCBAD.EQ.0)             GO TO 22
      RETURN

C--           new control

   24 CALL M_KRAK (0)
      IF (JCCBAD.NE.0)             RETURN
      CALL CCIFLV

      IF (JCCTYP.EQ.MCCELS)        GO TO 26
      IF (JCCTYP.EQ.MCCEND)        GO TO 28

C--           handle +__IF

      CALL CCPROC
      LEVIF = LEVIF + 1
      N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      IF (LEVIF.GE.MAXLEV)         GO TO 92
   25 MEXAN = IOR (MEXAN,MXCCIF)
      IF (JCCTYP.EQ.MCCFAL)  JCCIFV= MAX(0, 1-JCCIFV)
      IF (JCCIFV.EQ.0)             GO TO 22
      LVREND = LEVIF
      LVRELS = LEVIF + 1
      GO TO 41

C--       handle  +_ELSE, IF=xxx

   26 N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      LVREND = LEVIF
      LVRELS = LEVIF
      GO TO 41

C--       handle  +_ENDIF

   28 N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      LEVIF = LEVIF - 1
      IF (LEVIF.EQ.0)              RETURN
      GO TO 22

C------            rejected material

   41 MBUSE  = KM5
   42 IQ(LDOAN) = IOR (IQ(LDOAN), MBUSE)
      MBUSE  = KM6
      CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 94
      IF (JCCTYP.GE.MCCSEL)        GO TO 94
      IF (JCCTYP.LT.MCCTRU)        GO TO 42

C--           new control

      CALL M_KRAK (0)
      IF (JCCBAD.NE.0)             RETURN
      CALL CCIFLV

      IF (JCCTYP.EQ.MCCELS)        GO TO 46
      IF (JCCTYP.EQ.MCCEND)        GO TO 48

C--           handle +__IF

      LEVIF = LEVIF + 1
      N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      IF (LEVIF.GE.MAXLEV)         GO TO 92
      GO TO 42

C--       handle  +_ELSE, IF=xxx

   46 N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      IF (LEVIF.GE.LVRELS)         GO TO 42
      CALL CCPROC
      MEXAN = IOR (MEXAN,MXCCIF)
      IF (JCCIFV.NE.0)             GO TO 41
      GO TO 22

C--       handle  +_ENDIF

   48 N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      LEVIF = LEVIF - 1
      IF (LEVIF.GE.LVREND)         GO TO 42
      IF (LEVIF.EQ.0)              RETURN
      GO TO 22

C-----        Trouble

   94 CALL FAILCC (1, 'Non-terminated +_IF')
      IFSTEP = 0
      RETURN

   93 IF (JCCTYP.EQ.MCCEND)  THEN
          CALL FAILCC (0, 'Redundant +_ENDIF')
          RETURN
        ENDIF

      CALL M_FAIL ('Unmatched +_ELSE')
      RETURN

   92 CALL M_FAIL ('Maximum +_IF nesting level exceeded')
      RETURN
      END
