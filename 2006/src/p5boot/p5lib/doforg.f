CDECK  ID>, DOFORG.
      SUBROUTINE DOFORG (IFTONX)

C-    Store new foreign material into division 1
C-    IF-selection has been done by M_ANA3
C.    started  1-dec-91

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      IFTON = 1
      IF (IAND(IQ(LDOAN),KM5).NE.0)  THEN
          IFLNIL = IAND(IQ(LDOAN),KM8)
          IF (IFLNIL.EQ.0)           GO TO 99
        ELSE
          IFLNIL = 0
        ENDIF

      IF (LDECO.EQ.0)  CALL CRDECO
      CALL CCKRAK (IQ(LDOAN+1))

      MEXAN = MX_FORG
      IF (JCCTYP.NE.MCCKEE)          GO TO 41

C----------        Handle +KEEP                   --------------------

      IFLSGL = KM5+KM6+KM17
      IFLMSK = KM5+KM6
      NSTEP  = 0
      LAST   = 0
      IXS    = MCCPAR(JCCPZ+1)
      LACT   = LQ(LDOAN-3)
      IF (LACT.GT.0)               GO TO 24

C--       append to existing sequence

      LACT   = -LACT
      IFLSGL = 0
      IFTON  = 0
      KJOIN  = KQLAST (LACT-2)
      JCCTYP = 0
      IF (IQ(LDOAN+1).EQ.1)        GO TO 31
      NSTEP = 1
      GO TO 32

C--         ready the KEEP bank

   24 IF (NCCPP.NE.0)  IFLSGL= 0

      IQ(LACT)   = IOR (IQ(LACT), KM7)
      IQ(LACT+1) = IQ(LDOAN+1) + 1
      IQ(LACT+2) = IQ(LDOAN+2) - 1
C!    IQ(LACT+4) = IXS
      IQ(LDOAN+2) = 1

      IF (LUPAN.EQ.0)  THEN
          LQ(LACT-3) = LDECO
          IQ(LACT+3) = IQ(LACT+1) - JSLORG
          LAST = LACT
        ELSE
          LQ(LACT-3) = LQ(LDOAN-2)
          IQ(LACT+3) = IQ(LDOAN+3) + 1
        ENDIF

C--       NIL sequence

      IF (IFLNIL.NE.0)             GO TO 99

C------       create MAT continuation banks for KEEP

      IFTON = 0
      KJOIN = LACT - 2
   31 CALL MK_FNX
      IF (LDOAN.EQ.0)              GO TO 39
      IF (JCCTYP.GE.MCCSEL)        GO TO 39
      NSTEP = 0

   32 IF (LUPAN.NE.0)              GO TO 38
      JSLNEW = IQ(LDOAN+1) + NSTEP
      NSLNEW = IQ(LDOAN+2) - NSTEP
      NUMNEW = JSLNEW - JSLORG
      IF (NSLNEW.EQ.0)             GO TO 31

      IF (JCCTYP.LT.MCCCDE)        GO TO 36
      IF (JCCTYP.GE.MCCXSQ)        GO TO 35

C--         handle +CDE or +SEQ

      CALL CCKRAK (JSLNEW)

C--       check +SEQ,..., T=DUMMY

      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  GO TO 31

C----       try immediate substitution for +SEQ, single name

      IF (NCCPZ.NE.1)              GO TO 34
      IF (NCCPIF.NE.0)             GO TO 34

      IXC = MCCPAR(JCCPZ+1)
      CALL LOCSEQ (LSQF, IXC,0,0)
      IF (LSQF.EQ.0)               GO TO 34
      IF (IAND(IQ(LSQF),KM17).EQ.0)   GO TO 34

C--       do the substitution

      JSLNEW = IQ(LSQF+1)
      IF (JSLNEW.LT.LQLSTA(4))  THEN
          JSLNEW = LN_COP4 (JSLNEW,1)
          IQ(LSQF+1) = JSLNEW
        ENDIF

      JCCTYP = 0

C--       does the KEEP bank have zero lines?
      IF (LAST.NE.LACT)            GO TO 36
      IF (IQ(LACT+3).NE.NUMNEW)    GO TO 36
      IQ(LACT+1) = JSLNEW
      IQ(LACT+2) = 1
      GO TO 31

C----       lift XSQ bank for +CDE or +SEQ

   34 LMAT = LIFTSQ (KJOIN,JCCTYP,0,1)
      IFLMSK = 0
      GO TO 37

C----       lift new MAT bank

   35 IFLMSK = 0
   36 CALL MQLIFT (LMAT,KJOIN,0,JBKMAT,1)

      IQ(LMAT)   = IOR (IQ(LMAT), ISHFTL(JCCTYP,8))
   37 LQ(LMAT-2) = LDECO
      IQ(LMAT+1) = JSLNEW
      IQ(LMAT+2) = NSLNEW
      IQ(LMAT+3) = NUMNEW

      LAST   = LMAT
      KJOIN  = LMAT - 1
      GO TO 31

C----              shunt higher level MAT banks

   38 LEV = JBYT (IQ(LDOAN),21,5)
      CALL SBYT (LEV+1,IQ(LDOAN),21,5)

      CALL QSHUNT (KDOAN,KJOIN)
      LAST   = 0
      KJOIN  = LDOAN - 1
      LDOAN  = KDOAN + 1
      IFLMSK = 0
      GO TO 31

C----              End of material for KEEP, finalize

   39 IF (LQ(LACT-2).NE.0)  IFLSGL= 0
      IF (IQ(LACT+2).NE.1)  IFLSGL= 0

      IFLMSK   = IOR (IFLMSK, MEXAN)
      IFLMSK   = IOR (IFLMSK, IFLSGL)
      IQ(LACT) = IOR (IQ(LACT), IFLMSK)
      GO TO 99

C----------        Handle +DEL  +REPL  +ADB  +ADD   -----------------

   41 LCRD  = LQ(LDOAN-3)
      JSLCL = IQ(LDOAN+1)
      JACTU = ISHFTL (JCCTYP-MCCDEL,8)

      IF (LUPAN.EQ.0)  THEN
          LORG   = LDECO
          NUMORG = JSLCL - JSLORG
        ELSE
          LORG   = LQ(LDOAN-2)
          NUMORG = IQ(LDOAN+3)
        ENDIF

C--         lift the ACT bank

   45 CALL MQLIFT (LACT,LCRD,-2,JBKACT,1)

      IQ(LACT)   = IOR (IQ(LACT), JACTU)
      IQ(LACT)   = IOR (IQ(LACT), MEXAN)
      LQ(LACT-3) = LORG
      IQ(LACT+1) = JSLCL
      IQ(LACT+2) = 1
      IQ(LACT+3) = NUMORG
      IQ(LACT+4) = MCCPAR(JCCPC+1)
      IQ(LACT+5) = MCCPAR(JCCPC+2)

C--         NIL action

      IF (IFLNIL.NE.0)  THEN
          IQ(LACT) = IOR (IQ(LACT),KM12)
          IF (LUPAN.EQ.0)  IQ(LACT+2) = 0
          IF (JCCTYP.NE.MCCDEL)    GO TO 99
          JCCTYP = -1
          GO TO 79
        ENDIF

C--         true action

      IF (JCCTYP.EQ.MCCDEL)  THEN
          IF (NCCPC.GE.2)  JSLCL= LN_COP4 (JSLCL,1)
          JCCTYP = -1
        ENDIF
      IF (JCCTYP.EQ.-1)            GO TO 79

      IFTON = 0
      KJOIN = LACT - 2

C--         handle +ADD, ..., Z=sname

      IF (NCCPZ.EQ.0)              GO TO 47
      LMAT = LIFTSQ (KJOIN,MCCXSQ,0,1)
      LQ(LMAT-2) = LORG
      IQ(LMAT+1) = JSLCL
      IQ(LMAT+2) = 0
      IQ(LMAT+3) = NUMORG
      KJOIN = LMAT - 1

   47 IF (LUPAN.NE.0)              GO TO 51
      JCCTYP = 0
      NSTEP  = 1
      GO TO 53

C----       create MAT continuation banks for ACT

   51 CALL MK_FNX
      IF (LDOAN.EQ.0)              GO TO 79
      IF (JCCTYP.GE.MCCSEL)        GO TO 79
      IF (IQ(LDOAN+2).EQ.0)        GO TO 51
      IF (LUPAN.NE.0)              GO TO 77
      NSTEP = 0
   53 CALL MK_FRAG (KJOIN,NSTEP)
      GO TO 51

C----              shunt higher level MAT banks

   77 LEV = JBYT (IQ(LDOAN),21,5)
      CALL SBYT (LEV+1,IQ(LDOAN),21,5)

      CALL QSHUNT (KDOAN,KJOIN)
      KJOIN  = LDOAN - 1
      LDOAN  = KDOAN + 1
      GO TO 51

C----              End of material for ACT, finalize

   79 IQ(LACT) = IOR (IQ(LACT), MEXAN)
      IF (JCCTYP.NE.-1)            GO TO 99

C--           iterate if multiple +DEL

      JCCPC = JCCPC + 3
      NCCPC = NCCPC - 1
      IF (NCCPC.NE.0)              GO TO 45

   99 IFTONX = IFTON
      RETURN
      END
