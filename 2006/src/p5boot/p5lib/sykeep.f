CDECK  ID>, SYKEEP.
      SUBROUTINE SYKEEP

C-    Remember sequence definition
C.    started 18-july-95

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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      PARAMETER   (NOFFLI=2, NOFFDA=4)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      IF (JCCBAD.NE.0)             GO TO 91
      MOPT = MCCPAR(JCCPT+1)
      IXS  = MCCPAR(JCCPZ+1)
      IF (LDECO.EQ.0)  CALL CRDECO
      IF (INCRAD.EQ.0)             GO TO 41

C--------     +KEEP in the cradle     --------------------------

C--       check sequence existing

      CALL SYLKEEP (LACT)
      IF (LACT.NE.0)               GO TO 39
      KACT = IQUEST(1)

      CALL MQLIFT (LACT,KACT,0,JBKKEE,1)

      IQ(LACT-3) = LDECO
      IQ(LACT)   = IOR (IQ(LACT), KM18)
      IQ(LACT+4) = IXS

      IF (IAND(MOPT,KMS).EQ.0)  THEN
          IQ(LACT) = IOR (IQ(LACT), KM12)
        ELSE
          IQ(LACT) = IOR (IQ(LACT), KM14)
        ENDIF

C--       no associated material for a +KEEP in the cradle

   39 NSL = IQ(LDOAN+2)
      IF (NSL.GE.2)  L = M_SPLIT (LDOAN, IQ(LDOAN+1)+1)
      RETURN

C--------     +KEEP on the PAM file   --------------------------

   41 IF (IAND(MOPT,KMD).NE.0)     RETURN

C--       check sequence existing

      CALL SYLKEEP (LACT)
      KACT = IQUEST(1)
      IF (LACT.EQ.0)               GO TO 47

C--       check sequence is special or blocked from the cradle

      MACT = IQ(LACT)
      IF (IAND(MACT,KM12+KM13).NE.0)  RETURN
      IF (IAND(MACT,KM14).EQ.0)       GO TO 44

C--       sequence had been registered with T=SINGLE

      IF (IAND(MACT,KM15).NE.0)    RETURN
      CALL TOGARB (KACT,0)
      IFLSING = 7
      GO TO 48

C--       a new definition of an existing sequence

   44 KACT = LACT - 4
      LACT = LQ(KACT)
      IF (LACT.NE.0)         GO TO 44

C--       create the heading KEEP bank

   47 IFLSING = 0
   48 CALL MQLIFT (LACT,KACT,0,JBKKEE,1)
      CALL SYSPLIT (LDOAN)

      IQ(LACT-3) = LDECO
      IQ(LACT)   = IOR (IQ(LACT), KM7)
      IQ(LACT+1) = IQ(LDOAN+1) + 1
      IQ(LACT+2) = IQ(LDOAN+2) - 1
      IQ(LACT+3) = IQ(LACT+1) - JSLORG
      IQ(LACT+4) = IXS
      IF (IAND(MOPT,KMN).NE.0)  IQ(LACT) = IOR (IQ(LACT), KM16)
      IF (IFLSING.NE.0)         IQ(LACT) = IOR (IQ(LACT), KM14+KM15)

C------       create MAT continuation banks for KEEP

      IFLSGL = KM5+KM6+KM17
      IFLMSK = KM5+KM6
      LAST   = LACT
      IF (NCCPP.NE.0)  IFLSGL= 0

      KJOIN = LACT - 2
      LDOTK = LDOAN
   51 LDOTK = LQ(LDOTK-1)
      IF (LDOTK.EQ.0)              GO TO 69
      IF (LQ(LDOTK-2).NE.0)        GO TO 51

      JCCTYP = JBYT (IQ(LDOTK),9,6)
      IF (JCCTYP.GE.MCCSEL)        GO TO 69

      JSLNEW = IQ(LDOTK+1)
      NSLNEW = IQ(LDOTK+2)
      NUMNEW = JSLNEW - JSLORG
      IF (NSLNEW.EQ.0)             GO TO 51

      IF (JCCTYP.LT.MCCCDE)        GO TO 62
      IF (JCCTYP.GE.MCCXSQ)        GO TO 61

C--         handle +CDE or +SEQ

      CALL CCKRAK (JSLNEW)

C--       check +SEQ,..., T=DUMMY

      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  GO TO 51

C----       try immediate substitution for +SEQ, single name

      IF (NCCPZ.NE.1)              GO TO 54
      IF (NCCPIF.NE.0)             GO TO 54

      IXC = MCCPAR(JCCPZ+1)
      CALL SYLSEQ (LSQF, IXC, 0,0)
      IF (LSQF.EQ.0)               GO TO 54
      IF (IAND(IQ(LSQF),KM17).EQ.0)   GO TO 54

C--       do the substitution

      JSLNEW = IQ(LSQF+1)
      IF (JSLNEW.LT.LQLSTA(4))  THEN
          JSLNEW = LN_COP4 (JSLNEW,1)
          IQ(LSQF+1) = JSLNEW
        ENDIF

      JCCTYP = 0

C--       does the KEEP bank have zero lines?
      IF (LAST.NE.LACT)            GO TO 62
      IF (IQ(LACT+3).NE.NUMNEW)    GO TO 62
      IQ(LACT+1) = JSLNEW
      IQ(LACT+2) = 1
      GO TO 51


C----       lift XSQ bank for +CDE or +SEQ

   54 NCCPZ = MIN (NCCPZ,61)

      MMBANK(3,JBKXSQ) = NCCPZ + NOFFLI
      MMBANK(5,JBKXSQ) = NCCPZ + NOFFDA
      CALL MQLIFT (LMAT,KJOIN,0,JBKXSQ,1)

      IQ(LMAT) = IOR (IQ(LMAT), ISHFTL(JCCTYP,8))
      IQ(LMAT) = IOR (IQ(LMAT), IAND(MCCPAR(JCCPT+1),KMP))

      IQ(LMAT+NOFFDA) = NCCPZ
      CALL UCOCOP (MCCPAR(JCCPZ+1),IQ(LMAT+NOFFDA+1),NCCPZ,1,3,1)
      IFLMSK = 0
      GO TO 63

C----       lift new MAT bank

   61 IFLMSK = 0
   62 CALL MQLIFT (LMAT,KJOIN,0,JBKMAT,1)

      IQ(LMAT)   = IOR (IQ(LMAT), ISHFTL(JCCTYP,8))
   63 LQ(LMAT-2) = LDECO
      IQ(LMAT+1) = JSLNEW
      IQ(LMAT+2) = NSLNEW
      IQ(LMAT+3) = NUMNEW

      LAST   = LMAT
      KJOIN  = LMAT - 1
      GO TO 51

C----              End of material for KEEP, finalize

   69 IF (LQ(LACT-2).NE.0)  IFLSGL= 0
      IF (IQ(LACT+2).NE.1)  IFLSGL= 0

      IFLMSK   = IOR (IFLMSK, IFLSGL)
      IQ(LACT) = IOR (IQ(LACT), IFLMSK)

      JCCTYP = MCCKEE
      RETURN

C----         Faulty control line

   91 CONTINUE
      RETURN
      END
