CDECK  ID>, MK_NIL.
      SUBROUTINE MK_NIL

C-    Store NIL sequences or actions into division 1 to carry the
C-    EXE bits of deleted material to the deck where it would have
C-    appeared.  For this we have to scan the de-selected PREP banks.
C.    started  9-dec-91

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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
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
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      LDO = KQPREP + 1

C------            Find next deleted foreign material

   21 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                RETURN
      IF (IQ(LDO+2).EQ.0)          GO TO 21
      IF (IAND(IQ(LDO),KM5+KM6).EQ.0)  GO TO 21

      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LT.MCCKEE)        GO TO 21
      IF (JCCTYP.GE.MCCUSE)        GO TO 21

      CALL CCKRAK (IQ(LDO+1))
      IF (JCCBAD.NE.0)             GO TO 21
      MEXACT = IAND (IQ(LDO),15)

      IF (JCCTYP.NE.MCCKEE)        GO TO 41

C----------        Handle +KEEP                   --------------------

      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)   GO TO 21
      IF (IAND(MEXACT,NOT(MU_GLOB)).EQ.0)   GO TO 21

C--       inquire existence, get the attachment adr if not pre-existing

   34 CALL LOCKEEP (LSQ)
      IF (LSQ.NE.0)                GO TO 21
      KACT = IQUEST(1)
      LPD  = IQUEST(11)

      IF (LPD.NE.0)  THEN
          IF (IAND(MEXACT,NOT(IQ(LPD))).EQ.0)  GO TO 21
        ENDIF

C--         lift the KEEP bank

      IF (LDECO.EQ.0)  CALL CRDECO
      CALL MQLIFT (LACT,KACT,0,JBKKEE,1)

      IQ(LACT)   = IOR (IQ(LACT), MEXACT+KM12)
      LQ(LACT-3) = LDECO
      IQ(LACT+3) = IQ(LDO+1) - JSLORG
      IQ(LACT+4) = MCCPAR(JCCPZ+1)
      GO TO 21

C----------        Handle +DEL +REPL +ADB +ADD    --------------------

   41 LPD = LQ(LDO-3)
      IF (LPD.EQ.0)                GO TO 21
      IF (LPD.EQ.LEXD)             GO TO 21

      IF (IAND(MEXACT,NOT(IQ(LPD))).EQ.0)  GO TO 21
      JACT = JCCTYP - MCCDEL

C--         lift the ACT bank

      IF (LDECO.EQ.0)  CALL CRDECO
   47 CALL MQLIFT (LACT,LPD,-2,JBKACT,1)

      IQ(LACT) = IOR (IQ(LACT), MEXACT+KM12)
      IQ(LACT) = IOR (IQ(LACT), ISHFTL(JACT,8))

      LQ(LACT-3) = LDECO
      IQ(LACT+3) = IQ(LDO+1) - JSLORG
      IQ(LACT+4) = MCCPAR(JCCPC+1)
      IQ(LACT+5) = MCCPAR(JCCPC+2)
      IF (JCCTYP.EQ.MCCDEL)        GO TO 48

C--       check whether the action starts with ordinary material

      JSLF = IQ(LDO+1) + 1
      JTXF = MLIAD(JSLF)
      IF (TEXT(JTXF).NE.'-')       GO TO 21
      IF (JPTYPE(TEXT(JTXF)).LT.MCCSEL)  GO TO 21
      IQ(LACT) = IOR (IQ(LACT), KM16)
      GO TO 21

C--       handle multiple deletes

   48 IQ(LACT) = IOR (IQ(LACT), KM16)
      NCCPC = NCCPC - 1
      IF (NCCPC.EQ.0)              GO TO 21
      JCCPC = JCCPC + 3
      GO TO 47
      END
