CDECK  ID>, M_A3NX.
      SUBROUTINE M_A3NX

C-    Service routine to M_ANA3, content analysis stage 3:
C-    step to next non-deleted bank, set JCCTYP
C-    activating delayed control lines now used in self material.
C.    started 26-nov-91

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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

      PARAMETER    (MASKTY=KM9+KM10+KM11+KM12+KM13+KM14)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)



C------            Next PREP bank

      IF (LUPAN.NE.0)              GO TO 34
   22 KDOAN = LDOAN - 1
      LDOAN = LQ(KDOAN)
      IF (LDOAN.EQ.0)              RETURN
      IF (IAND(IQ(LDOAN),KM5+KM6).NE.0)  GO TO 22

C--       next live PREP bank

      L = LQ(LDOAN-2)
      IF (L.NE.0)                  GO TO 31
      JCCTYP = JBYT (IQ(LDOAN),9,6)
      RETURN

C----              Follow reference to foreign material

   31 LOWAN = LDOAN
      LUPAN = L
      LDOAN = LUPAN - 1

C------            Next MAT bank

   34 KDOAN = LDOAN - 1
      LDOAN = LQ(KDOAN)
      IF (LDOAN.EQ.0)  THEN
C--       end of chain of MAT banks
          LDOAN = LOWAN
          LUPAN = 0
          GO TO 22
        ENDIF

      JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (MOPUPD.LT.0)             GO TO 61
C!    IF (MODEAN.EQ.4)             RETURN

C--       activate delayed c/l now used in self or KEEP material

      IF (IAND(IQ(LDOAN),KM15).EQ.0)    RETURN
      JSLA = IQ(LDOAN+1)
      JTX  = MLIAD(JSLA)
      TEXT(JTX) = '+'
      JTYPE     = JPTYPE(TEXT(JTX))
      IF (MOPUPD.NE.0)  THEN
          IF (JTYPE.LT.MCCSEL)     RETURN
        ENDIF
      JCCTYP    = JTYPE
      IQ(LDOAN) = IOR (IQ(LDOAN),ISHFTL(JCCTYP,8))
      RETURN

C--       +PAM, T=UP  update mode

   61 IF (JCCTYP.EQ.0)             GO TO 62
      IF (JCCTYP.EQ.MCCXSQ)        RETURN
      JCCTYP = 0
      IQ(LDOAN) = IAND (IQ(LDOAN),NOT(MASKTY))

   62 JSLA = IQ(LDOAN+1)
      JSLE = IQ(LDOAN+2) + JSLA
   64 CALL NEXTCC ('-',JSLA,JSLE,JSLF,JTYPE)
      IF (JTYPE.EQ.0)              RETURN
      JTX  = MLIAD(JSLF)
      TEXT(JTX) = '+'
      JSLA = JSLF + 1
      IF (JSLA.LT.JSLE)            GO TO 64
      RETURN
      END
