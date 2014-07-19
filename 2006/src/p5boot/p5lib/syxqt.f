CDECK  ID>, SYXQT.
      SUBROUTINE SYXQT

C-    List the material of the current deck according to the map
C.    started 18-july-95

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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


      IXPL  = -1
      LEVIF = 0
      LUPAN = 0
      LDOAN = KQPREP + 1

C----              Next bank

   20 KDOAN = LDOAN - 1
      LDOAN = LQ(KDOAN)
   22 MODEAN = 0
      IF (LDOAN.EQ.0)              GO TO 87
      IF (LQ(LDOAN-2).EQ.0)        GO TO 24

C--       print action into here

   23 IF (NVEXDK(2).NE.0)  CALL SYFORG (-7,0)
      IF (MODEAN.EQ.5)             GO TO 58
      GO TO 20

C--       MODEAN = 0 self material
C-                 1 CDE expansion
C-                 2 comment line
C-                 3 +SELF header line
C-                 4 +CDE  call line
C-                 5 foreign material
C-                 6 control line

   24 JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (JCCTYP.GE.MCCKEE)        GO TO 51

C----------        Self material         ----------------

      IF (NVEXDK(2).EQ.0)          GO TO 20
      IF (JCCTYP.EQ.0)             GO TO 39
      IF (JCCTYP.EQ.MCCNIL)        GO TO 38
      IF (JCCTYP.LE.MCCINC)        GO TO 71
      IF (JCCTYP.LE.MCCXSQ)        GO TO 35
      IF (JCCTYP.LT.MCCSEL)        GO TO 32
      IXPL  = -1
      LEVIF = 0

C--       to here +SELF, +SKIP and fault

      MODEAN = 3
      IF (JCCTYP.NE.MCCSES)        GO TO 28
      IF (MOPTIO(26).NE.0)         GO TO 28
      CALL CCKRAK (IQ(LDOAN+1))

      IF (IQ(LDOAN+2).GE.2)  L = M_SPLIT (LDOAN, IQ(LDOAN+1)+1)
      CALL DPLIST (MODEAN,LDOAN,LUPAN,1)
      GO TO 37

   28 CALL DPLIST (MODEAN,LDOAN,LUPAN,1)
      GO TO 20

C--       to here +_IF etc

   32 IF (JCCTYP.GE.MCCELS)        GO TO 34
      IF (LEVIF.EQ.0)  IQ(LDOAN)= IOR (IQ(LDOAN), KM17)
      LEVIF = LEVIF + 1
      GO TO 39

   34 IF (JCCTYP.EQ.MCCELS)        GO TO 39
      IF (LEVIF.EQ.1)  IQ(LDOAN)= IOR (IQ(LDOAN), KM17)
      LEVIF = MAX (LEVIF-1, 0)
      GO TO 39

C--       sequence call

   35 MODEAN = 4
      CALL DPLIST (MODEAN,LDOAN,LUPAN,0)
      IF (JCCTYP.EQ.MCCCDE)  THEN
          IF (MOPTIO(25).EQ.0)     GO TO 36
        ELSE
          IF (MOPTIO(26).EQ.0)     GO TO 36
        ENDIF
      IF (MOPTIO(5).NE.0)  CALL XEJECT
      GO TO 20

   36 CALL CCKRAK (IQ(LDOAN+1))
      IF (NCCPZ.EQ.0)                      GO TO 20
      IF (IAND(MCCPAR(JCCPT+1),KMD).NE.0)  GO TO 20

   37 CALL SYSEQ
      GO TO 20

C--       comment line

   38 MODEAN = 2
   39 CALL DPLIST (MODEAN,LDOAN,LUPAN,1)
      GO TO 20


C----------        Foreign material         --------------

   51 LEVIF = 0
      IF (JCCTYP.GE.MCCUSE)        GO TO 71
      CALL CCKRAK (IQ(LDOAN+1))
      IF (JCCTYP.EQ.MCCKEE)        GO TO 56
      IXP = MCCPAR(JCCPP+1)
      IXD = MCCPAR(JCCPD+1)
      IF (IXP.EQ.0)                GO TO 52
      IF (IXP.EQ.IXPL .AND. IXD.EQ.IXDL)     GO TO 53
      IXPL = IXP
      IXDL = IXD
      GO TO 54

C--       implied target

   52 IF (IXPL.LT.0)               GO TO 54
      IF (MCCPAR(JCCPD+2).LT.0)    GO TO 53
      IF (IXD.EQ.IXDL)             GO TO 53
      IXDL = IXD
      GO TO 54

C--       same action target as previous

   53 IQ(LDOAN) = IOR (IQ(LDOAN), KM18)
   54 CALL SYFORG (IXPL,IXDL)
      GO TO 57

C--           sequence definition

   56 IXPL = -1
      CALL SYKEEP

   57 MODEAN = 5
      IF (NVEXDK(2).NE.0)  CALL DPLIST (MODEAN,LDOAN,LUPAN,1)

   58 KDOAN = LDOAN - 1
      LDOAN = LQ(KDOAN)
      IF (LDOAN.EQ.0)              GO TO 87
      IF (LQ(LDOAN-2).NE.0)        GO TO 23
      JCCTYP = JBYT (IQ(LDOAN),9,6)
      IF (JCCTYP.GE.MCCSEL)        GO TO 22
      IF (NVEXDK(2).NE.0)  CALL DPLIST (MODEAN,LDOAN,LUPAN,0)
      GO TO 58

C----------        Control line             ---------------

   71 MODEAN = 6
      IF (NVEXDK(2).EQ.0)          GO TO 20
      CALL DPLIST (MODEAN,LDOAN,LUPAN,1)
      IF (JCCTYP.LT.MCCPAT)        GO TO 20
      NQLPAT = NQUSED
      GO TO 20

C----              Done

   87 LQCEND(2) = LQCSTA(2)
      RETURN
      END
