CDECK  ID>, M_A3SQ.
      SUBROUTINE M_A3SQ

C-    Service routine to M_ANA3, content analysis stage 3:
C-    ready sequence call bank for +CDE +SEQ or +SELF,sname
C-    connecting the called sequences ready for use.
C-    Return IQUEST(1) zero except for +SELF, sname with
C-    sequence sname existing, when the default self material
C-    has to be ignored.
C.    started 15-dec-91

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
      COMMON /INCLC/ N_INCL, IX_INCL(100)
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


C--       MODEAN = 0 ordinary self  1 IF-conditional self
C--                2 +SELF,sname   (3 +REPL,+ADB,+ADD)

      ICODE  = 0
      IF (JCCTYP.EQ.MCCXSQ)        GO TO 37

      IF (JCCIFV.NE.0)  THEN
          IQ(LDOAN) = IOR (IQ(LDOAN), KM5)
          IQUEST(1) = 1
          RETURN
        ENDIF
      IF (JCCTYP.LE.MCCINC)        GO TO 61

      LOSTNZ = 0
      IF (MODEAN.NE.2)             GO TO 31

C----         handle +SELF, sname

C- -   if self material of this deck:
C-       "sname" exists:  replace PREP bank by a new XSQ bank
C-         not existing:  change type of the PREP bank to MCCSEL
C-
C- -   if material into this deck:
C-                       the MAT bank is already a XSQ
C-       "sname" exists:  use it
C-         not existing:  change type of the PREP bank to MCCSEL

      IF (LUPAN.NE.0)  THEN
          IXSQ = IQ(LDOAN+5)
        ELSE
          IXSQ = MCCPAR(JCCPZ+1)
        ENDIF

      KP = LEXP - 3
      KD = 0
      IF (LEXD.NE.0)  KD= LEXD-3

      CALL LOCSEQ (L, IXSQ,KP,KD)
      IF (L.EQ.0)                  GO TO 49

C--       sequence exists

      ICODE = 7

C----         lift XSQ bank to replace the PREP bank

C-    LIFTSQ (kjoin, cc-type to be stored,
C-                   JSL of line to be cracked if non-zero, division)

   31 IF (LUPAN.NE.0)              GO TO 37
      LSQ = LIFTSQ (KDOAN, JCCTYP, 0,2)
      LOSTNZ = IQUEST(1)

      LQ(LSQ-1) = LQ(LDOAN-1)
      IQ(LSQ)   = IOR (IQ(LSQ), IAND(IQ(LDOAN),KM5-1))
      IQ(LSQ+1) = IQ(LDOAN+1)
      IQ(LSQ+2) = 1
      IQ(LSQ+3) = IQ(LDOAN+1) - JSLORG

      LDOAN = LSQ
      IF (LOSTNZ.NE.0)             GO TO 39

C--       connect all sequences called

   37 MX    = LINKSQ (LDOAN,0)
      MEXAN = IOR (MEXAN,MX)
      IQUEST(1) = ICODE
      RETURN

C----         Trouble

   39 CALL M_FAIL ('More than 61 sequence calls on this line')
      RETURN

C----     sequence not defined, convert to ordinary +SELF

   49 CALL SBYT (MCCSEL, IQ(LDOAN),9,6)
      IQUEST(1) = 0
      RETURN

C------       Handle +INCLUDE, sname

   61 IF (JCCTYP.EQ.MCCKIL)        RETURN
   62 IXSQ  = MCCPAR(JCCPZ+1)
      IF (N_INCL.GE.100)              GO TO 66
      N_INCL = N_INCL + 1
      IX_INCL(N_INCL) = IXSQ

      LINCL = LQFIND (IXSQ,1,LQINCL)
      IF (LINCL.EQ.0)                 GO TO 66
      MEXAN = IOR (MEXAN,IQ(LINCL))
      IF (IAND(IQ(LINCL),KM5).EQ.0)   GO TO 67

   66 NEWINC = 1
   67 JCCPZ = JCCPZ + 3
      NCCPZ = NCCPZ - 1
      IF (NCCPZ.GT.0)              GO TO 62
      RETURN
      END
