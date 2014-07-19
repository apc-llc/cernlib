CDECK  ID>, M_ANA1.
      SUBROUTINE M_ANA1

C-    Content analysis, stage 1:
C-    find all control lines present in the current deck;
C-    each control line starts a new PREP bank, unless it is
C-    a soft c/line covered by a +KEEP +REPL etc. in update mode;
C-    c/lines without associated text are held in a 1-line PREP bank.
C-    -  this is not called if the deck is processed under +PAM, T=UPD
C-       and hence all c/l's in the deck are treated as ordinary lines
C.    started 11-dec-91

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
C--------------    End CDE              --------------------------------

      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)


      LDO  = LQPREP
      JSLA = IQ(LDO+1)
      JSLE = JSLA + IQ(LDO+2)
      JSLG = JSLA

C----         Find next control line, split on it

   21 IF (JSLG.GE.JSLE)            RETURN
      CALL NEXTCC ('+',JSLG,JSLE,JSLF,JCCTYP)
      IF (JCCTYP.EQ.0)             RETURN

   22 IF (JSLF.GT.JSLA)  LDO= M_SPLIT (LDO,JSLF)

      IQ(LDO) = IOR (IQ(LDO), ISHFTL(JCCTYP,8))
      JSLA = IQ(LDO+1)
      JSLG = JSLA + 1

C--       control-lines with associated material are:
C--       +_xxx  +SELF +SKIP +KEEP +REPL +ADB +ADD

      IF (JCCTYP.LT.MCCTRU)        GO TO 27
      IF (JCCTYP.LE.MCCEND)        GO TO 21
      IF (JCCTYP.GE.MCCUSE)        GO TO 27
      IF (JCCTYP.GE.MCCKEE)        GO TO 31
      IF (JCCTYP.NE.MCCSEL)        GO TO 21

C--       for +SELF, sname change the type to MCCSES

      CALL CCKRAK (JSLA)
      IF (NCCPZ.EQ.0)              GO TO 21
      CALL SBYT (MCCSES, IQ(LDO),9,6)
      GO TO 21

C--       stand-alone control-line

   27 IF (JSLG.GE.JSLE)            RETURN
      LDO  = M_SPLIT (LDO,JSLG)
      JSLA = JSLG
      GO TO 21

C----         Found the start of an action, if Update mode:
C--           handle all soft c/lines inside as ordinary lines

   31 IF (JCCTYP.EQ.MCCDEL)        GO TO 27
      IF (MOPUPD.EQ.0)             GO TO 21
   37 CALL NEXTCC ('+',JSLG,JSLE,JSLF,JCCTYP)
      IF (JCCTYP.EQ.0)             RETURN
      IF (JCCTYP.GE.MCCSEL)        GO TO 22
      JSLG = JSLF + 1
      IF (JSLG.LT.JSLE)            GO TO 37
      RETURN
      END
