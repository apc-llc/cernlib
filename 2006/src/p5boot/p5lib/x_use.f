CDECK  ID>, X_USE.
      SUBROUTINE X_USE

C-    Process   +USE, +XDIV, +DIV, +LIST, +EXE
C-              +SUSPEND, +FORCE

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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      DIMENSION    MV(2)
      EQUIVALENCE (MV(1),IQUEST(1))

      PARAMETER   (NPOSS = 10)
      CHARACTER    POSS(NPOSS)*8

      PARAMETER    (KBOFF=KM8, KBREP=KM9)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

      DATA  POSS  / 'E*XECUTE'
     +,             'L*IST   '
     +,             'D*IVERT '
     +,             'X*DIVERT'
     +,             'ONL*Y   '
     +,             'T*RANSMI'
     +,             'I*NHIBIT'
     +,             'OFF     '
     +,             'R*EPEAT '
     +,             'ENV*IRON' /


      MASKOP = 0
      IF (NCCPT.NE.0)  THEN
          CALL CCOPT (POSS,NPOSS)
          IF (JCCBAD.NE.0)             GO TO 91
          MASKOP = MCCPAR(JCCPT+2)
        ENDIF

      MXNEW = IAND (MASKOP,KM5-1)
      IF (JCCTYP.GE.MCCFOR)        GO TO 71

C----------        Doing +USE, +EXE, etc                     ---------

C-:   MODE=  1 ONLY,  2 TRANS,  4 INHIBIT

   31 MODE = ISHFTR (MASKOP,4)
      MODE = IAND (MODE,15)
      IF (MODE.EQ.3)               GO TO 91
      IF (MODE.GT.4)               GO TO 91

      J = MCCIMI - JCCTYP
      CALL SBIT1 (MXNEW,J)

C--       Propagation permission filter

      IF (INCRAD.EQ.0)  MXNEW= IAND (MXNEW, IOR(MU_DECK,KM3))

C------       Process global activation          ------

      IF (JCCPD.NE.0)              GO TO 51
      IF (JCCPP.NE.0)              GO TO 41

      IF (INCRAD.EQ.0)             GO TO 92
      IF (MODE.EQ.4)  THEN
          IF (MXNEW.GE.16)         GO TO 91
        ENDIF

      IQUEST(11) = MXNEW
      IQUEST(12) = MODE
      IQUEST(13) = 0
      CALL X_UPUT (MU_GLOB)

C--           propagate through all patches and decks

      LQ(LLPAST-1) = 0
      L = LEXP
   34 CALL MXJOIN (MU_GLOB,IQ(L))
      CALL X_UDECK (L)
      L = LQ(L-1)
      IF (L.NE.0)                  GO TO 34
      RETURN

C------       Process patch activation           ------

   41 IXUPAT = MCCPAR(JCCPP+1)
      IF (IXUPAT.EQ.0)             GO TO 48

      CALL CREAPD (IXUPAT,-1, 0)

      IQUEST(11) = MXNEW
      IQUEST(12) = MODE
      IQUEST(13) = MX_TRAN
      CALL X_UPUT (IQ(LCRP))
      IF (IAND(MASKOP,KBREP).NE.0)  IQ(LCRP+1)= IOR (IQ(LCRP+1),KM5)

C--           set "used from"

      IF (MXNEW.LT.16)             GO TO 45
      IF (MODE.EQ.4)               GO TO 44
      IF (IAND(IQ(LCRP),KM5).EQ.0) GO TO 45
   44 LQ(LCRP-4) = LEXP

C--           propagate through all decks

   45 CALL X_UDECK (LCRP)
   48 NCCPP = NCCPP - 1
      IF (NCCPP.EQ.0)              RETURN
      JCCPP = JCCPP + 3
      GO TO 41

C------       Process deck activation            ------

   51 IF (NCCPP.NE.1)              GO TO 91
      IXUPAT = MCCPAR(JCCPP+1)
      IF (IXUPAT.EQ.0)             RETURN

   54 IXUDEC = MCCPAR(JCCPD+1)

      CALL CREAPD (IXUPAT,IXUDEC, 0)

      IQUEST(11) = MXNEW
      IQUEST(12) = MODE
      IQUEST(13) = MX_TRAN
      CALL X_UPUT (IQ(LCRD))

C--       blank deck and D=CRA* of P=CRA* can change their own status

      IF (INCRAD.GE.2)   THEN
          IF (LCRP.EQ.LEXP)  CALL X_UDECK (0)
        ENDIF

   58 NCCPD = NCCPD - 1
      JCCPD = JCCPD + 3
      IF (NCCPD.NE.0)              GO TO 54

C--           set "used from"

      IF (MODE.EQ.4)               RETURN
      IF (MXNEW.LT.16)             RETURN
      IQ(LCRP+1) = IOR (IQ(LCRP+1),KM4)
      IF (LQ(LCRP-4).EQ.0)  LQ(LCRP-4)=LEXP
      RETURN

C----------        +FORCE,   (OFF,) EXE, LIST, DIV, XDIV     ---------
C--                +SUSPEND, (OFF,) EXE, LIST, DIV, XDIV

   71 MXNEG = KM5-1 - MXNEW
      MXOFF = IAND (MASKOP,KBOFF)
      MV(1) = IAND (ISHFTR(MX_FORC,5),  KM5-1)
      MV(2) = IAND (ISHFTR(MX_FORC,14), KM5-1)


      IF (JCCTYP.EQ.MCCFOR)  THEN
          IF (MXOFF.EQ.0)  MV(2) =  IOR (MV(2),MXNEW)
          IF (MXOFF.NE.0)  MV(2) = IAND (MV(2),MXNEG)
        ELSE
          IF (MXOFF.EQ.0)  MV(1) = IAND (MV(1),MXNEG)
          IF (MXOFF.NE.0)  MV(1) =  IOR (MV(1),MXNEW)
        ENDIF

      MX_FORC = IOR (ISHFTL(MV(1),5), ISHFTL(MV(2),14))
      RETURN

C-----             Faulty control line

   91 CALL M_FAIL ('Bad syntax')
      RETURN

   92 CALL M_FAIL ('This is allowed only in the cradle')
      RETURN
      END
