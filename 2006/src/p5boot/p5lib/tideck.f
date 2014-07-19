CDECK  ID>, TIDECK.
      SUBROUTINE TIDECK

C-    Tidy all decks until next +TITLE
C.    started 26-may-94

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
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
     +,       IXBLADK, IXSQSP(7), NTRBLA,NMODIF, JSLTTL,   LASTTI
C--------------    End CDE              --------------------------------


C--           Check file starts with +PATCH or +DECK

      IF (JCCTYP.EQ.MCCDEC)  JDKTYP= 1
      IF (JCCTYP.EQ.MCCPAT)  JDKTYP= 2
      NOFFBD = 0

C--       make sure the deck is in memory

   21 IF (JDKNEX.EQ.0)  THEN
          CALL ARRIVE (2)
          CALL TIDISTB
          NTRBLA = NTRBLA + IQUEST(1)
        ENDIF

C--       set limits of control division 2

   22 LQCSTA(2) = LQCEND(1) + 100
      LQCEND(2) = LQCSTA(2)

C--       clear text division 3

      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)

C--------      Ready the deck parameters

      NQNEWH = 7
      IF (JDKTYP.EQ.1)             GO TO 46
      IXEXDEC = 0
      IF (JDKTYP.EQ.2)             GO TO 41

C----     deck header is +TITLE or start of PAM

      JD_DTP = 0
      JD_DTD = 0
      GO TO 49

C----     deck header is +PATCH, name

   41 JSLF = IQ(LQHOLD+1)
      IF (JDKNEX.NE.1)             GO TO 44

C--           check next deck is BLANKDEK

      JSLN   = JSLF + IQ(LQHOLD+3)
      JCCTYP = MCCDEC
      CALL CCKRAK (JSLN)
      IX = MCCPAR(JCCPD+1)
      IF (IX.NE.IXBLADK)           GO TO 44

      NOFFBD = IQ(LQHOLD+3)
      JTXN   = MLIAD(JSLN)
      TEXT(JTXN) = '*'
      CALL ARRNXD (0)
      IF (JDKNEX.EQ.0)             GO TO 21

   44 JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)
      IXEXPAT = MCCPAR(JCCPP+1)
      IF (IXEXPAT.EQ.0)  IXEXPAT= 1
      IXEXID = IXEXPAT

      JD_DTD = 0
      IF (JCCBAD.EQ.0)  THEN
          CALL CCDOPT (1, JD_DTD)
          IF (JD_DTD.EQ.0)  JD_DTD= 1
        ENDIF
      JD_DTP = JD_DTD
      GO TO 49

C----     deck header is +DECK, name

   46 JSLF   = IQ(LQHOLD+1)
      JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)
      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1
      IXEXID  = IXEXDEC

      JD_DTD = 0
      IF (JCCBAD.EQ.0)  THEN
          CALL CCDOPT (1, JD_DTD)
          IF (JD_DTD.EQ.0)  JD_DTD= JD_DTP
        ENDIF

C--       if type is INCLUDE: handle as CC

   49 IF (JD_DTD.EQ.JIN_TYP)  JD_DTD= JCC_TYP

C--------      Process the deck

      LEXD  = 0
      NSLORG = 0

C--       restore the leading + for +DECK, BLANKDEK.

      IF (NOFFBD.NE.0)  THEN
          JSLN   = IQ(LQHOLD+1) + NOFFBD
          JTXN   = MLIAD(JSLN)
          TEXT(JTXN) = '+'
        ENDIF

C--       analyse the contents

      CALL DOMAPA (0)
      IF (JDKTYP.EQ.3)             GO TO 67

      IF (NOFFBD.NE.0)  THEN
          L = LQFIND (JSLN,1, LQPREP)
          IF (L.NE.0)  LQ(L-2) = -1
          NOFFBD = 0
        ENDIF

C--       transform the contents

      IF (JD_DTD.EQ.1)  CALL TICOMM
      CALL TISEQ
      CALL TILIST

C--       send the deck to output

   67 CALL TISEND

C--           step to the next deck

      CALL ARRNXD (1)
      IFLGAR = 0
      IF (JDKTYP.LT.3)             GO TO 21
      RETURN
      END
