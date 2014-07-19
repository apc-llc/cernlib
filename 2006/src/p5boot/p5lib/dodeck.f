CDECK  ID>, DODECK.
      SUBROUTINE DODECK (JREADY)

C-    Process the next decks until end of patch
C-        JREADY = 0  c/l +DECK to be analysed
C-                 1  ready to run
C.    started 27-feb-92

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
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
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
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
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)


      IFLRDY = JREADY
      NCHTAG = 0
      LLORG  = 0
      LLACT  = 0
      LMODE  = 0

C--            Check enough space

   21 IFLGAR = 0
      CALL SETGAP

C------        Ready the deck parameters

      IF (IFLRDY.NE.0)             GO TO 62
      JD_NEW  = 1
      JD_MULT = 0
      IF (JDKTYP.NE.1)             GO TO 47

C----     Deck header is +DECK, name

   42 INCRAD = MIN (INCRAD,1)
      JSLF   = IQ(LQHOLD+1)
      JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)

C--       get the name

      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1
      IXEXID  = IXEXDEC

C--       find the deck bank, set operation mode

   47 LEXD = KQFIND (IXEXDEC,1, LEXP-2,KEXD)
      IF (LEXD.NE.0)  THEN
          MU_DECK = IAND (IQ(LEXD),KM19-1)
        ELSE
          MU_DECK = MU_PAT
        ENDIF

      CALL MXOPER (0)

      IF (JDKTYP.NE.1)             GO TO 61
      IF (IAND(MU_DECK,KM5).EQ.0)  GO TO 85

C------        Process the deck

   61 NQNEWH = 7
      LDECO  = 0
      NSLORG = 0

   62 IFLRDY = 0
      IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C--       ready the foreign material into this deck

      CALL ACSORT

C--       analyse the contents

      CALL DOMAP
      IF (JANSW.LT.0)              GO TO 84

C--       process the contents

      IF (JANSW.NE.0)  CALL DOXQT
      LQCEND(2) = LQCSTA(2)

C------        Processing complete

      NQDKNO = NQDKNO + 1

C--           send the DECK bank to garbage collection
C-            unless it is the blank deck of P=CRA*

      IF (LEXD.NE.0)  THEN
          L = KQFIND (IXEXDEC,1, LEXP-2, KEXD)
          IF (L.NE.0)       THEN
              IF (L.EQ.LDCRAB)     GO TO 67
              CALL TOGARB (KEXD,0)
            ENDIF
        ENDIF

      IF (NVEXDK(1).EQ.0)          GO TO 88
      IF (JD_NEW.EQ.0)  CALL DPEXTM (1)
      GO TO 88

C--       End of blank deck of P=CRA*

   67 IF (NQERR.NE.0)
     +     CALL P_KILL ('errors in the blank deck of the cradle')

      CALL ASMUSE
      LQ(KEXD) = LQ(LEXD-1)
      IF (NCLASH.EQ.0)  THEN
          IF (MOPUPD.EQ.0)  NCLASH= 2
        ENDIF
      GO TO 88

C----          Skip patch or deck

   84 JDKTYP = 2
   85 CALL ARRSKP
      GO TO 89

C--           step to the next deck

   88 CALL ARRNXD (1)
   89 CONTINUE
      IF (JDKTYP.EQ.1)             GO TO 21
      RETURN
      END
