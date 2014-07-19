CDECK  ID>, SYPAT.
      SUBROUTINE SYPAT

C-    Process all patches of the current PAM file
C.    started 18-july-95

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
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


      IF (JDKTYP.EQ.3)             GO TO 27
      IF (JDKTYP.EQ.1)             GO TO 26

C--       get the patch name

   21 JSLF   = IQ(LQHOLD+1)
      JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)

      IXEXPAT = MCCPAR(JCCPP+1)
      IF (IXEXPAT.NE.0)            GO TO 27
   26 IXEXPAT= 1

C--       find/create the patch bank

   27 CALL CREAPD (IXEXPAT,-1,7)
      IXEXID  = IXEXPAT
      CALL SBYT (NQDKNO, IQ(LEXP+1),13,20)
      MU_PAT = IAND (IQ(LEXP),KM19-1)
      IXEXDEC = 0

C--            Check enough space

   31 IFLGAR = 0
      CALL SETGAP
      IF (JDKNEX.EQ.0)  CALL ARRIVE (2)
      NQNEWH = 7
      NCHTAG = 0
      LDECO  = 0


C------        Ready the deck parameters

      IF (JDKTYP.NE.1)             GO TO 37

C----     Deck header is +DECK, name

      JSLF   = IQ(LQHOLD+1)
      JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)

C--       get the name

      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1
      IXEXID  = IXEXDEC

C--       find the deck bank, set operation mode

   37 LEXD = KQFIND (IXEXDEC,1, LEXP-2,KEXD)
      IF (LEXD.NE.0)  THEN
          MU_DECK = IAND (IQ(LEXD),KM19-1)
        ELSE
          MU_DECK = MU_PAT
        ENDIF

      NSLHAV = IQ(LQHOLD+3)
      IF (IAND(MU_DECK,KM5).EQ.0)  GO TO 71

      CALL MXOPER (0)

C--       start of PAM

      IF (JDKTYP.NE.3)             GO TO 44
      NVEXDK(2) = 1
      IF (NQJOIN.GT.0)  NQJOIN = 0
      IF (NQPAGE.LT.2)  NQJOIN = 1
      CALL DPHEAD
      NQJOIN = -1
      GO TO 51

C--       start of patch or deck

   44 IF (NVEXDK(2).EQ.0)          GO TO 51
      CALL CCJOIN

C------        Process the deck

   51 NSLORG = 0
      NCHTAG = 0
      LLORG  = 0
      LLACT  = 0
      LMODE  = 0

C--       ready the foreign material into this deck

      CALL ACSORT

C--       analyse the contents

      CALL DOMAPA (7)

C--       process the contents

      CALL SYXQT

C------        Processing complete

C--           send the DECK bank to garbage collection

      IF (LEXD.NE.0)  THEN
          L = KQFIND (IXEXDEC,1, LEXP-2, KEXD)
          IF (L.NE.0)       THEN
              CALL TOGARB (KEXD,0)
            ENDIF
        ENDIF

      IF (NVEXDK(2).NE.0)          GO TO 72

C----          Deck not listed, step the Wylbur line number

   71 NQWYL = NQWYL + NSLHAV

C--           step to the next deck

   72 NQDKNO = NQDKNO + 1
      CALL ARRNXD (1)
      IF (JDKTYP.EQ.1)             GO TO 31

      IF (IAND(IQ(LEXP+1),KM5).EQ.0)  THEN
          IF (LQ(LEXP-2).NE.0)  CALL TOGARB (LEXP-2,7)
          IF (LQ(LEXP-3).NE.0)  CALL TOGARB (LEXP-3,7)
        ENDIF

      IF (JDKTYP.GE.3)             RETURN

C--           check stop for RETURN=pname

      IF (IXEXPAT.NE.IQ(LPAM+7))   GO TO 21
      RETURN
      END
