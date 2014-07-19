CDECK  ID>, DOCRAD.
      SUBROUTINE DOCRAD

C-    Process the cradle
C.    started 27-feb-92

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
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
      PARAMETER   (IXCRA=2)
      CHARACTER    MARK*4

      IAND (IZV,IZW) = AND (IZV, IZW)


      LARRV   = LACRAD
      JREADY  = 1
      NSLORG  = 1
      NDKOFF  = 0
      JD_DTP  = 1
      JD_DTN  = 0
      JD_NEW  = 1
      IFLMERG = 0
      IFLDISP = 0
      IXEXPAM = 0
      IF (INCRAD.EQ.3)             GO TO 31

C------      Restart the cradle after having read a PAM file

      CALL VZERO (IQ(LQHOLD+1),3)
      CALL ARRIVE (1)
      IF (JDKTYP.EQ.2)             GO TO 27

C--       lift PAT bank for P=CRA*

   24 CALL CREAPD (IXCRA,-1,1)
      CALL SBYT (NQDKNO, IQ(LEXP+1),13,20)
      LPCRA = LEXP
   25 LEXD  = 0
      MU_PAT = IAND (IQ(LEXP), KM19-1)
      MU_DECK = MU_PAT
      CALL MXOPER (0)

      IXEXPAT = IXCRA
      IXEXDEC = IXCRA
      IXEXID  = IXCRA
      LDECO   = 0
      NQNEWH  = 1
      NQJOIN  = -1
      NSLORG  = 1
      INCRAD  = 2


   27 IF (JDKTYP.GE.6)             GO TO 79
      IF (JDKTYP.GE.4)             GO TO 42
      IF (JDKTYP.EQ.0)             GO TO 29
      INCRAD = 1
      JREADY = 0
      NSLORG = 0
      NDKOFF = 0
      IF (JDKTYP.GE.2)             GO TO 34
      GO TO 31

C--             Do next deck

   29 NQDKNO = NQDKNO - NDKOFF
      JREADY = 1

   31 JDKTYP = 1
      CALL DODECK (JREADY)
      GO TO 37

C--             do next patch

   34 INCRAD = 1
      IF (JDKTYP.EQ.2)  THEN
          CALL DOPAT
        ELSE

C--             skip for +TITLE

          JDKTYP = 2
          CALL ARRSKP
        ENDIF

   37 IF (JDKTYP.GE.6)             GO TO 79
      IF (JDKTYP.LE.3)             GO TO 34

C----        Handle +PAM or +QUIT

      NSLORG = IQ(LQHOLD+1) - JSLORG
      NDKOFF = 1

   42 JSLCRA = IQ(LQHOLD+1)
      NSLCRA = IQ(LQHOLD+2)
      JTXCRA = MLIAD(JSLCRA)
      NTX    = MLIAD(JSLCRA+1) - JTXCRA - NCHNEWL
      JCCTYP = JPTYPE (TEXT(JTXCRA))

      CALL CCKRAK (JSLCRA)
      IF (JCCBAD.NE.0)  THEN
          MARK = ' *! '
          GO TO 44
        ENDIF

      CALL CCPROC
      IF (JCCIFV.EQ.0)  THEN
          MARK = '  + '
        ELSE
          MARK = '  ( '
        ENDIF

C--       print the c/line

   44 IF (INCRAD.GE.2)  THEN
          LINUM = NSLORG
        ELSE
          LINUM = 1
        ENDIF

      CALL DPBLAN (1)
      CALL DPLINE (LINUM,MARK,NTX,TEXT(JTXCRA))

C--       exit if faulty c/line

      IF (JCCBAD.NE.0)             GO TO 91

C--       continue deck if IF deselected c/line

      IF (JCCIFV.EQ.0)             GO TO 61
      IQ(LQHOLD+1) = IQ(LQHOLD+1) + 1
      IQ(LQHOLD+2) = IQ(LQHOLD+2) - 1
      NSLORG = NSLORG + 1

      IF (IQ(LQHOLD+2).EQ.0)  THEN
          CALL ARRIVE (1)
        ELSE
          CALL ARRNXD (-1)
        ENDIF

      IF (INCRAD.GE.2)             GO TO 27
      IF (JDKTYP.GE.1)             GO TO 27
      NDKOFF = 0
      IF (IXEXPAT.NE.IXCRA)        GO TO 24
      GO TO 25

C------       Accepted control line +PAM or +QUIT

   61 IF (JCCTYP.EQ.MCCQUI)        GO TO 79
      NSLORG = 0

C--       Digest the +PAM parameters to bank at LPAM

      CALL DOAPAM
      IF (JCCBAD.NE.0)             GO TO 91

C--       Save cradle material after +PAM, ...

      JSLCRA = JSLCRA + 1
      NSLCRA = NSLCRA - 1
      IF (NSLCRA.EQ.0)             GO TO 77

      IF (IQ(LARRV+7).LT.0)        GO TO 76
      JSLE = JSLCRA + NSLCRA
      CALL NEXTDE (JSLCRA,JSLE,JSLF,JTYP,-2)
      IF (JTYP.EQ.0)               GO TO 76

C--       reposition the cradle file to the start of next deck

      IN_DOX = 1
      IN_DO1 = JSLF
      CALL ARRIN

      NSLCRA = JSLF - JSLCRA
      IF (NSLCRA.EQ.0)             GO TO 77
   76 CALL MQLIFT (LASAV, LACRAD,-2, JBKASA,1)
      IQ(LASAV+1) = JSLCRA
      IQ(LASAV+2) = NSLCRA
   77 IQ(LQHOLD+2) = 0
      IQ(LPAM+14)  = IQ(LPAM+14) + 1
      JANSW = 0
      RETURN

C--       +QUIT or EOF reached

   79 JANSW = 1
      RETURN

C----         Trouble

   91 IF (JCCTYP.EQ.MCCQUI)        GO TO 79
      CALL P_FATAL ('faulty line +PAM,...')
      END
