CDECK  ID>, DOPAM.
      SUBROUTINE DOPAM

C-    Process the next multi-PAM file
C.    started 27-feb-92

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
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
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
      PARAMETER      (NFIMAX=100)
      COMMON /TITLEC/ NFILET, JTIPAM(NFIMAX)
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
C--------------    End CDE              --------------------------------

      IAND (IZV,IZW) = AND (IZV, IZW)


C----         Connect the PAM file

      CALL DOPAMCO

      JDKSUB = 0
      MXFOSV = MX_FORC
      MOPT   = IQ(LPAM+6)

      IFLHOL  = IAND (MOPT,KM2)
      IFLUPD  = IAND (MOPT,KM4)
      IFLMERG = IAND (MOPT,KM5)
      IFLDISP = IAND (MOPT,KM6)

C--           Handle UPDATE mode

      IF (MOPUPD.NE.0)  THEN
        IF (IFLUPD.NE.0)  THEN
            MOPUPD = -1
          ELSE
C-          suspend EXE mode for self on this PAM file
            MX_FORC = IAND (MX_FORC, NOT(KM6))
          ENDIF
        ENDIF

      NHOLD = IQ(LARRV+9)
      IF (NHOLD.EQ.0)              GO TO 31

C----          Resume input of the PAM file

      IQ(LARRV+9) = 0
      CALL ARRIVE (1)

      JSLTTL  = IQ(LARRV+10)
      IXEXPAM = IQ(LARRV+11)
      IF (IQ(LPAM+4).GT.0)         GO TO 42

      NCENT = NQDKNO / 100
      NNUM  = MOD (NHOLD,100)
      NNEW  = 100*NCENT + NNUM
      IF (NNEW.LT.NQDKNO)  NNEW= NNEW + 100
      NQDKNO = NNEW
      JDKTYP = 2
      GO TO 42

C------        Load the title deck to memory      ------------

   31 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C----          Handle the PAM-file title

      JCCTYP = 0
      JD_DTP = 1
      JD_DTN = 1

      CALL KRTITL
      IF (JCCTYP.EQ.MCCPAT)  JDKSUB= 2

      JSLTTL = IQ(LARRV+10)
      IQ(LPAM+13)  = IQ(LPAM+13) + 1
      IF (IQ(LPAM+4).GT.0)         GO TO 42

      IF (NQDKNO.GE.2)  THEN
          NQDKNO = ((NQDKNO-1)/100 + 1) *100
        ELSE
          NQDKNO = 0
        ENDIF
      JDKTYP = 3

C--       create RPAM bank

   42 CALL QSHUNT (KQMAIN,KQPAST)
      CALL MQLIFT (LRPAM, LEXP,1, JBKRPA,3)
      IQ(LRPAM+1) = JSLTTL
      IQ(LRPAM+2) = -IXEXPAM
      IQ(LRPAM+3) = NQDKNO

      CALL LN_GET (JSLTTL, SLLINE, 60)
      NTXT = NDSLAT

C--       PAM file to be read, not skipped

      IF (IQ(LPAM+4).GT.0)         GO TO 71

      CALL DPBLAN (0)
      WRITE (IQPRNT,9043) SLLINE(1:NTXT)
      NQUSED = NQUSED + 2
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9043) SLLINE(1:NTXT)
 9043 FORMAT (' Read Pam file: ',A/)

      IF (NFILET.LT.NFIMAX)  NFILET= NFILET + 1
      JTIPAM(NFILET) = JSLTTL

C------            Loop over all patches

      IF (JDKSUB.NE.0)  JDKTYP= 2
      JDKSUB = 0
      JD_DTP = 1
      JD_DTN = 1

   44 CALL DOPAT
      IF (JDKTYP.EQ.6)               GO TO 81
      IF (IQ(LEXP+2).EQ.IQ(LPAM+7))  GO TO 61
      IF (JDKTYP.EQ.2)               GO TO 44

C----          +TITLE seen, do next PAM file

      IQ(LPAM+5) = IQ(LPAM+5) - 1
      IF (IQ(LPAM+5).LE.0)         GO TO 62
      GO TO 31

C----     Stop input from PAM
C-             for RETURN=name reached
C-             for number of PAM files to be done exhausted

   61 IF (JDKTYP.EQ.2)  IQ(LARRV+9)= NQDKNO
   62 JSLSAV = IQ(LQHOLD+1)
      NSLSAV = IQ(LQHOLD+2)
      IF (IFLHOL.EQ.0)  THEN
          LQLEND(2) = JSLSAV
          LQLSTA(3) = LQLEND(2)
          LQLEND(3) = LQLSTA(3)
          GO TO 82
        ENDIF

      IF (NSLSAV.EQ.0)             GO TO 88
      IF (IQ(LARRV+7).GT.0)  THEN
          IN_DOX = 1
          IN_DO1 = JSLSAV
          CALL ARRIN
          GO TO 88
        ENDIF

      CALL MQLIFT (LASAV,LARRV,-2,JBKASA,1)
      IQ(LASAV+1) = JSLSAV
      IQ(LASAV+2) = NSLSAV
      GO TO 88

C------            Skip the PAM file

   71 IQ(LPAM+4)  = IQ(LPAM+4) - 1
      IQ(LRPAM+4) = -1

      CALL DPBLAN (0)
      WRITE (IQPRNT,9072) SLLINE(1:NTXT)
      NQUSED = NQUSED + 2
 9072 FORMAT (' Skip  Pam file: ',A/)

      JDKTYP = 3
      CALL ARRSKP
      IF (JDKTYP.LT.6)             GO TO 31

C------        End of multi-PAM reached

   81 CONTINUE

C--       Detach or Rewind

   82 IF (IFLHOL.EQ.0)  THEN
          IN_DOX = -1
        ELSE
          IN_DOX =  0
        ENDIF
      CALL ARRIN

   88 IQ(LQHOLD+2) = 0
      IQ(LPAM+12)  = IQ(LPAM+12) + IQ(LARRV+12)
      IQ(LARRV+12) = 0
      MOPUPD  = ABS(MOPUPD)
      MX_FORC = MXFOSV
      RETURN
      END
