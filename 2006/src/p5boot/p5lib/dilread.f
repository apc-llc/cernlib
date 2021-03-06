CDECK  ID>, DILREAD.
      SUBROUTINE DILREAD (JFILE)

C-    Load the file JFILE = 1 old, 2 new
C.    started 3-june-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER (KQUSER=9)
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
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------


      JFI    = JFILE
      LFILE  = KQUSER + JFI + 2
      NSLCUM = 0
      NTXCUM = 0
      JDKNEX = 0
      JDKSUB = 3
      NPAT  = 0
      NDECK = 0

      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)

   21 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)
      IF (JDKSUB.EQ.3)  JDKTYP= 3

   22 JSLF = IQ(LQHOLD+1)
      NSLF = IQ(LQHOLD+3)
      JDKSUB  = 0
      IXEXDEC = 0
      IFLGAR  = 0

      IF (JDKTYP.EQ.1)             GO TO 43
      IF (JDKTYP.EQ.2)             GO TO 41

C----     Deck header is +TITLE or start of PAM

      CALL MQLIFT (LFILE, LFILE,-3, JBKPAT,3)
      IF (NOBJTT(3,JFI).EQ.0)  LPAT = LFILE - 3
      NOBJTT(3,JFI) = NOBJTT(3,JFI) + 1

      CALL KRTITL

      IQ(LFILE+1) = JSLF
      IQ(LFILE+4) = IXEXPAM
      IQ(LFILE+7) = NSLCUM
      IQ(LFILE+8) = NTXCUM


      JSLTTL = IQ(LARRV+10)
      IF (NOBJTT(3,JFI).EQ.1)  JSLTTF(JFI)= JSLTTL

      CALL LN_GET (JSLTTL, SLLINE, 72)
      NTXT = NDSLAT

      CALL DPBLAN (0)
                             WRITE (IQPRNT,9027) SLLINE(1:NTXT)
      IF (IQPRNT.NE.IQTYPE)  WRITE (IQTYPE,9027) SLLINE(1:NTXT)

 9027 FORMAT (' Read Pam file: ',A/)

C--           check file starts with +PATCH or +DECK

      IF (JCCTYP.EQ.MCCDEC)  JDKSUB= 1
      IF (JCCTYP.EQ.MCCPAT)  JDKSUB= 2
      IF (JDKSUB.NE.0)  NSLF = 0
      GO TO 42

C----     Deck header is +PATCH, name

   41 JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)
      IXEXPAT = MCCPAR(JCCPP+1)
      IF (IXEXPAT.EQ.0)  IXEXPAT= 1
   42 CALL MQLIFT (LPAT,  LPAT,-1, JBKPAT,3)
      NOBJTT(2,JFI) = NOBJTT(2,JFI) + 1
      LQ(LPAT-3) = LFILE
      IQ(LPAT+1) = JSLF
      IQ(LPAT+4) = IXEXPAT
      IQ(LPAT+5) = NPAT
      IQ(LPAT+7) = NSLCUM
      IQ(LPAT+8) = NTXCUM
      IF (JFI.NE.2)     NPAT= NPAT + 1
      IF (JDKTYP.EQ.3)  LQ(LFILE-4)= LPAT
      LDECK = LPAT - 3
      GO TO 44

C----     Deck header is +DECK, name

   43 JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)
      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1
   44 CALL MQLIFT (LDECK, LDECK,-1,  JBKDEC,3)
      NOBJTT(1,JFI) = NOBJTT(1,JFI) + 1
      LQ(LDECK-3) = LPAT

      NTXF = MLIAD(JSLF+NSLF) - MLIAD(JSLF)

      IQ(LDECK+1) = JSLF
      IQ(LDECK+2) = NSLF
      IQ(LDECK+3) = NTXF
      IQ(LDECK+4) = IXEXDEC
      IQ(LDECK+5) = NDECK
      IQ(LDECK+7) = NSLCUM
      IQ(LDECK+8) = NTXCUM
      IF (JFI.NE.2)  NDECK= NDECK + 1

      IQ(LPAT+2) = IQ(LPAT+2) + NSLF
      IQ(LPAT+3) = IQ(LPAT+3) + NTXF

      IQ(LFILE+2) = IQ(LFILE+2) + NSLF
      IQ(LFILE+3) = IQ(LFILE+3) + NTXF

      NSLCUM = NSLCUM + NSLF
      NTXCUM = NTXCUM + NTXF

C--           step to the next deck

      IF (JDKSUB.NE.0)  THEN
          JDKTYP = JDKSUB
          GO TO 22
        ENDIF

      CALL ARRNXD (1)
      IF (JDKTYP.LE.1)             GO TO 21
      NSLMAX(JFI) = MAX (NSLMAX(JFI),IQ(LPAT+2))
      NTXMAX(JFI) = MAX (NTXMAX(JFI),IQ(LPAT+3))
      IF (JDKTYP.LE.3)             GO TO 21

      NSLTOT(JFI) = NSLCUM
      JSLF = IQ(LQUSER(JFI)+1)
      IF (JSLF+NSLCUM.EQ.LQLEND(2))  INMEM(JFI)= NSLCUM

C--           rewind

      IN_DOX = 0
      CALL ARRIN
      CALL DIDUPL (JFI)
      RETURN
      END
