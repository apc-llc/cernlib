CDECK  ID>, INDDECK.
      SUBROUTINE INDDECK

C-    Handle all decks until next +TITLE for nyindex
C.    started 12-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
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
      CHARACTER    LINE*110, LINETT*60
      EQUIVALENCE (LINE,   SLLINE)
      EQUIVALENCE (LINETT, LINE(47:))


C--       set limits of control division 2

   21 LQCSTA(2) = LQCEND(1) + 100
      LQCEND(2) = LQCSTA(2)

C--       clear text division 3

      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)

C----     Ready the deck parameters

      IXEXDEC = 0
      LEXD   = 0
      IF (JDKTYP.EQ.3)             GO TO 46

      LINE(1:68)  = ' '
      LINE(43:46) =                             '. d='
C-    '                        <<<  pname nnnnnn . d=
C-    '_:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5_:.=+=
C-    '                                   nnnnnn . d=dname
C-    ' F=fname                      ---- nnnnnn .   title

      JSLF = IQ(LQHOLD+1)
      IF (JDKTYP.EQ.1)             GO TO 42

C--       deck header is +PATCH, name

      JCCTYP = MCCPAT
      CALL CCKRAK (JSLF)
      IXEXPAT = MCCPAR(JCCPP+1)
      IF (IXEXPAT.EQ.0)  IXEXPAT= 1

      JD_DTN = 0
      IF (JCCBAD.EQ.0)  CALL CCDOPT (1, JD_DTN)
      IF (JD_DTN.EQ.0)  JD_DTN= 1
      JD_DTP  = JD_DTN
      LASMT   = LQ(LHASM-JD_DTN)
      IQ(LASMT+8) = IQ(LASMT+8) + 1

      CALL INDCRP (IXEXPAT,0)
      IF (MOPTIO(17).NE.0)         GO TO 48

      CALL NA_GET (IXEXPAT, LINE,3)
      CALL CRIGHT (LINE,3,34)
      N = 46
      IF (MOPTIO(2).NE.0)          GO TO 43
      IF (NCHCCT.LE.NCHCCD)        GO TO 43
      CALL CCCOMG (1)
      IF (NCHCCC.EQ.0)             GO TO 43
      JP = N + 13
      N  = MIN (JP+NCHCCC, 110)
      NN = N - JP
      LINE(JP+1:JP+NN) = CCCOMF(1:NN)
      GO TO 43

C--       deck header is +DECK, name

   42 JCCTYP = MCCDEC
      CALL CCKRAK (JSLF)
      IXEXDEC = MCCPAR(JCCPD+1)
      IF (IXEXDEC.EQ.0)  IXEXDEC= 1

      JD_DTN = 0
      IF (JCCBAD.EQ.0)  CALL CCDOPT (1, JD_DTN)
      IF (JD_DTN.EQ.0)  JD_DTN= JD_DTP
      LASMT   = LQ(LHASM-JD_DTN)
      IQ(LASMT+8) = IQ(LASMT+8) + 1

      CALL INDCRD
      IF (MOPTIO(16).NE.0)         GO TO 48

      CALL NA_GET (IXEXDEC, LINE,47)
      N = NESLAT - 1
      IF (MOPTIO(2).NE.0)          GO TO 43
      IF (NCHCCT.LE.NCHCCD)        GO TO 43
      CALL CCCOMG (1)
      IF (NCHCCC.EQ.0)             GO TO 43
      JP = MAX (N+1, 46+16)
      N  = MIN (JP+NCHCCC, 110)
      NN = N - JP
      LINE(JP+1:JP+NN) = CCCOMF(1:NN)

   43 CALL CSETDI (NQDKNO, LINE,36,41)
      WRITE (IQPRNT,9043) LINE(1:N)
 9043 FORMAT (A)
      NQUSED = NQUSED + 1
      GO TO 48

C--       deck header is +TITLE or start of PAM

   46 JD_DTP = 1
      CALL INDCRP (IXEXPAT,0)

      LINE(1:46) = ' F=                           ----        .   '
C-                 ' F=fname                      ---- nnnnnn .   title
C-                 '_:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5_:.=+=

      JSLTTL = IQ(LARRV+10)
      CALL LN_GET (JSLTTL, LINETT, 60)
      NTTL = NDSLAT

      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9046) LINETT(1:NTTL)
 9046 FORMAT (' Read Pam file: ',A/)

      CALL CSETDI (NQDKNO, LINE,36,41)

      CALL NA_GET (IXEXPAM, LINE,4)

      N = NTTL + 46
      IF (MOPTIO(17).EQ.0)  THEN
          WRITE (IQPRNT,9047) LINE(1:N)
        ELSE
          WRITE (IQPRNT,9048) LINE(1:N)
        ENDIF
      NQUSED = NQUSED + 3
 9047 FORMAT (/A/)
 9048 FORMAT (/A)

C------        Process the deck

   48 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)
      IF (MOPTIO(24).NE.0)         GO TO 88

C--       analyse the contents

      CALL DOMAPA (0)

C--       process the contents

      CALL INDXQT

C--           step to the next deck

   88 CALL ARRNXD (1)
      NQDKNO = NQDKNO + 1
      IFLGAR = 0
      IF (JDKTYP.LT.3)             GO TO 21
      RETURN
      END
