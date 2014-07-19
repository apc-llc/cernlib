CDECK  ID>, DPLACT.
      SUBROUTINE DPLACT

C-    LIST what action at LUPAN
C.    started 10-feb-92

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*80, MARK*18, MSG*14
      EQUIVALENCE (LINE,SLLINE),    (MSG,SLERRM)
      EQUIVALENCE (MARK,SLLINE(63:))

      IAND (IZV,IZW) = AND (IZV, IZW)

C--           Print clash warning

      LEVCL = JBYT (IQ(LUPAN),14,2)
      IF (LEVCL.EQ.0)              GO TO 31
      IF (LEVCL.LT.NCLASH)         GO TO 31

      MSG = 'Clash level 2:'
C-           _:.=+=.: 1_:.=

      IF (LEVCL.EQ.1)  THEN
          MSG(13:13) = '1'
          JSEV = -1
        ELSE
          JSEV = 0
        ENDIF

      CALL FAILMSG (JSEV,4, 20,MSG)
      GO TO 32

C--           Ready the origin tag

   31 IF (NVEXDK(2).EQ.0)          RETURN
      CALL DPHEAD
   32 NCHTAG = 0
      LNACT  = LUPAN
      LNORG  = LQ(LUPAN-3)
      CALL DPTAG (1,0,0)

      IFLNLO = IAND (IQ(LOWAN),KM5+KM6)
      JTYPE  = JBYT (IQ(LUPAN),9,3)
      LINUM  = IQ(LUPAN+3)

      LINE = ' '

      IF (IFLNLO.NE.0)  THEN
          IF (IAND(IQ(LUPAN),KM12).NE.0)  THEN
              LINE(1:6) = '<nil: '
            ELSE
              LINE(1:6) = '<skip:'
            ENDIF
        ELSE
          LINE(1:4) = '<do:'
        ENDIF

      JVAL1 = IQ(LUPAN+4)
      JVAL2 = IQ(LUPAN+5)
      IF       (JTYPE.EQ.0)  THEN
          MARK(1:4) = 'DEL,'
        ELSEIF (JTYPE.EQ.1)  THEN
          MARK(1:4) = 'REP,'
        ELSEIF (JTYPE.EQ.2)  THEN
          MARK(1:4) = 'ADB,'
        ELSE
          MARK(1:4) = 'ADD,'
          JVAL1 = JVAL2
        ENDIF

      MARK(6:7) = 'C='
      CALL CSETDI (JVAL1,MARK,8,12)
      IF (JVAL2.NE.JVAL1)  THEN
          MARK(13:13) = '-'
          CALL CSETDI (JVAL2,MARK,14,18)
        ENDIF
      CALL CLEFT (MARK,8,18)

C--           Print the acting control line

      JSL = IQ(LUPAN+1)
      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      NTX = MIN (NTX,74)

      CALL DPLINE (LINUM, '    ', LEN(LINE), LINE)
      RETURN
      END
