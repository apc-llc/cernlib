CDECK  ID>, ASMDUMP.
      SUBROUTINE ASMDUMP (MSG)

C-    Dump the ASM d/s
C.    started 13-oct-93

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
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
      CHARACTER    MSG*(*)
      CHARACTER    SLCOL(512)*1
      EQUIVALENCE (SLCOL,SLLINE)


      WRITE (IQPRNT,9001) MSG
 9001 FORMAT (/'1Dump the ASM data stucture ',A)

      JASM  = 0
   21 JASM  = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 89
      LASMT = LQ(LHASM-JASM)
      IF (LASMT.EQ.0)              GO TO 89

      JNAME = JASM
      NN    = LNBLNK (CH_TYP(JNAME))
      WRITE (IQPRNT,9021) CH_TYP(JNAME)(1:NN)
 9021 FORMAT (/' for data type ',A)

      JSTR  = 0
      LASML = LASMT

   24 WRITE (IQPRNT,9024) JSTR
 9024 FORMAT (/5X,'stream :',I1)

      MUSE = IQ(LASML+1)
      IF (MUSE.GT.0)               GO TO 27
      IF (MUSE.LT.0)  THEN
          WRITE (IQPRNT,9025) MUSE
          GO TO 34
        ENDIF

      WRITE (IQPRNT,9026) MUSE
      GO TO 31

 9025 FORMAT (17X,'binding',I5,' to be resolved')
 9026 FORMAT (17X,'binding',I5,' stream is physical')

   27 JUA = MUSE / 8
      JUB = MOD (MUSE,8)
      SLLINE(1:20) = CH_TYP(JUA)
      IF (JUB.NE.0)  THEN
        SLCOL(17) = ':'
        CALL CSETDI (JUB,SLLINE,18,20)
       ENDIF
      CALL CLEFT (SLLINE,1,20)
      NN = NDSLAT
      WRITE (IQPRNT,9027) SLLINE(1:NN)
 9027 FORMAT (16X,'bound to  ',A)
      GO TO 34


   31 MODE = IQ(LASML+2)
      WRITE (IQPRNT,9031) MODE
 9031 FORMAT (12X,' output mode',I5)
 9032 FORMAT (12X,'IX file name',I5,:,1X,A)
 9033 FORMAT (12X,'IX    prefix',I5,:,1X,A)
 9034 FORMAT (12X,'IX extension',I5,:,1X,A)
 9035 FORMAT (12X,'IX  r/header',I5,:,1X,A)
 9036 FORMAT (12X,'IX  c/ch sub',I5,:,1X,A)

      JUSE = IQ(LASML+3)
      IF (JUSE.LE.0)  THEN
          WRITE (IQPRNT,9032) JUSE
        ELSE
          CALL NA_GET (JUSE,SLLINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9032) JUSE, SLLINE(1:N)
        ENDIF

      IF (MODE.LT.2)               GO TO 34
      JUSE = IQ(LASML+4)
      IF (JUSE.LE.0)  THEN
          WRITE (IQPRNT,9033) JUSE
        ELSE
          CALL NA_GET (JUSE,SLLINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9033) JUSE, SLLINE(1:N)
        ENDIF

   34 JREA = IQ(LASML+11)
      IF (JREA.NE.0)               GO TO 41
      JUSE = IQ(LASML+5)
      JUSX = JUSE
      IF (JUSX.LT.0)  JUSX= IQ(LASMT+5)
      IF (JUSX.LE.0)  THEN
          WRITE (IQPRNT,9034) JUSE
        ELSE
          CALL NA_GET (JUSX,SLLINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9034) JUSE, SLLINE(1:N)
        ENDIF

      JUSE = IQ(LASML+6)
      JUSX = JUSE
      IF (JUSX.LT.0)  JUSX= IQ(LASMT+6)
      IF (JUSX.LE.0)  THEN
          WRITE (IQPRNT,9035) JUSE
        ELSE
          CALL NA_GET (JUSX,SLLINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9035) JUSE, SLLINE(1:N)
        ENDIF

      JUSE = IQ(LASML+7)
      JUSX = JUSE
      IF (JUSX.LT.0)  JUSX= IQ(LASMT+7)
      IF (JUSX.LE.0)  THEN
          WRITE (IQPRNT,9036) JUSE
        ELSE
          CALL NA_GET (JUSX,SLLINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9036) JUSE, SLLINE(1:N)
        ENDIF
      GO TO 49

C--           logical stream re-assigned

   41 JA = JREA/8
      JS = MOD (JREA,8)
      NN = LNBLNK (CH_TYP(JA))

      WRITE (IQPRNT,9044) CH_TYP(JA)(1:NN),JS
 9044 FORMAT (12X,'data re-assigned to stream ',A,':',I1)

   49 JSTR = JSTR + 1
      IF (JSTR.GT.4)               GO TO 21
      LASML = LQ(LASMT-JSTR-1)
      GO TO 24

   89 CONTINUE
      RETURN
      END
