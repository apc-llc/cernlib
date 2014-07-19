CDECK  ID>, PEND.
      SUBROUTINE PEND

C-    Print the run summary
C.    started 11-feb-92

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
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
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
      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
      COMMON /NAMEC/ NA_OCC, NA_CUT, NA_MXSL, NA_MXTX
C--------------    End CDE              --------------------------------

      CHARACTER    LINE*128, COL(128)*1
      EQUIVALENCE (LINE,COL,SLLINE)

      PARAMETER    (KI=21, KF=29, KT=36, KS=43, KO=51)

      IAND (IZV,IZW) = AND (IZV, IZW)

C| Processing mode selections                        Other status bits for P=X
C|
C| U - use                                           F - skipped by +PATCH,X,IF=
C| L - list                                           M - skipped by +IMITATE,X
C| E - exe                                             X - at least 1 deck to AS
C| D - divert                                           P - partial +USE,X,D=...
C| X - xdivert                Self +         Self        R - P=X,T=REPEAT
C|                    Inhibit Foreign  Trans   only       Q - quoted in IF=X
C|                    ------- ------- ------ ------
C|_:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5_:.=+=.: 6_:.=+=.: 7_
C|        Patch        ULEDX   ULEDX   LEDX   LEDX   FMXPRQ  Used from P=
C|
C|      0 CRA*               . ULE   .      . LE   .     R   CRA*
C| P5MODEL /20 931111 21.50
C|      0 @P5MODEL           .  LE   .      . LE   .
C|      1 *MO                . ULE   .      . LE   .         CRA*
C|      2 ACTZ               . ULE   .      . LE   .         *MO
C|     16 VCDE               . ULE   .      . LE   .      Q  *MO
C|     19 ALPHA              . ULE   .      . LE   .      Q  *MO
C|     23 BETA               . ULE   .      . LE   .      Q  ALPHA
C|     30 CRA*               . ULE   .      . LE   .     R   CRA*
C|
C| Quoted patches not seen on the PAM files
C|
C|      0 NEWF               . ULED  .      . LED  .      Q  *MO
C|      0 QMSUN              .  LE   .      . LE   .      Q
C|
C| Other patches not seen
C|
C|      0 ZLONGNAME012 U     .  LE   .      . LE   .         CRA*
C|      0 ZLONGNAME01..U     .  LE   .      . LE   .         CRA*
C|_:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5_:.=+=.: 6_:.=+=.: 7_

      IF (NQERR.NE.0)  MOPTIO(22)= 1
      IF (MOPTIO(22).EQ.0)            GO TO 51

      IF (NQPAGE.EQ.1)  THEN
          CALL DPBLAN (1)
          LINE(1:1) = ' '
        ELSE
          LINE(1:1) = '1'
        ENDIF
      WRITE (IQPRNT, 9001) LINE(1:1)
 9001 FORMAT (A,'--------------------------------------'
     F/' Summary of patches in processing order'/
     F/' Processing mode selections',24X,'Other status bits for P=X'/
     F/' U - use ',42X,'F - skipped by +PATCH,X,IF=...'
     F/' L - list ',42X,'M - skipped by +IMITATE,X'
     F/' E - exe   ',42X,'X - not +EXE but deck to ASM'
     F/' D - divert ',42X,'P - partial +USE,X,D=...'
     F/' X - xdivert',16X,'Self +',9X,'Self',8X,'R - P=X,T=REPEAT'
     F/19X,' Inhibit Foreign  Trans   only',7X,'Q - quoted in IF=X'
     F/19X,' ------- ------- ------ ------',
     F/8X,'Patch',6X,
     F     '  ULEDX   ULEDX   LEDX   LEDX   FMXPRQ  Used from P='/)
 9000 FORMAT (A)

      CALL QSHUNT (KQMAIN,KQPAST)
      CALL QTOPSY (KQPAST)
      LQ(LLPAST-1) = 0
      LBK  = LQPAST
      MODE = 0

C------       Do one list of patch summary

   21 MST    = IQ(LBK)
      MW1    = IQ(LBK+1)
      IXNAME = IQ(LBK+2)

      CALL QNAMEX (LBK)
      IF (IQFOUL.NE.0)  THEN
          WRITE (IQPRNT,9822) LBK
          CALL P_CRASH ('trouble in PEND')
        ENDIF
 9822 FORMAT (' ***!!! Trouble at LBK =',I9)
C--           check to be printed in secondary lists

      IF (MODE.NE.0)  THEN
          IF (IXNAME.EQ.0)         GO TO 29
          MM = IAND (MW1,KM6)
          IF (MODE.EQ.1) THEN
              IF (MM.EQ.0)  THEN
                  NOTHER = 7
                  GO TO 29
                ENDIF
            ELSE
              IF (MM.NE.0)         GO TO 29
            ENDIF
          GO TO 26
        ENDIF

C----         doing the primary list

      IF (IXNAME.GE.0)             GO TO 26

C--           bank RPAM

      LINE(1:4) = '    '
      CALL LN_GET (MW1, LINE(2:), 40)
      N = NDSLAT + 1
      CALL CSQMBL (LINE,2,N)
      N = NESLAT - 1
      WRITE (IQPRNT,9000)
      GO TO 28

C--           bank PAT

   26 NDKNO = JBYT (MW1,13,20)

      LINE  = ' '
      CALL CSETDI (NDKNO,LINE,2,7)

      CALL NA_GET (IXNAME,LINE(1:21),9)
      IF (NFSLAT.GT.12)  LINE(20:21) = '..'

      LINE(28:50) = '.       .      .      .'

      IF (IAND(MST,KM10) .EQ.0)    COL(KI+1) = 'U'
      IF (IAND(MST,KM7)  .EQ.0)    COL(KI+2) = 'L'
      IF (IAND(MST,KM6)  .EQ.0)    COL(KI+3) = 'E'
      IF (IAND(MST,KM8)  .EQ.0)    COL(KI+4) = 'D'
      IF (IAND(MST,KM9)  .EQ.0)    COL(KI+5) = 'X'

      IF (IAND(MST,KM5)  .NE.0)    COL(KF+1) = 'U'
      IF (IAND(MST,KM2)  .NE.0)    COL(KF+2) = 'L'
      IF (IAND(MST,KM1)  .NE.0)    COL(KF+3) = 'E'
      IF (IAND(MST,KM3)  .NE.0)    COL(KF+4) = 'D'
      IF (IAND(MST,KM4)  .NE.0)    COL(KF+5) = 'X'

      IF (IAND(MST,KM12) .NE.0)    COL(KT+2) = 'L'
      IF (IAND(MST,KM11) .NE.0)    COL(KT+3) = 'E'
      IF (IAND(MST,KM13) .NE.0)    COL(KT+4) = 'D'
      IF (IAND(MST,KM14) .NE.0)    COL(KT+5) = 'X'

      IF (IAND(MST,KM16) .NE.0)    COL(KS+2) = 'L'
      IF (IAND(MST,KM15) .NE.0)    COL(KS+3) = 'E'
      IF (IAND(MST,KM17) .NE.0)    COL(KS+4) = 'D'
      IF (IAND(MST,KM18) .NE.0)    COL(KS+5) = 'X'


      IF (IAND(MW1,KM1)  .NE.0)    COL(KO+1) = 'F'
      IF (IAND(MW1,KM2)  .NE.0)    COL(KO+2) = 'M'
      IF (IAND(MW1,KM3)  .NE.0)    COL(KO+3) = 'X'
      IF (IAND(MW1,KM4)  .NE.0)    COL(KO+4) = 'P'
      IF (IAND(MW1,KM5)  .NE.0)    COL(KO+5) = 'R'
      IF (IAND(MW1,KM6)  .NE.0)    COL(KO+6) = 'Q'

C--           add "used from"

      IX = LQ(LBK-4)
      IF (IX.NE.0)  IX= IQ(IX+2)
      IF (IX.EQ.0)  THEN
          N = LENOCC (LINE(1:60))
        ELSE
          CALL NA_GET (IX,LINE,60)
          N = NESLAT - 1
        ENDIF

C--           print the line and loop

   28 WRITE (IQPRNT,9000) LINE(1:N)

   29 LBK = LQ(LBK-1)
      IF (LBK.NE.0)                GO TO 21

C--           End of the list

      IF (MODE.EQ.2)               GO TO 39
      IF (MODE.EQ.1)               GO TO 38

C--           remove bank for p=PY_VS5 if not quoted

      IXU = NA_LONG ('PY_VS5')
      L = KQFIND (IXU,2, KQMAIN,K)
      IF (L.NE.0)  THEN
          IF (IAND(IQ(L+1),KM6).EQ.0)  LQ(K)= LQ(L-1)
        ENDIF

      IF (LEXP.EQ.LLPAST)          GO TO 39

      WRITE (IQPRNT,9037)
 9037 FORMAT (/' Quoted patches not seen on the PAM files'/)

      CALL QSORTN (2,KQMAIN)
      NOTHER = 0
      LBK  = LEXP
      MODE = 1
      GO TO 21

   38 IF (NOTHER.EQ.0)             GO TO 39

      WRITE (IQPRNT,9038)
 9038 FORMAT (/' Other patches not seen'/)

      LBK  = LEXP
      MODE = 2
      GO TO 21
   39 CONTINUE

C------       Name stack occupation

      JSL  = LQLSTA(5)
      JSLE = NA_OCC + 1
      JTX  = MLIAD(JSL)
      JTXE = MLIAD(JSLE)

      NSLOCC = JSLE - JSL
      NTXOCC = JTXE - JTX
      NSLAV  = LQLEND(5) - JSL
      NTXAV  = NSIZETX - JTX

      NSLPRO = (100 * NSLOCC) / NSLAV
      NTXPRO = (100 * NTXOCC) / NTXAV

      WRITE (IQPRNT,9050) NA_CUT, NSLPRO,NSLAV, NTXPRO,NTXAV
      NQLLBL = 0
 9050 FORMAT (/' Name stack:',7X,
     F  'significant name length is',I3,' characters'
     F/ 12X,I5,'% used of',I7,' name slots available'
     F/ 12X,I5,'% used of',I7,' bytes of text available')

C------       Input summary

   51 CALL DPBLAN (1)
      WRITE (IQPRNT,9051) IQ(LACRAD+12)
 9051 FORMAT (' Read:',6X,I6,' lines from the cradle')

      WRITE (IQPRNT,9052) IQ(LPAM+12), IQ(LPAM+13), IQ(LPAM+14)
 9052 FORMAT (10X,I8,' lines from',I4,' PAM files for',I3,
     F' lines +PAM')

C------       ASM output summary

      WRITE (IQPRNT,9061)
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9061)
 9061 FORMAT (/' Written:')

C----     print all active physical streams

      JASM = 0
   62 JASM = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 71
      LASMT = LQ(LHASM-JASM)
      LASML = LASMT
      JDIV  = 0

   63 IF (IQ(LASML+1).NE.0)        GO TO 69
      LXASM = LQ(LASML-1)
      MODE  = IQ(LASML+2)
      NDK   = IQ(LXASM+3)
      NLI   = IQ(LXASM+4)
      IF (MODE.NE.3)  THEN
          IF (NDK.EQ.0)            GO TO 69
        ENDIF

      LINE = '   physical'
      CALL STRMID (JASM,JDIV,LINE,13)
      JP = MAX (NESLAT,24)
      CALL CSETDI (NDK,  LINE,JP+1,JP+5)
      JP = JP + 6
      IF (MODE.EQ.0)  THEN
          LINE(JP+1:JP+5) = 'decks'
        ELSEIF (MODE.EQ.1)  THEN
          LINE(JP+1:JP+5) = 'units'
        ELSE
          LINE(JP+1:JP+5) = 'files'
        ENDIF
      JP = JP + 6

      IF (MODE.EQ.0)  THEN
          LINE(JP+1:JP+8) = 'bypassed'
          JP = JP + 8
          GO TO 68
        ENDIF

      CALL CSETDI (NLI, LINE,JP+1,JP+6)
      JP = JP + 7
      LINE(JP+1:JP+5) = 'lines'
      JP = JP + 6

      IF (MODE.EQ.1)  THEN
          LINE(JP+1:JP+7) = 'to file'
          JP = JP + 9
          CALL NA_GET (IQ(LASML+3), LINE,JP)
          JP = NESLAT - 1
          GO TO 68
        ENDIF

      IF (MODE.EQ.2)  THEN
          LINE(JP+1:JP+22) = ' written in SPLIT mode'
        ELSE
          LINE(JP+1:JP+22) = 'written in MODIFY mode'
        ENDIF
      JP = JP + 22

   68 WRITE (IQPRNT,9000)
      WRITE (IQPRNT,9000)  LINE(1:JP)
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9000) LINE(1:JP)

      IF (MODE.LT.2)               GO TO 69
      IXU = IQ(LASML+3)
      IF (IXU.NE.0)  THEN
          CALL NA_GET (IXU,LINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9067) LINE(1:N)
        ENDIF

      IXU = IQ(LASML+4)
      IF (IXU.NE.0)  THEN
          CALL NA_GET (IXU,LINE,1)
          N = NDSLAT
          WRITE (IQPRNT,9068) LINE(1:N)
        ENDIF

 9067 FORMAT ('   log file name is  ',A)
 9068 FORMAT ('   file name prefix: ',A)

   69 IF (JDIV.EQ.4)               GO TO 62
      JDIV  = JDIV + 1
      LASML = LQ(LASMT-JDIV-1)
      GO TO 63

C----     print all active logical streams

   71 WRITE (IQPRNT,9000)
      JASM = 0
   72 JASM = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 81
      LASMT = LQ(LHASM-JASM)
      LASML = LASMT
      JDIV  = 0

   73 NDKG = IQ(LASML+8)
      NDKW = IQ(LASML+9)
      IF (NDKG.EQ.0)               GO TO 79
      MUSE = IQ(LASML+1)

      LINE = '    logical'
      CALL STRMID (JASM,JDIV,LINE,13)
      JP = MAX (NESLAT,24)
      CALL CSETDI (NDKG, LINE,JP+1,JP+5)
      JP = JP + 6
      LINE(JP+1:JP+5) = 'decks'
      JP = JP + 6
      IF (NDKW.EQ.NDKG)            GO TO 76

      LINE(JP+1:JP+5) = 'done,'
      JP = JP + 6
      CALL CSETDI (NDKW, LINE,JP+1,JP+5)
      JP = JP + 6
      LINE(JP+1:JP+5) = 'decks'
      JP = JP + 6

   76 LINE(JP+1:JP+7) = 'written'
      JP = JP + 7
      IF (MUSE.EQ.0)               GO TO 78

      LINE(JP+1:JP+4) = ' via'
      JP = JP + 6

      JASMP = MUSE / 8
      JDIVP = MOD (MUSE,8)

      LINE(JP:JP+15) = CH_TYP(JASMP)
      JP = LNBLNK (LINE(1:JP+15)) + 1
      LINE(JP:JP) = ':'
      JP = JP + 1
      CALL CSETDI (JDIVP, LINE,JP,JP)

   78 WRITE (IQPRNT,9000)  LINE(1:JP)

   79 IF (JDIV.EQ.4)               GO TO 72
      JDIV  = JDIV + 1
      LASML = LQ(LASMT-JDIV-1)
      GO TO 73

C----     Print number of warnings and errors

   81 IF (NQERR+NQWARN.EQ.0)  THEN
        WRITE (IQPRNT,9081)
       ELSE
        IF (NQERR.EQ.0)  THEN
            WRITE (IQPRNT,9082) NQWARN
          IF (IQTYPE.NE.IQPRNT) WRITE (IQTYPE,9082) NQWARN
         ELSE
            WRITE (IQPRNT,9082) NQWARN,NQERR
          IF (IQTYPE.NE.IQPRNT) WRITE (IQTYPE,9082) NQWARN,NQERR
         ENDIF
       ENDIF


      IF (NQERR.EQ.0)        RETURN

      CALL EXITRC (2)

 9081 FORMAT (/' No errors, no warnings.'/)
 9082 FORMAT (/'    **!!', I5,' warnings  !!**'/
     F       :,'   ***!!!',I4,' errors   !!!***'/)
      END
