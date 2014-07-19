CDECK  ID>, DQBANK.
      SUBROUTINE DQBANK

C-    Dump the bank whose parameters are in /MQCN/

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
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER      (IQDROP=31, IQMARK=30, KMDROP=1073741824)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
C--------------    End CDE              --------------------------------
      INTEGER      INTID
      CHARACTER    CHID*4, WK*2, CHK*1
      EQUIVALENCE (CHID,INTID)

      CHARACTER    LINE*48, LINEZ*30, COLZ(30)*1, LINEWH*34
      EQUIVALENCE (LINE,SLLINE), (LINEZ,SLLINE(81:)), (COLZ,LINEZ)
     +,           (LINEWH,SLLINE(161:))

      IAND (IZV,IZW) = AND (IZV, IZW)


      LNA = IQLNA
      LBK = IQLST

      INTID = MMBANK(1,IQTY)

      IF (IAND(IQ(LNA),KMDROP).NE.0)  THEN
          CHK = '_'
        ELSE
          CHK = ' '
        ENDIF

      IF (IQNL.GT.0)  THEN
          NLP = MIN (IQNL,6)
          CALL UCOPIV (LQ(LBK-NLP), IQUEST, NLP)
          WRITE (IQPRNT,9022) LBK,CHK,CHID,IQLI,IQNL,IQNS,IQND,
     +                        (IQUEST(J),J=1,NLP)
        ELSE
          WRITE (IQPRNT,9022) LBK,CHK,CHID,IQLI,IQNL,IQNS,IQND
        ENDIF

 9022 FORMAT (/1X,I6,A,A,' LI/NL/NS/ND= ',I1,2I3,I5,:,
     F', links:',6I7)

C--           Print the object identifier

      SLLINE = ' '

      IF       (IQTY.EQ.JBKPAT)  THEN
          WK = 'P='
          IX = IQ(LBK+2)
        ELSEIF (IQTY.EQ.JBKDEC)  THEN
          WK = 'D='
          IX = IQ(LBK+1)
        ELSEIF (IQTY.EQ.JBKKEE)  THEN
          WK = 'Z='
          IX = IQ(LBK+4)
        ELSE
          GO TO 30
        ENDIF

      LINEWH(1:2) = WK
      CALL NA_GET (IX, LINEWH, 3)

      N = LENOCC (LINEWH)
      WRITE (IQPRNT,9029) LINEWH(1:N)
 9029 FORMAT (13X,A)

C----         Dump status word

   30 IF (IQTY.EQ.JBKPRE)          GO TO 31
      IF (IQTY.EQ.JBKXSQ)          GO TO 31
      IF (IQTY.EQ.JBKMAT)          GO TO 31
      IF (IQTY.EQ.JBKKEE)          GO TO 31
      IF (IQTY.NE.JBKACT)          GO TO 61

   31 MSTAT = IQ(LBK)
      CALL UPKBYT (MSTAT,1,IQUEST,30,0)

      DO 32 J=1,30
      IF (IQUEST(31-J).EQ.0)  THEN
          LINEZ(J:J) = '.'
        ELSE
          LINEZ(J:J) = '1'
        ENDIF
   32 CONTINUE
      IF (COLZ(27).NE.'.')  COLZ(27) = 'X'
      IF (COLZ(28).NE.'.')  COLZ(28) = 'D'
      IF (COLZ(29).NE.'.')  COLZ(29) = 'L'
      IF (COLZ(30).NE.'.')  COLZ(30) = 'E'
      IF (IQTY.EQ.JBKKEE)          GO TO 33

      IF (COLZ(25).NE.'.')  COLZ(25) = 'K'
      IF (COLZ(26).NE.'.')  COLZ(26) = 'D'
      IF (IQTY.EQ.JBKACT)          GO TO 34

C--           PREP & XSQ & MAT

      LINE(3:8) = LINEZ(11:16)

      JC = JBYT(MSTAT,9,6)
      CALL CSETDI (JC,LINE,9,11)
      GO TO 36

C--           KEEP & ACT

   33 IF (COLZ(25).NE.'.')  COLZ(25) = 'A'
      IF (COLZ(26).NE.'.')  COLZ(26) = 'U'
   34 LINE(1:9) = LINEZ(11:19)

      JC = JBYT(MSTAT,9,3)
      CALL CSETDI (JC,LINE,10,11)

   36 LINE(13:14) = LINEZ(23:24)
      LINE(16:17) = LINEZ(25:26)
      LINE(19:22) = LINEZ(27:30)

   37 WRITE (IQPRNT,9037) LINE(1:22)
 9037 FORMAT (13X,'st: ',A)

C----         Print line content

   61 IF (IQLI.EQ.0)               GO TO 77

      JSLA = IQ(LBK+1)
      NSL  = IQ(LBK+2)

      WRITE (IQPRNT,9062) NSL,JSLA
 9062 FORMAT (13X,'with',I6,' lines at JSLA',I7)

      IF (JSLA.EQ.0)               GO TO 77
      IF (JSLA.EQ.1)               GO TO 77
      IF (NSL.EQ.0)                GO TO 77
      IF (JSLA.LT.0)               GO TO 93
      IF (JSLA.GE.LQLSTA(5))       GO TO 93

C--           print first line in lot

      JSLU = JSLA
      JTXA = MLIAD(JSLU)
      JTXE = MLIAD(JSLU+1)
      NTX  = JTXE - JTXA - NCHNEWL

      IF (NTX.LT.0)                GO TO 94
      IF (NTX.GE.513)              GO TO 94
      N = MIN (NTX,60)

      CALL LN_GET (JSLU,SLLINE,N)
      WRITE (IQPRNT,9063) SLLINE(1:N)

 9063 FORMAT (17X,'first: ',A)
 9064 FORMAT (17X,' last: ',A)

C--           print last line in lot

      IF (NSL.LE.1)                GO TO 77
      JSLU = JSLA + NSL - 1
      JTXA = MLIAD(JSLU)
      JTXE = MLIAD(JSLU+1)
      NTX  = JTXE - JTXA - NCHNEWL

      IF (NTX.LT.0)                GO TO 94
      IF (NTX.GE.513)              GO TO 94
      N = MIN (NTX,60)

      CALL LN_GET (JSLU,SLLINE,N)
      WRITE (IQPRNT,9064) SLLINE(1:N)

   77 RETURN

C----         trouble

   93 WRITE (IQPRNT,9093)
 9093 FORMAT (/' ***!!! JSL invalid !!!***')
      GO TO 97

   94 WRITE (IQPRNT,9094) JSLU,JTXA,JTXE
 9094 FORMAT (/' ***!!! trouble with line at slot',I6
     F/5X,'JTXA =',Z10/5X,'JTXE =',Z10)

   97 IQFOUL = -1
      RETURN
      END
