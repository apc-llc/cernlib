CDECK  ID>, FAILLN.
      SUBROUTINE FAILLN (JSEVER,LORGP,NUMORG,JSLFAU,MSG)

C-    Print faulty control-line, where
C-        JSEVER  0 warning,  1 error,  2 fatal
C-          LORG  adr of the origin deck, if zero: current
C-        NUMORG  Patchy line-number
C-        JSLFAU  slot number of the line
C-           MSG  text of the message to be printed

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)
      CHARACTER    LINE*80, TAIL*64
      EQUIVALENCE (LINE, SLLINE), (TAIL, SLLINE(51:))



      LORG = LORGP
      NUM  = NUMORG
      JSL  = JSLFAU

      CALL FAILMSG (JSEVER,3, 0,MSG)
      IF (NDSLAT.EQ.0)             RETURN

C--       foreign material

      IF (LORG.EQ.0)               GO TO 41
      LINE = ' !!'
      NDK  = JBYT (IQ(LORG),1,16)
      CALL CSETDI (NDK,TAIL,1,12)
      CALL CLEFT  (TAIL,4,12)
      J = NESLAT

      TAIL(J:J) = '='
      J = J + 1
      CALL NA_GET (IQ(LORG+1),TAIL,J)
      J = NESLAT

      TAIL(J:J) = '.'
      J = J + 1
      CALL NA_GET (IQ(LORG+2),TAIL,J)
      NN  = NESLAT - 1
      NOV = NN - 30
      IF (NOV.GT.0)  THEN
          CALL CCOPYL (TAIL,LINE(51-NOV:),NN)
          NN = NN - NOV
        ENDIF
      NN = NN + 50
      WRITE (IQPRNT,9036) LINE(1:NN)
      NQUSED = NQUSED + 1

   41 NQUSED = NQUSED + 2
      IF (JSL.EQ.0)                GO TO 44
      JTX  = MLIAD(JSL)
      NTX  = MLIAD(JSL+1) - JTX - NCHNEWL
      NTX  = MIN (80, NTX)
      CALL CCOPYL (TEXT(JTX),LINE,NTX)
      WRITE (IQPRNT,9041) NUM,LINE(1:NTX)
      GO TO 49

   44 WRITE (IQPRNT,9044)
   49 IF (JSEVER.GE.2)  CALL P_FATAL ('this')
      NQLLBL = 1
      RETURN

 9036 FORMAT (A)
 9041 FORMAT (1X,I5,'  ! ',A/)
 9044 FORMAT (8X,'!  at end-of-deck'/)
      END
