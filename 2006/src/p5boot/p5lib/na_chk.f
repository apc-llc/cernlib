CDECK  ID>, NA_CHK.
      FUNCTION NA_CHK (LINE,JL,JR)

C-    Find the P/D/Z name in LINE(JL:JR-1) and return its relative adr
C-    or -1 if not found
C-    NDSLAT returns the length of the name
C.    started  1-oct-91

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
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
      COMMON /NAMEC/ NA_OCC, NA_CUT, NA_MXSL, NA_MXTX
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*512, NAMEOP*64


      NN = MIN (64, JR-JL)
      IF (NN.NE.0)  THEN
          NAMEOP(1:NN) = LINE(JL:JL+NN-1)
          CALL CLEFT (NAMEOP,1,NN)
          NN  = MIN (NDSLAT, NA_CUT)
        ENDIF

C----              Find the name

      JSL = LQLSTA(5)
      IF (NN.EQ.0)                 GO TO 29
   24 JSL = JSL + 1
      IF (JSL.GT.NA_OCC)           GO TO 91
      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX
      IF (NTX.NE.NN)               GO TO 24
      IF (ICEQU(NAMEOP,TEXT(JTX),NN) .NE.0)   GO TO 24

   29 NA_CHK = JSL - LQLSTA(5)
      NDSLAT = NN
      RETURN

   91 NA_CHK = -1
      NDSLAT = NN
      RETURN
      END
