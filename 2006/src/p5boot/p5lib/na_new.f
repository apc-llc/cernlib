CDECK  ID>, NA_NEW.
      FUNCTION NA_NEW (LINE,JL,JR)

C-    Register the P/D/Z name in LINE(JL:JR-1) and return its adr
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


      NN  = MIN (64, JR-JL)
      IF (NN.LE.0)                 GO TO 49

      NAMEOP(1:NN) = LINE(JL:JL+NN-1)
      CALL CLEFT (NAMEOP,1,NN)
      NN = MIN (NDSLAT, NA_CUT)
      IF (NN.EQ.0)                 GO TO 49


C----              Find the name if it exists already

      JSL = LQLSTA(5) + 1
   24 JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX
      IF (NTX.EQ.NN)  THEN
          IF (ICEQU(NAMEOP,TEXT(JTX),NN) .EQ.0)   GO TO 29
        ENDIF

      JSL = JSL + 1
      IF (JSL.LE.NA_OCC)           GO TO 24

C----              Name does not exist, enter it

      IF (JSL+1 .GE.LQLEND(5))     GO TO 81
      JTX = MLIAD(JSL)
      IF (JTX+NN+8.GE.NSIZETX)     GO TO 81

      CALL CCOPYL (NAMEOP,TEXT(JTX),NN)
      MLIAD(JSL+1) = JTX + NN
      NA_OCC = JSL
   29 NDSLAT = NN
      NA_NEW = JSL - LQLSTA(5)
      RETURN

C--           Zero-length string

   49 NDSLAT = 0
      NA_NEW = 0
      RETURN

C----              Space full

   81 CALL NA_FUL
      NA_NEW = 0
      END
