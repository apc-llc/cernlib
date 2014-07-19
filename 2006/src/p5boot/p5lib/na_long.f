CDECK  ID>, NA_LONG.
      FUNCTION NA_LONG (NAMEOP)

C-    Register long string in NAMEOP as is and return its adr
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
      CHARACTER    NAMEOP*(*)


C----              Find the name if it exists already

      NN  = LNBLNK(NAMEOP)
      JSL = LQLSTA(5)
      IF (NN.EQ.0)                 GO TO 999

   24 JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX
      IF (NTX.EQ.NN)  THEN
          IF (ICEQU(NAMEOP,TEXT(JTX),NN) .EQ.0)   GO TO 999
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
  999 NDSLAT = NN
      NA_LONG = JSL - LQLSTA(5)
      RETURN

C----              Space full

   81 CALL NA_FUL
      NA_LONG = 0
      END
