CDECK  ID>, NA_DIF.
      FUNCTION NA_DIF (JNA,JNB)

C-    Compare NAMEA at JNA with NAMEB at JNB,
C-    return zero if NAMEA = NAMEB
C-             -1 if NAMEA < NAMEB
C-             +1 if NAMEA > NAMEB
C.    started 12-feb-92

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


      IF (JNA.EQ.JNB)  THEN
          NA_DIF = 0
          RETURN
        ENDIF

      JSLA = JNA + LQLSTA(5)
      JSLB = JNB + LQLSTA(5)

      JTXA = MLIAD(JSLA)
      JTXB = MLIAD(JSLB)

      NTXA = MLIAD(JSLA+1) - JTXA
      NTXB = MLIAD(JSLB+1) - JTXB

      NA_DIF = NA_XCO (TEXT(JTXA),TEXT(JTXB),NTXA,NTXB)
      RETURN
      END
