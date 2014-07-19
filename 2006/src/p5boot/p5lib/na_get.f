CDECK  ID>, NA_GET.
      SUBROUTINE NA_GET (JNA,LINE,JCL)

C-    Transfer the text of name JNA into LINE at col. JCL
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
      CHARACTER   LINE*(*)


      JSL = JNA + LQLSTA(5)
      IF (JNA.LT.0 .OR. JSL.GT.NA_OCC)
     +             CALL P_CRASH ('name-index out of range')
      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX
      N   = MIN (NTX, LEN(LINE)+1-JCL)

      CALL CCOPYL (TEXT(JTX),LINE(JCL:),N)

      NDSLAT = N
      NESLAT = JCL + N
      NFSLAT = NTX
      RETURN
      END
