CDECK  ID>, LN_GET.
      SUBROUTINE LN_GET (JSL,LINE,NCH)

C-    Transfer the line at JSL from the store to local,
C-    NCH characters maximum,
C-    return NDSLAT # of chars. transferred,
C-           NESLAT # of chars. in store

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ),  MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      CHARACTER*512  LINE

      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      NESLAT = NTX

      NTX = MIN (NCH,NTX)
      NDSLAT = NTX

      CALL CCOPYL (TEXT(JTX),LINE,NTX)
      RETURN
      END
