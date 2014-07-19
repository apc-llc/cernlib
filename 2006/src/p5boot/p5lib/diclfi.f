CDECK  ID>, DICLFI.
      SUBROUTINE DICLFI (LFI)

C-    Reset all banks of all PAMs as out-of-memory
C.    started 3-june-94

      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------



      LF = LFI
      LP = LQ(LF-4)
   22 IF (LP.EQ.0)           RETURN
      IF (IQ(LP+1).EQ.0)     GO TO 26

      IQ(LP+1) = 0
      LD = LQ(LP-4)
   24 IF (LD.EQ.0)           GO TO 26
      IQ(LD+1) = 0
      LD = LQ(LD-1)
      GO TO 24

   26 LP = LQ(LP-1)
      GO TO 22
      END
