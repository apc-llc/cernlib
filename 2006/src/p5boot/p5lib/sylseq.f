CDECK  ID>, SYLSEQ.
      SUBROUTINE SYLSEQ (LSEQ, IXSEQ, KP,KD)

C-    Find KEEP bank for the sequence with the name index IXSEQ
C-    KP gives the patch-directed, KD the deck-directed sequences
C-
C-    return   LSEQ       L-adr of the sequence found, or LSEQ = 0
C-             IQUEST(1)  K-adr of the seq found, if found
C-                          else: K-adr for attachment at end
C-
C-    if the sequence has been found,
C-    it is re-linked to the start of its structure
C.    started 29-aug-92

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
C--------------    End CDE              --------------------------------
      EQUIVALENCE (KSEQ,IQUEST(1))
      PARAMETER   (LOCID=4)


C--         Global set

      IF (KP.NE.0)                 GO TO 23
      KU = KQKEEP
      GO TO 29

C--         Patch-directed set

   23 IF (KD.NE.0)                 GO TO 24
      KU = KP
      GO TO 29

C--         Deck-directed set

   24 KU = KD

   29 L = KQFIND (IXSEQ,LOCID,KU,KSEQ)
      IF (L.NE.0)      THEN
      IF (KSEQ.NE.KU)  THEN
          LQ(KSEQ) = LQ(L-1)
          LQ(L-1)  = LQ(KU)
          LQ(KU)   = L
          KSEQ     = KU
        ENDIF
        ENDIF

      LSEQ = L
      RETURN
      END
