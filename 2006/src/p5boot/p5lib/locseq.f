CDECK  ID>, LOCSEQ.
      SUBROUTINE LOCSEQ (LSEQ, IXSEQ,KP,KD)

C-    Find KEEP bank for the sequence with the name index IXSEQ
C-    KP gives the patch-directed, KD the deck-directed sequences

C-    return   LSEQ       L-adr of the sequence found, or LSEQ=0
C-             IQUEST(1)  K-adr of the seq found, if found
C-                           else: K-adr for attachment at end
C-             IQUEST(2)  zero:  sequence is global
C-                         -ve:  sequence is local
C-             IQUEST(3)  EXE bits of global sequence
C-             IQUEST(4)  EXE bits of sequence

C-    if the sequence has been found as global, it is re-linked
C-    to the start of the global structure

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
      EQUIVALENCE (KSQLOC,IQUEST(1)), (L,IQUEST(12))
      DIMENSION    IXSEQ(9)


      IQUEST(11) = IXSEQ(1)
      IQUEST(2)  = 0
      IQUEST(3)  = 0
      IQUEST(4)  = 0

C--         Global set

      CALL LOCSQ2 (KQKEEP)
      IQUEST(3) = IQUEST(4)
      IF (L.EQ.0)                  GO TO 21
      LQ(KSQLOC) = LQ(L-1)
      LQ(L-1) = LQKEEP
      LQKEEP = L
      KSQLOC = KQKEEP
   19 LSEQ = L
      RETURN

C--         Patch-directed set: if KP not zero

   21 IF (KP.EQ.0)                 GO TO 19
      IQUEST(2) = -1
      CALL LOCSQ2 (KP)
      IF (L.NE.0)                  GO TO 19

C--         Deck-directed set: if KD not zero

      IF (KD.EQ.0)                 GO TO 19
      CALL LOCSQ2 (KD)
      GO TO 19
      END
