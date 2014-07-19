CDECK  ID>, DIPREM.
      SUBROUTINE DIPREM

C-    Pre-match on the basis of patch/deck names only
C.    started 3-june-94

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------


      CALL DPBLAN (1)
      WRITE (IQPRNT,9001) 'old',NSLTOT(1),NOBJTT(1,1),NOBJTT(2,1)
      WRITE (IQPRNT,9001) 'new',NSLTOT(2),NOBJTT(1,2),NOBJTT(2,2)
      CALL DPBLAN (0)

 9001 FORMAT (' file "',A,'" has',I6,' lines in',I5,' decks, in'
     F,I4,' patches')

      CALL DILINK (LQUSER(1), LQUSER(2), 2,1)

C----         match patch names

      LFOLD = LQUSER(1)
      LFNEW = LQUSER(2)
      NPMAT = 0

      CALL DIMNAME (LFOLD,LFNEW,1,1,IRC)
      NPMAT = NPMAT + IQUEST(1)

C--           each patch: match the deck names

      LOLD = LQ(LFOLD-4)
   36 LNEW = LQ(LOLD-2)
      IF (LNEW.NE.0)     CALL DIMNAME (LOLD, LNEW, 0, 1, IRC)

   37 LOLD = LQ(LOLD-1)
      IF (LOLD.NE.0)               GO TO 36

C--           match unmatched file titles by position

      IF (NOBJTT(3,1).NE.NOBJTT(3,2))    GO TO 49
      LFX = LQUSER(1)
      LFN = LQUSER(2)
   44 LFX = LQ(LFX-3)
      LFN = LQ(LFN-3)
      IF (LFN.EQ.0)                GO TO 49
      LPX = LQ(LFX-4)
      LPN = LQ(LFN-4)
      IF (LQ(LPX-2).NE.0)    GO TO 44
      IF (LQ(LPN-2).NE.0)    GO TO 44
      CALL DILINK (LPX,LPN,1,1)
      GO TO 44

   49 IF (NOBJTT(1,1).NE.NOBJTT(1,2))  IFSTRUC= 7
      IF (NOBJTT(1,1).NE.NOBJMAT)      IFSTRUC= 7
C--           check that 1/2 of the patches actually match

      IF (MOPTIO(1).NE.0)          RETURN

      NDH = MIN (NOBJTT(1,1), NOBJTT(1,2))
      NPH = MIN (NOBJTT(2,1), NOBJTT(2,2))
      IF (2*NOBJMAT.GE. NDH)       RETURN
      IF (2*NPMAT  .GE. NPH)       RETURN
      IF (NPH.LE.6)                RETURN
      CALL P_KILL
     +      ('Insufficient number of matching patch-names initially')
      RETURN
      END
