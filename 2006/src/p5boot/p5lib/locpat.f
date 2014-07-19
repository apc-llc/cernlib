CDECK  ID>, LOCPAT.
      FUNCTION LOCPAT (IXPAT)

C-    Locate the PAT bank for patch IXPAT

C-    return the adr of the bank found as function value,
C-    or zero if not found

C-    if the bank has been found in the 'future' chain
C-    relink it to be just after LEXP

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


C--           look in the 'future' chain

      LQ(LLPAST-1) = 0
      LF = KQFIND (IXPAT,2,KQMAIN,KP)
      IF (LF.NE.0)  THEN
          IF (LF.EQ.LEXP)          GO TO 27
          CALL QSHUNT (KP,LEXP-1)
          GO TO 27
        ENDIF

C--           look in the 'past' chain

      LF = LQFIND (IXPAT,2,LQPAST)

   27 LOCPAT = LF
      RETURN
      END
