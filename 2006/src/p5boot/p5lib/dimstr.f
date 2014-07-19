CDECK  ID>, DIMSTR.
      SUBROUTINE DIMSTR (LOLDF,LNEWF,LEVEL,IFLINK,IST)

C-    Find first match of bank at LOLDF in lin/st at LNEWF
C-        LEVEL = 0,1,2  for bank type DECK, PATCH, PAM
C-       IFLINK = 0/1  do not/do link the matching banks
C-    return IST = 0  exact match
C-                 1  all remain OLD banks have matched
C-                 2  more OLD banks to be tried
C-        IQUEST(1) = number of consecutive matching banks
C.    started 3-june-94

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


      LOLD  = LOLDF
      LNEW  = LNEWF
      IXNM  = IQ(LOLD+4)

C--       find the start of match

      NLEN  = 0
      ISTAT = 0
      IF (LQ(LNEW-2).EQ.0 .AND. IQ(LNEW+4).EQ.IXNM)   GO TO 31

      ISTAT = 2
   24 LNEW  = LQ(LNEW-1)
      IF (LNEW.EQ.0)               GO TO 49
      IF (IQ(LNEW+4).NE.IXNM)      GO TO 24
      IF (LQ(LNEW-2).NE.0)         GO TO 24

C--       find the length of the match

   31 LOLDM = LOLD
      LNEWM = LNEW
      NLEN  = 1

   32 LOLD = LQ(LOLD-1)
      LNEW = LQ(LNEW-1)
      IF (LOLD.EQ.0)               GO TO 41
      IF (LNEW.EQ.0)               GO TO 42

      IF (IQ(LNEW+4).NE.IQ(LOLD+4))   GO TO 42
      IF (LQ(LNEW-2).NE.0)            GO TO 42
      NLEN = NLEN + 1
      GO TO 32

C--       store the match

   41 ISTAT = MIN(1,ISTAT)
      IF (LNEW.NE.0)  ISTAT= 1
      GO TO 43

   42 ISTAT = 2
   43 IF (IFLINK.EQ.0)             GO TO 49

      CALL DILINK (LOLDM,LNEWM,LEVEL,NLEN)

   49 IQUEST(1) = NLEN
      IST = ISTAT
      RETURN
      END
