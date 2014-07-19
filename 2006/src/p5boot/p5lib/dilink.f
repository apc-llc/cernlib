CDECK  ID>, DILINK.
      SUBROUTINE DILINK (LOLD,LNEW,LEVEL,NLEN)

C-    Link NLEN banks, at least one, starting with LOLD/LNEW
C-         LEVEL = 0,1,2  for bank type DECK, PATCH, PAM
C-         if NLEN < 0: do not count the match (for proxy)
C.    started 16-june-94

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


      LX  = LOLD
      LN  = LNEW
      LEV = LEVEL
      NDO = NLEN
      IF ((LEV.GT.2) .OR. (LEV.LT.0))
     +               CALL P_KILL ('LEVEL is faulty in DILINK')

   21 LQ(LX-2) = LN
      LQ(LN-2) = LX
      IQ(LN+5) = IQ(LX+5)
      IF (NDO.GT.0)  NOBJMAT= NOBJMAT + 1
      NDO = NDO - 1
      IF (LEV.NE.0)                GO TO 26
   24 IF (NDO.LE.0)                RETURN
      LX = LQ(LX-1)
      LN = LQ(LN-1)
      GO TO 21

C--           if a PAT bank has been linked, link also it blank deck
C--           (similarly for PAM bank)

   26 LXX = LX
      LNN = LN
      LVX = LEV
   27 LXX = LQ(LXX-4)
      LNN = LQ(LNN-4)

      LQ(LXX-2) = LNN
      LQ(LNN-2) = LXX
      LVX = LVX - 1
      IF (LVX.NE.0)          GO TO 27
      GO TO 24
      END
