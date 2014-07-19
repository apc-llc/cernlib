CDECK  ID>, SETGAP.
      SUBROUTINE SETGAP

C-    Ready the gap for the next deck
C.    started 19-july-95

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------


C--            Check enough space

      NWFREE = LQCSTA(3) - LQCEND(1)
      IF (NWFREE.LE.NVGAP(1))      GO TO 24

      JSLOCC = LQLSTA(4)
      JSLFRE = LQLEND(2)
      NSLFRE = JSLOCC - JSLFRE
      NTXFRE = MLIAD(JSLOCC) - MLIAD(JSLFRE)

      NTXWAN = NVGAP(2) * NVGAP(4)
      IF (NSLFRE.LT.NVGAP(2))      GO TO 24
      IF (NTXFRE.GE.NTXWAN)        GO TO 29

   24 IF (IFLGAR.EQ.0)  CALL MQSHIFT
      IFLGAR = 7
      NWFREE = LQCSTA(3) - LQCEND(1)

C--       set limits of control division 2

   29 LQCSTA(2) = LQCEND(1) + 5*(NWFREE/8)
      LQCEND(2) = LQCSTA(2)

C--       clear text division 3

      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      RETURN
      END
