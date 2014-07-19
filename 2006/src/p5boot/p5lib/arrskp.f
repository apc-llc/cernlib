CDECK  ID>, ARRSKP.
      SUBROUTINE ARRSKP

C-    Skip one PAM/Patch/Deck according to JDKTYP, using the C library
C.    started 16-mar-92

      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
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


      IXEXDEC = 0
      NSLORG = 0
      NDKSKP = 0
      LEVEL  = JDKTYP

C--       Make sure the current deck is complete in memory

      IF (JDKNEX.EQ.0)  CALL ARRNXD (0)
   24 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C--       Skip one deck, next -> current

      CALL ARRNXD (1)
      NDKSKP = NDKSKP + 1
      IF (JDKTYP.LT.LEVEL)         GO TO 24

C--       The now current deck is at the right level

      IF (LEVEL.GE.3)              RETURN
      IF (LEVEL.EQ.2)  IXEXPAT= 0
      NQDKNO = NQDKNO + NDKSKP
      JSLZER = IQ(LQHOLD+1)
      JSLORG = JSLZER
      RETURN
      END
