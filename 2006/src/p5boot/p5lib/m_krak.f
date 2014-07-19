CDECK  ID>, M_KRAK.
      SUBROUTINE M_KRAK (IFPROC)

C-    Crack control line at LDOAN for M_ANA3 and M_A3IF;
C-          do not/do the IF= evaluation if IFPROC = 0/1
C-    if faulty control found:
C-    change c/c type to FAULTY and print the line
C.    started 29-nov-93

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------


      CALL CCKRAK (IQ(LDOAN+1))
      IF (JCCBAD.NE.0)             GO TO 41
      IF (IFPROC.NE.0)  THEN
          IF (NCCPIF.NE.0)  CALL CCPROC
        ENDIF
      IF   (JCCWAR.EQ.0)           RETURN

C:    DATA CHWA( 1) /'obsured dot in control line'/
C:    DATA CHWA( 2) /'control line ends on comma'/

      IF (JCCWAR.EQ.1)  THEN
          IF (MOPTIO(2).NE.0)      RETURN
          CALL FAILCC (0, 'obsured dot in control line')
        ELSE
          CALL FAILCC (0, 'control line ends on comma')
        ENDIF
      RETURN

C--           faulty c/line

   41 CALL M_FAIL ('Syntax error')
      RETURN
      END
