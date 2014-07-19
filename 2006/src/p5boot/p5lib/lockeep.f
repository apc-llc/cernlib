CDECK  ID>, LOCKEEP.
      SUBROUTINE LOCKEEP (LSEQ)

C-    Find the KEEP bank for the sequence decribed by
C-    the current +KEEP control line;
C-    create target PAT/DECK banks if local sequence.

C-    return   LSEQ       L-adr of the sequence found, or LSEQ=0
C-             IQUEST(1)  K-adr of the seq found, if found
C-                        else: K-adr for attachment at end
C-             IQUEST(2)  zero:  sequence is global
C-                         -ve:  sequence is local
C-             IQUEST(3)  EXE bits of global sequence
C-             IQUEST(4)  EXE bits of sequence
C-             IQUEST(11) adr of P/D to which the sequence would have
C-                        to be attached, if local and not existing
C-                        (to have access to the EXE bits)
C.    started  7-jan-92

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
C--------------    End CDE              --------------------------------


      IXS = MCCPAR(JCCPZ+1)
      KP  = 0
      KD  = 0
      LPD = 0
      IF (NCCPP.EQ.0)              GO TO 24

      IXP = MCCPAR(JCCPP+1)
      IXD = MCCPAR(JCCPD+2)

      CALL CREAPD (IXP, IXD, 0)
      KP  = LCRP - 3
      LPD = LCRP
      IF (LCRD.NE.0)  THEN
          KD  = LCRD - 3
          LPD = LCRD
        ENDIF

   24 CALL LOCSEQ (LSEQ, IXS,KP,KD)
      IQUEST(11) = LPD
      RETURN
      END
