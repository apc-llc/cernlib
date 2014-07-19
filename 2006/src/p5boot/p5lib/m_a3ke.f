CDECK  ID>, M_A3KE.
      SUBROUTINE M_A3KE

C-    Content analysis stage 3:
C-    scan the material of the +KEEP being defined
C-    return JCCBAD non-zero to signal c/line with syntax error at LDOAN
C.    started 8-dec-93

      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
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


      MAXLEV = 36
      LEVIF  = 0

   22 CALL M_A3NX
      IF (LDOAN.EQ.0)              GO TO 49
      IF (JCCTYP.GE.MCCSEL)        GO TO 49
      IF (JCCTYP.LT.MCCCDE)        GO TO 22
      IF (JCCTYP.EQ.MCCXSQ)        GO TO 22

      CALL M_KRAK (0)
      IF (JCCBAD.NE.0)             RETURN
      IF (JCCTYP.LT.MCCTRU)        GO TO 22

C--           new control

      CALL CCIFLV

      IF (JCCTYP.EQ.MCCELS)        GO TO 26
      IF (JCCTYP.EQ.MCCEND)        GO TO 28

C--           handle +__IF

      LEVIF = LEVIF + 1
      IF (LEVIF.GE.2)              GO TO 24
      INCLEV = LEVIF - JCCLEV
      IF (INCLEV.EQ.LEVAN)         GO TO 22
      IF (LEVAN.EQ.7)  THEN
          LEVAN = INCLEV
        ELSE
          CALL M_WNIF (LEVAN-INCLEV)
        ENDIF
      GO TO 22

   24 N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      IF (LEVIF.GE.MAXLEV)         GO TO 91
      GO TO 22

C--       handle  +_ELSE, IF=xxx

   26 IF (LEVIF.EQ.0)              GO TO 92
      N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      GO TO 22

C--       handle  +_ENDIF

   28 IF (LEVIF.EQ.0)              GO TO 93
      N = JCCLEV+INCLEV - LEVIF
      IF (N.NE.0)  CALL M_WNIF (N)
      LEVIF = LEVIF - 1
      GO TO 22

C----         Done

   49 IF (LEVIF.EQ.0)              RETURN

C-----        Trouble

      CALL FAILCC (1, 'Non-terminated +_IF')
      RETURN

   93 CALL FAILCC (0, 'Redundant +_ENDIF')
      GO TO 22

   92 CALL M_FAIL ('Unmatched +_ELSE')
      RETURN

   91 CALL M_FAIL ('Maximum +_IF nesting level exceeded')
      RETURN
      END
