CDECK  ID>, TICOMM.
      SUBROUTINE TICOMM

C-    Delete leading and trailing comment or blank lines
C-    in Fortran decks only
C.    started 26-may-94

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
     +,       IXBLADK, IXSQSP(7), NTRBLA,NMODIF, JSLTTL,   LASTTI
C--------------    End CDE              --------------------------------



C----         delete leading comments

      LDO = LQPREP
   21 LPR = LDO
      LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                RETURN
      IF (LQ(LDO-2).NE.0)          GO TO 21
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.EQ.0)             GO TO 24
      IF (JCCTYP.LT.MCCTRU)        GO TO 22
      IF (JCCTYP.LT.MCCKEE)        GO TO 31

C--       step over foreign and control material

   22 CALL TIFORG (LPR,LDO)
      IF (LDO.EQ.0)                RETURN
      IF (JCCTYP.NE.0)             GO TO 31

C--       check all comment

   24 JSLA = IQ(LDO+1)
      NSL  = IQ(LDO+2)
      JSLE = JSLA + NSL

      JSL = JSLA - 1
   25 JSL = JSL + 1
      IF (JSL.GE.JSLE)             GO TO 27

      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      IF (NTX.EQ.0)                GO TO 25
      IF (TEXT(JTX).EQ.'C')        GO TO 25
      IF (TEXT(JTX).EQ.'c')        GO TO 25
      IF (TEXT(JTX).EQ.'*')        GO TO 25

      IF (JSL.EQ.JSLA)             GO TO 31
      LPR = LDO
      LDO = M_SPLIT (LPR,JSL)
      LQ(LPR-2) = -1
      GO TO 31

   27 LQ(LDO-2) = -1
      GO TO 21

C-----        set the reverse link on remaining banks

   31 CONTINUE
   34 LPR = LDO
   35 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 39
      IF (LQ(LDO-2).NE.0)          GO TO 35
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.EQ.MCCNIL)        GO TO 36
      IF (JCCTYP.EQ.MCCKIL)        GO TO 36
      IF (JCCTYP.LT.MCCKEE)        GO TO 37

C--       step over foreign and control material

   36 CALL TIFORG (LPR,LDO)
      IF (LDO.EQ.0)                GO TO 39

   37 LQ(LDO-3) = LPR
      GO TO 34

   39 LLAST = LPR

C----         look for trailing comment lines

      LDO = LLAST

   42 JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.NE.0)             GO TO 89

      JSLA = IQ(LDO+1)
      NSL  = IQ(LDO+2)
      JSLE = JSLA + NSL

      JSL = JSLE
   44 JSL = JSL - 1
      IF (JSL.LT.JSLA)             GO TO 47

      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      IF (NTX.EQ.0)                GO TO 44
      IF (TEXT(JTX).EQ.'C')        GO TO 44
      IF (TEXT(JTX).EQ.'c')        GO TO 44
      IF (TEXT(JTX).EQ.'*')        GO TO 44

      JSL = JSL + 1
      IF (JSL.EQ.JSLE)             GO TO 89

      LPR = LDO
      LDO = M_SPLIT (LPR,JSL)
      LQ(LDO-2) = -1
      GO TO 89

C--       all lines are comments

   47 LQ(LDO-2) = -1
      LDO = LQ(LDO-3)
      IF (LDO.NE.0)                GO TO 42

C----         done

   89 RETURN
      END
