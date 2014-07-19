CDECK  ID>, TIFORG.
      SUBROUTINE TIFORG (LPREV,LNEXT)

C-    Skip over foreign and control material
C.    started 9-may-95

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
C--------------    End CDE              --------------------------------



      LDO = LQ(LPREV-1)
      IF (JCCTYP.LT.MCCKEE)        GO TO 24
      IF (JCCTYP.GE.MCCUSE)        GO TO 24

C--           scan foreign

   21 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 27
      IF (LQ(LDO-2).NE.0)          GO TO 21
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LT.MCCSEL)        GO TO 21
      IF (JCCTYP.GE.MCCUSE)        GO TO 24
      IF (JCCTYP.GE.MCCKEE)        GO TO 21
      GO TO 27

C--           scan control

   24 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 27
      IF (LQ(LDO-2).NE.0)          GO TO 24
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.GE.MCCUSE)        GO TO 24
      IF (JCCTYP.GE.MCCKEE)        GO TO 21
      IF (JCCTYP.EQ.0)             GO TO 27
      IF (JCCTYP.LT.MCCTRU)        GO TO 24

C--       end of scan

   27 LNEXT = LDO
      RETURN
      END
