CDECK  ID>, LINKSU.
      SUBROUTINE LINKSU (LKEEPX)

C-    Initial processing of the sequence kept at LKEEP:
C-    evaluate all IF conditions, delink all deselected material
C.    started 14-july-93

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      LKEEP  = LKEEPX
      LEVIF  = 0
      MEXCUM = 0
      IFLKM6 = KM6
      KDO    = LKEEP - 2
      GO TO 23

C------            accepted material

   21 IFLKM6 = 0
   22 KDO = LDO - 1
   23 LDO = LQ(KDO)
      IF (LDO.EQ.0)                GO TO 71
      MEXCUM = IOR (MEXCUM,IQ(LDO))
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LE.MCCNIL)        GO TO 22
      IF (JCCTYP.GE.MCCTRU)        GO TO 25

C--           +CDE +SEQ:  check IF condition is true

      IF (JCCTYP.EQ.MCCXSQ)        GO TO 21
      CALL CCKRAK (IQ(LDO+1))
      CALL CCPROC
      MEXCUM = IOR (MEXCUM,MXCCIF)
      IF (JCCIFV.EQ.0)             GO TO 21
   24 CALL TOGARB (KDO,0)
      GO TO 23

C--           new control

   25 IF (JCCTYP.EQ.MCCELS)        GO TO 26
      IF (JCCTYP.EQ.MCCEND)        GO TO 28

C--           handle +__IF

      CALL CCKRAK (IQ(LDO+1))
      CALL CCPROC
      MEXCUM = IOR (MEXCUM,MXCCIF)
      LEVIF  = LEVIF + 1
      IF (JCCTYP.EQ.MCCFAL)  JCCIFV= MAX(0, 1-JCCIFV)
      IF (JCCIFV.EQ.0)             GO TO 29
      LVREND = LEVIF
      LVRELS = LEVIF + 1
      GO TO 41

C--       handle  +_ELSE, IF=xxx

   26 LVREND = LEVIF
      LVRELS = LEVIF
      GO TO 41

C--       handle  +_ENDIF

   28 LEVIF = LEVIF - 1
   29 IF (IQ(LDO+2).LT.2)          GO TO 24
      IQ(LDO+1) = IQ(LDO+1) + 1
      IQ(LDO+2) = IQ(LDO+2) - 1
      IQ(LDO+3) = IQ(LDO+3) + 1
      CALL SBYT (0, IQ(LDO),9,6)
      GO TO 22

C------            rejected material

   41 CALL TOGARB (KDO,0)
      LDO = LQ(KDO)
      IF (LDO.EQ.0)                GO TO 71
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LT.MCCTRU)        GO TO 41

C--           new control

      IF (JCCTYP.EQ.MCCELS)        GO TO 46
      IF (JCCTYP.EQ.MCCEND)        GO TO 48

C--           handle +__IF

      LEVIF = LEVIF + 1
      GO TO 41

C--       handle  +_ELSE, IF=xxx

   46 IF (LEVIF.GE.LVRELS)         GO TO 41
      CALL CCKRAK (IQ(LDO+1))
      CALL CCPROC
      MEXCUM = IOR (MEXCUM,MXCCIF)
      IF (JCCIFV.NE.0)             GO TO 41
      GO TO 29

C--       handle  +_ENDIF

   48 LEVIF = LEVIF - 1
      IF (LEVIF.GE.LVREND)         GO TO 41
      GO TO 29

C------            done

   71 MEXCUM = IAND (MEXCUM,15)
      MEXCUM = MEXCUM + KM5 + IFLKM6
      IQ(LKEEP) = IOR (IQ(LKEEP),MEXCUM)
      RETURN
      END
