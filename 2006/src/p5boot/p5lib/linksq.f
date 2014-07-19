CDECK  ID>, LINKSQ.
      FUNCTION LINKSQ (LXX,LYY)

C-    if LXX not zero:
C-           connect all KEEP banks of the sequences called by
C-           the call bank at LXX;
C-    if LXX is zero:
C-           finalize only the sequence whose KEEP bank is LYYY;
C-    finalizing all global KEEPs called directly or indirectly,
C-    collect EXE bits and return them as function value
C.    started 28-nov-91

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------
      PARAMETER   (NOFFLI=2, NOFFDA=4)
      PARAMETER   (MAXLEV=36)
      DIMENSION    MSAVE(8,MAXLEV)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      LDO = LXX
      KP  = LEXP - 3
      KD  = 0
      IF (LEXD.NE.0)  KD= LEXD - 3

      MEXL  = 0
      MEXG  = 0
      IFL6  = 0
      LKEEP = 0
      LEVK  = 0
      IF (LDO.NE.0)                GO TO 31
      LKEEP = LYY

C-        MEXi are the cumulated EXE bits of:
C-            MEXL  just all patch/deck directed sequences called
C-            MEXG  all global seq called from the current KEEP
C-            MEXH  all global seq called from the current call bank

C-----        Start new KEEP bank, is it ready for use?

   21 IF (IAND(IQ(LKEEP),KM5).EQ.0)  CALL LINKSU (LKEEP)
      IF (IAND(IQ(LKEEP),KM6).NE.0)   GO TO 72

C----         the KEEP bank is not ready, scan its associated
C--           structure for unsatisfied sequence call banks

      MEXG = IQ(LKEEP)
      IFL6 = KM6
      LDO = LKEEP - 1
   23 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 71
      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.LT.MCCCDE)        GO TO 23
      IF (JCCTYP.GT.MCCXSQ)        GO TO 23

C----         call bank reached, connect all sequences called

   31 MEXH = IQ(LDO)
      IF (IAND(IQ(LDO),KM17).NE.0)    GO TO 48
      NSEQ  = IQ(LDO+NOFFDA)
      IFL17 = KM17
C-        bit 17 : all sequences are definitly connected

C--           next call

      JSEQ = 1
   34 LNEW = LQ(LDO-JSEQ-NOFFLI)
      IF (LNEW.GT.0)               GO TO 47

C--           locate the KEEP bank

      IXSEQ = IQ(LDO+NOFFDA+JSEQ)
      CALL LOCSEQ (LNEW, IXSEQ, KP,KD)
      MEXH = IOR (MEXH,IQUEST(3))
      MEXL = IOR (MEXL,IQUEST(4))
      IF (LNEW.EQ.0)               GO TO 45
      IFLPDD = IQUEST(2)

      IF (JBYT(IQ(LNEW),5,2).NE.3)    GO TO 61

C--           store the adr of the KEEP bank into XSQ

   41 IF (IFLPDD.EQ.0)             GO TO 46
      LNEW  = -LNEW
   45 IFL17 = 0
      IFL6  = 0
   46 LQ(LDO-JSEQ-NOFFLI) = LNEW
   47 JSEQ = JSEQ + 1
      IF (JSEQ.LE.NSEQ)            GO TO 34

      IQ(LDO) = IOR (IQ(LDO), IAND(MEXH,15))
      IQ(LDO) = IOR (IQ(LDO), IFL17)
   48 MEXG = IOR (MEXG,MEXH)
      IF (LEVK.NE.0)               GO TO 23
   49 LINKSQ  = IAND (IOR(MEXL,MEXG), 15)
      RETURN

C------     new unready KEEP, go down one level

   61 IF (IAND(IQ(LNEW),KM7).EQ.0)  GO TO 67
      LEVK = LEVK + 1
      IF (LEVK.GT.MAXLEV)          GO TO 91
      MSAVE (1,LEVK) = LKEEP
      MSAVE (2,LEVK) = MEXG
      MSAVE (3,LEVK) = LDO
      MSAVE (4,LEVK) = MEXH
      MSAVE (5,LEVK) = IFL17
      MSAVE (6,LEVK) = JSEQ
      MSAVE (7,LEVK) = IFLPDD
      MSAVE (8,LEVK) = IFL6
      LKEEP = LNEW
      GO TO 21

C--         sequence definition not yet filled in

   67 IFL17 = 0
      IFL6  = 0
      GO TO 41

C------     end of processing this KEEP, come back up one level

   71 IQ(LKEEP) = IOR (IQ(LKEEP),IAND(MEXG,15))
      IQ(LKEEP) = IOR (IQ(LKEEP),IFL6)
   72 IF (LEVK.EQ.0)               GO TO 49
      LNEW   = LKEEP
      LKEEP  = MSAVE (1,LEVK)
      MEXG   = MSAVE (2,LEVK)
      LDO    = MSAVE (3,LEVK)
      MEXH   = MSAVE (4,LEVK)
      IFL17  = MSAVE (5,LEVK)
      JSEQ   = MSAVE (6,LEVK)
      IFLPDD = MSAVE (7,LEVK)
      IFL6   = MSAVE (8,LEVK)
      LEVK = LEVK -1

      NSEQ = LQ(LDO+NOFFDA)
      IF (IAND(IQ(LNEW),KM6).EQ.0)  IFLPDD= -1

      IF (IFLPDD.EQ.0)  THEN
          MEXH = IOR (MEXH,IQ(LNEW))
        ELSE
          MEXL = IOR (MEXL,IQ(LNEW))
        ENDIF
      GO TO 41

C-----             Sequence call loop

   91 CONTINUE
      CALL P_FATAL ('sequence call loop')
      LINKSQ = 0
      END
