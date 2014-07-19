CDECK  ID>, M_ANA2.
      SUBROUTINE M_ANA2

C-    Content analysis stage 2:
C-    1.1) for each action insert a zero-line PREP bank into the chain
C-         of PREP banks with a reference to the ACT bank acting into
C-         the current deck;
C-    1.2) split the target PREP bank containing the end of
C-         a DELETE or REPLACE range;
C-    2.1) transfer the action EXE bits into the new PREP bank;
C-    2.2) for the PREP banks covered by a DEL or REPL transfer the
C-         EXE bits of the action, and mark as deleted (unless nil)
C-    3) add one zero-line PREP bank for each trailing action;
C-    -  called also if PAM update mode  -
C.    started 11-nov-91

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      PARAMETER  (MCCNIL=1,  MCCKIL=2,  MCCINC=3,  MCCCDE=4,  MCCSEQ=5,
     + MCCXSQ=6,  MCCTRU=7,  MCCFAL=8,  MCCELS=9,  MCCEND=10,
     +            MCCSEL=11, MCCSES=12, MCCFAU=13, MCCSKI=14,
     +            MCCKEE=15, MCCDEL=16, MCCREP=17, MCCADB=18, MCCADD=19,
     + MCCUSE=20, MCCXDI=21, MCCDIV=22, MCCLIS=23, MCCEXE=24, MCCIMI=25,
     + MCCASM=26, MCCUPD=27, MCCNAM=28, MCCGAP=29, MCCMOR=30, MCCONL=31,
     + MCCFOR=32, MCCSUS=33, MCCOPT=34, MCCOP2=35, MCCSHO=36, MCCPAM=37,
     + MCCQUI=38, MCCEOD=39, MCCDEC=40, MCCPAT=41, MCCTIT=42)
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

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)


C----------        Split PREP banks on all action limits  -----------

      LDO  = LQPREP
      LACT = LEXD - 1

C----              Next action

   21 LACT = LQ(LACT-1)
      IF (LACT.EQ.0)               GO TO 40

C-                    JACT = 0 DEL, 1 REP, 2 ADB, 3 ADD

      JACT = JBYT (IQ(LACT),9,3)
      JSLX = IQ(LACT+4) + JSLZER

C-        break at the insertion point

      LDO = M_SPLIT (LDO,JSLX)
      IF (LDO.EQ.0)                GO TO 40

C-        zero-line PREP bank with reference to ACT

      LX  = LDO
      LDO = M_SPLIT (LDO,0)
      LQ(LX-2) = LACT
      IF (JACT.GE.2)               GO TO 21

C----              +DEL, +REPL:  split at end of delete

      JSLD = IQ(LACT+5) + JSLZER + 1
      LNX  = M_SPLIT (LDO,JSLD)
      GO TO 21

C----------        Set EXE bits and mark deleted material  ----------

   40 KMDEL = KM6 + KM7
      IF (IFLDISP.NE.0)  KMDEL= KM5 + KM7

      LDO  = LQPREP
      LACT = LEXD - 1

C----              Next action

   41 LACT = LQ(LACT-1)
      IF (LACT.EQ.0)               RETURN

C-        JACT = 0 DEL, 1 REP, 2 ADB, 3 ADD

      JACT  = JBYT (IQ(LACT),9,3)
      NIL   = IAND (IQ(LACT),KM12)
      NDEL  = IAND (IQ(LACT),KM12+KM13)
      MEXE  = IAND (IQ(LACT),15)
      MEXAN = IOR (MEXAN,MEXE)

C--       Find the PREP bank for this action

   43 IF (LQ(LDO-2).NE.LACT)  THEN
          LSV = LDO
          LDO = LQ(LDO-1)
          IF (LDO.EQ.0)            GO TO 61
          GO TO 43
        ENDIF

      MAB = MEXE
      IF (NDEL.NE.0)  MAB = MAB + KM6
      IQ(LDO) = IOR (IQ(LDO),MAB)
      IF (JACT.GE.2)               GO TO 41

C----              +DEL, +REPL:  mark deleted section

      JSLD = IQ(LACT+5) + JSLZER + 1
      LNX  = LDO
   47 LNX  = LQ(LNX-1)
      IF (LNX.EQ.0)                GO TO 41
      JSLA = IQ(LNX+1)
      IF (JSLA.GE.JSLD)            GO TO 41

      IQ(LNX) = IOR (IQ(LNX),MEXE)
      IF (NIL.NE.0)                GO TO 47
      IF (LQ(LNX-2).NE.0)          GO TO 47

      IQ(LNX) = IOR (IQ(LNX),KMDEL)

C--       deleted action?

      JTYP = JBYT (IQ(LNX),9,6)
      IF (JTYP.LT.MCCKEE)          GO TO 47
      IF (JTYP.GE.MCCUSE)          GO TO 47
      NEWNIL = NEWNIL + 1
      GO TO 47

C----              Add trailing actions

   61 JSLSV = IQ(LSV+1) + IQ(LSV+2)

   62 MEXAN = IOR (MEXAN,MEXE)
      IF (NDEL.EQ.0)  THEN
          IF (JACT.LT.2)           GO TO 67
        ELSE
          MEXE = MEXE + KM6
        ENDIF

      CALL MQLIFT (LNEW,LSV,-1,JBKPRE,2)
      LSV = LNEW
      LQ(LSV-2) = LACT
      IQ(LSV)   = IOR (IQ(LSV),MEXE)
      IQ(LSV+1) = JSLSV

   64 LACT = LQ(LACT-1)
      IF (LACT.EQ.0)               RETURN

      JACT = JBYT (IQ(LACT),9,3)
      MEXE = IAND (IQ(LACT),15)
      NDEL = IAND (IQ(LACT),KM12+KM13)
      GO TO 62

C:    DATA CHER( 1) /'illegal action after end-of-deck'/

   67 CALL FAILLN (1,LQ(LACT-3),IQ(LACT+3),IQ(LACT+1),
     +               'Illegal action after end-of-deck')
      GO TO 64
      END
