CDECK  ID>, X_SEQ.
      SUBROUTINE X_SEQ (LXQT)

C-    Send the sequences called from bank LXQT to output.
C.    started 21-jan-92

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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
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
      COMMON /MSAVEC/ MSAVE(5,MAXLEV)

      IAND (IZV,IZW) = AND (IZV, IZW)


      IFLNHI = 0
      IFLNOL = 1 - NVEXDK(2)

      LEVK = 0
      LCSQ = LXQT

      LNACT = 0
      LLACT = 0

C----     New XSQ bank

   21 JSEQ = 0
      NSEQ = IQ(LCSQ+NOFFDA)
      IFLPAS = IAND (IQ(LCSQ),KM16)
   22 JSEQ = JSEQ + 1
      IF (JSEQ.GT.NSEQ)            GO TO 37
      IXSEQ =       IQ(LCSQ+JSEQ+NOFFDA)
      LKEEP = IABS (LQ(LCSQ-JSEQ-NOFFLI))
      IF (LKEEP.NE.0)              GO TO 24

C--       missing sequence
      IF (IFLPAS.NE.0)             GO TO 22
      CALL DPLMSQ (IXSEQ)
      GO TO 22


C--       Output of the KEEP bank itself

   24 IF (NVEXDK(2).NE.0)  IFLNOL= MAX (IFLNHI, IAND (IQ(LKEEP),KM16))

C--           ready special sequence

      IF (IAND(IQ(LKEEP),KM13).NE.0)  THEN
          CALL X_SEQSP (IQ(LCSQ+1), IXSEQ, IFSEND)
          IF (IFSEND.EQ.0)         GO TO 22
          GO TO 25
        ENDIF

C--       be sure the sequence is ready for output

      IF (IAND(IQ(LKEEP),KM5).EQ.0)  MX= LINKSQ (0,LKEEP)

   25 JCCTYP = 0
      IF (IFLNOL.EQ.0)  THEN
          LLORG = 0
          LNORG = LQ(LKEEP-3)
          CALL DPTAG  (2,LEVK,IXSEQ)

          IF (IQ(LKEEP+2).NE.0)    GO TO 27
          LNX = LQ(LKEEP-2)
          IF (LNX.EQ.0)  THEN
              NCHTAG = 0
              GO TO 22
            ENDIF

          IF (LQ(LNX-2).EQ.LLORG)  THEN
              IFLST = LEVK+1
              GO TO 28
            ENDIF

   27     IFLST = 0
          CALL DPLIST (1,LKEEP,0,LEVK+1)
        ENDIF

      IF (NVEXDK(1).NE.0)  CALL DPEXE (LKEEP)

C--       Output of continuation MAT banks

   28 LDO = LKEEP - 1
   29 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 22

      IF (IFLNOL.EQ.0)  THEN
          LNORG = LQ(LDO-2)
          LVINC = JBYT(IQ(LDO),21,5)
          IF (LVINC.EQ.0)  THEN
              IF (IFLST.EQ.0)  CALL DPTAG (2,LEVK,0)
            ELSE
              IF (LNORG.NE.LLORG)  THEN
                  CALL DPTAG (0,LEVK+LVINC,0)
                ENDIF
            ENDIF
          CALL DPLIST (1,LDO,0,IFLST)
          IFLST = 0
        ENDIF

      JCCTYP = JBYT (IQ(LDO),9,6)
      IF (JCCTYP.GE.MCCCDE)        GO TO 31
      IF (NVEXDK(1).EQ.0)          GO TO 29
      IF (JCCTYP.EQ.MCCINC)        GO TO 30
      IF (JCCTYP.NE.0)             GO TO 29
      CALL DPEXE (LDO)
      GO TO 29

   30 CALL X_INCL (LDO)
      GO TO 29

C----       new sequence call bank, step level up by one

   31 LEVK = LEVK + 1
      MSAVE(1,LEVK) = LCSQ
      MSAVE(2,LEVK) = LDO
      MSAVE(3,LEVK) = JSEQ
      MSAVE(4,LEVK) = IFLNHI
      MSAVE(5,LEVK) = IFLNOL
      IFLNHI = IFLNOL
      LCSQ   = LDO
      GO TO 21

C----       end of doing this sequence call bank, step level down

   37 IF (LEVK.EQ.0)               GO TO 39

      LCSQ   = MSAVE(1,LEVK)
      LDO    = MSAVE(2,LEVK)
      JSEQ   = MSAVE(3,LEVK)
      IFLNHI = MSAVE(4,LEVK)
      IFLNOL = MSAVE(5,LEVK)
      LEVK   = LEVK - 1

      NSEQ   = IQ(LCSQ+NOFFDA)
      IFLPAS = IAND (IQ(LCSQ),KM16)
      GO TO 29

   39 NCHTAG = 0
      RETURN
      END
