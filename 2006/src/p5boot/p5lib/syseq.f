CDECK  ID>, SYSEQ.
      SUBROUTINE SYSEQ

C-    Send the sequences called from bank LDOAN at KDOAN to output.
C.    started 24-aug-95

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      PARAMETER   (NOFFLI=2, NOFFDA=4)
      PARAMETER   (MAXLEV=36)
      COMMON /MSAVEC/ MSAVE(6,MAXLEV)

      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTL (IZW,NZB) = LSHIFT (IZW, NZB)

      LEVK   = 0
      LNACT  = 0
      LLACT  = 0
      NCHTAG = 0

      KPC  = LEXP - 3
      KDC  = 0
      IF (LEXD.NE.0)  KDC= LEXD - 3

      NCCPZ = MIN (NCCPZ,61)

      MMBANK(3,JBKXSQ) = NCCPZ + NOFFLI
      MMBANK(5,JBKXSQ) = NCCPZ + NOFFDA
      CALL MQLIFT (LMAT,KDOAN,0,JBKXSQ,2)

      LQ(LMAT-1) = LQ(LDOAN-1)
      IQ(LMAT)   = IOR (IQ(LMAT), ISHFTL(JCCTYP,8))
      IQ(LMAT)   = IOR (IQ(LMAT), IAND(MCCPAR(JCCPT+1),KMP))
      IQ(LMAT+1) = IQ(LDOAN+1)
      IQ(LMAT+2) = IQ(LDOAN+2)

      IQ(LMAT+NOFFDA) = NCCPZ
      CALL UCOCOP (MCCPAR(JCCPZ+1),IQ(LMAT+NOFFDA+1),NCCPZ,1,3,1)

      LDOAN = LMAT
      LCSQ  = LMAT

C----     New XSQ bank

   21 JSEQ = 0
      NSEQ = IQ(LCSQ+NOFFDA)
      IFLPAS = IAND (IQ(LCSQ),KM16)
   22 JSEQ = JSEQ + 1
      IF (JSEQ.GT.NSEQ)            GO TO 67
      ITER  = 0
      NDONE = 0
      IXSEQ = IQ(LCSQ+JSEQ+NOFFDA)
      LKEEP = LQ(LCSQ-JSEQ-NOFFLI)
      IF (LKEEP.NE.0)              GO TO 41

      CALL SYLSEQ (LKEEP, IXSEQ, 0,0)
      IF (LKEEP.EQ.0)              GO TO 51
      LQ(LCSQ-JSEQ-NOFFLI) = LKEEP

C----       Start output of this sequence

   41 IF (IAND(IQ(LKEEP),KM12+KM16).NE.0)  GO TO 22
      IFALTN = NDONE

C--           ready special sequence

      IFSPEC = IAND(IQ(LKEEP),KM13)
      IF (IFSPEC.NE.0)  THEN
          CALL X_SEQSP (IQ(LCSQ+1), IXSEQ, IFSEND)
          IF (IFSEND.EQ.0)         GO TO 22
        ENDIF

      NDONE = NDONE + 1
      LLORG = 0
      LNORG = LQ(LKEEP-3)
      CALL SYKTAG (LEVK,IXSEQ)

C--       output of the start of the sequence expansion

      IF (IQ(LKEEP+2).NE.0)        GO TO 44
      IF (LQ(LKEEP-2).NE.0)        GO TO 43
      CALL SYKTAG (-1,0)
      GO TO 48

   43 IFLST = LEVK+1
      GO TO 45

   44 CALL DPLIST (1,LKEEP,0,LEVK+1)
      IFLST = 0

C--       output of continuation MAT banks

   45 LDO = LKEEP - 1
   46 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                GO TO 48
      JCCTYP = JBYT (IQ(LDO),9,6)

      LNORG = LQ(LDO-2)
      CALL DPLIST (1,LDO,0,IFLST)
      IFLST = 0
      IF (JCCTYP.GE.MCCCDE)        GO TO 61
      GO TO 46

C--       next sequence in multiple definition

   48 IF (IFSPEC.NE.0)             GO TO 22
      LKEEP = LQ(LKEEP-4)
      IF (LKEEP.NE.0)              GO TO 41

C--       do also patch/deck directed sequences

   51 ITER = ITER + 1
      IF (ITER.GE.3)               GO TO 57
      KP = KPC
      IF (ITER.EQ.1)  THEN
          IF (LQ(KP).EQ.0)         GO TO 51
          KD = 0
        ELSE
          IF (KDC.EQ.0)            GO TO 57
          IF (LQ(KDC).EQ.0)        GO TO 57
          KD = KDC
        ENDIF

      CALL SYLSEQ (LKEEP, IXSEQ, KP,KD)
      IF (LKEEP.NE.0)              GO TO 41
      GO TO 51

C--       missing sequence ?

   57 IF (NDONE.NE.0)              GO TO 22
      IF (IFLPAS.NE.0)             GO TO 22
      IF (MOPTIO(13).EQ.0)         GO TO 22
      CALL DPLMSQ (IXSEQ)
      GO TO 22

C----       new sequence call bank, step level up by one

   61 LEVK = LEVK + 1
      MSAVE(1,LEVK) = LCSQ
      MSAVE(2,LEVK) = LDO
      MSAVE(3,LEVK) = JSEQ
      MSAVE(4,LEVK) = NDONE
      MSAVE(5,LEVK) = LKEEP
      MSAVE(6,LEVK) = ITER
      LCSQ   = LDO
      GO TO 21

C----       end of doing this sequence call bank, step level down

   67 IF (LEVK.EQ.0)               GO TO 69

      LCSQ   = MSAVE(1,LEVK)
      LDO    = MSAVE(2,LEVK)
      JSEQ   = MSAVE(3,LEVK)
      NDONE  = MSAVE(4,LEVK)
      LKEEP  = MSAVE(5,LEVK)
      ITER   = MSAVE(6,LEVK)
      LEVK   = LEVK - 1

      IFALTN = NDONE - 1
      NSEQ   = IQ(LCSQ+NOFFDA)
      IXSEQ  = IQ(LCSQ+JSEQ+NOFFDA)
      IFLPAS = IAND (IQ(LCSQ),KM16)
      IFSPEC = 0
      IFLST  = 0
      CALL SYKTAG (LEVK,0)
      GO TO 46

   69 NCHTAG  = 0
      NQWYLDO = 7
      RETURN
      END
