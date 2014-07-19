CDECK  ID>, DIF_XQT.
      SUBROUTINE DIF_XQT (JSLX,NSLX, JSLY,NSLY, NQUICKP)

C-    Excute differencing,
C-    On entry:
C-        JSLX  slot number in MLIAD of the first line on "old"
C-        NSLX  number of lines on "old"
C-        JSLY, NSLY      same for "new"
C-
C-        NQUICK if not zero:
C-        quick test only to see if at least NQUICK lines do match,
C-           if not so: reset NQUICK to zero
C-               if so: leave NQUICK unchanged,
C-                      return NFAID = 0: perfect match (identity)
C-                                     1: good but not perfect

C-    On exit if NQUICK =zero on entry:
C-        Failure table in MFAID, from JFAIDA to JFAIDE-1,
C-        at the start of control division 2, NFAID entries;
C-        one entry of 5 words describes one failure :
C-
C-        MFAID(JFAID + 0 :  code  -1 pure delete
C-                                  0 delete / insert
C-                                 +1 pure insert
C-                    + 1 :  n of failure-start line for "old"
C-                    + 2 :  n of failure-start line for "new"
C-                    + 3 :  no. of lines deleted
C-                    + 4 :  no. of lines inserted

C-    Internal:
C-        Table of matching regions, one entry describes
C-        one set of matching lines:
C-
C-        MGOOD(JGOOD + 0 :  NM  number of matching lines
C-                    + 1 :  JX  ordinal number of first line on "old"
C-                    + 2 :  JY  ... on "new"
C-
C-          The first entry is at  JGOOD = JGOODA
C-           the last entry is at  JGOOD = JGOODE - 3

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      COMMON /DIFFC/ NFAID, JFAIDA, JFAIDE
        PARAMETER   (LGLIV=4, LGLIV2=8)
      COMMON /DIFXC/ JX_ORG,  JY_ORG,  NX_NLI, NY_NLI
     +,              JX_FEND, JY_FEND, NLI_MAT
     +,              JX_NTH, JX_JSL, JX_JTX, JX_NCH
     +,              JY_NTH, JY_JSL, JY_JTX, JY_NCH
     +,                 LXV_FAI(LGLIV), LYV_FAI(LGLIV)
     +,                 LXV_SCA(LGLIV), LYV_SCA(LGLIV)
     +,                 LXV_MAT(LGLIV), LYV_MAT(LGLIV)
C--------------    End CDE                        -------------------
      COMMON /SLATE/ NDSLAT,NESLAT,DUMMY(38)

      DIMENSION    MGOOD(NSIZEQ), MHAVE(NSIZEQ), MFAID(NSIZEQ)
      EQUIVALENCE (MGOOD,IQ),    (MHAVE,IQ),    (MFAID,IQ)


      NQUICK = NQUICKP
      JX_ORG = JSLX - 1
      JY_ORG = JSLY - 1
      IF (JX_ORG.LT.0 .OR. JY_ORG.LT.0)
     +    CALL P_CRASH ('Faulty slot number in DIF_XQT')
      NX_NLI = NSLX
      NY_NLI = NSLY

      LOWQ   = LQCSTA(2)
      LIMQ   = LQCSTA(3)
      JGOODA = LOWQ
      JHAVEE = LIMQ - 6

      JFAIDA = LOWQ
      JFAIDE = JFAIDA
      NFAID  = 0

C--------      Start, set up a first GOOD table                  -------

      CALL DIF_LX (1)
      CALL DIF_LY (1)

C--       Walk along to find the first failure

      CALL DIF_HEAD
      IF (NDSLAT.EQ.2)             RETURN
      NFAID = 1

      JGOOD = JGOODA
      NGOOD = JX_NTH - 1
      MGOOD(JGOOD)   = NGOOD
      MGOOD(JGOOD+1) = 1
      MGOOD(JGOOD+2) = 1
      JGOOD = JGOOD + 3

      MGOOD(JGOOD)   = 0
      MGOOD(JGOOD+1) = NX_NLI + 1
      MGOOD(JGOOD+2) = NY_NLI + 1
      JGOODE = JGOOD + 3
      IF (NDSLAT.NE.0)  THEN
          NGOOD = NGOOD + 1
          MGOOD(JGOODA) = NGOOD
          JHAVEA = JHAVEE - 6
          CALL UCOPY (MGOOD(JGOODA),MHAVE(JHAVEA),6)
          GO TO 61
        ENDIF

      MAXGAP = MIN (NX_NLI-JX_NTH, NY_NLI-JY_NTH) + 1
      MIMAT  = MIN (MAXGAP/2,128)
      MILILE = 14
      IFTAIL = 0

C--------      Match, construct a new GOOD table          --------------

   21 IF (NQUICK.NE.0)  THEN
          IF (NGOOD.GE.NQUICK)     RETURN
        ENDIF

      MIMAT = MIN (MIMAT,MAXGAP)
      IF (MIMAT.GE.8)  THEN
          MIMAT = MIMAT/2
        ELSE
          MIMAT = MIN(MIMAT,5) - 1
        ENDIF

      NWD    = JGOODE - JGOODA
      JHAVEA = JHAVEE - NWD
      IF (JHAVEA.GT.JGOODE)  THEN
          CALL UCOPY (MGOOD(JGOODA),MHAVE(JHAVEA),NWD)
        ELSE
          CALL UCOPY2 (MGOOD(JGOODA),MHAVE(JHAVEA),NWD)
        ENDIF
      IF (MIMAT.LT.0)              GO TO 61

      MAXGAP  = 0
      NGOOD   = 0
      JGOOD   = JGOODA
      JHAVE   = JHAVEA
      LMATX   = 1
      LMATY   = 1
      NMAT    = MHAVE(JHAVE)
      JX_FEND = 0
      JY_FEND = 0
      GO TO 34

C------       Scan all failure regions

C--       No new match, use the next pre-established match

   31 LMATX = JX_FEND+1
      LMATY = JY_FEND+1
      NMAT  = MATAFT

C--       Is this match an extension of the previous?

   32 N   = MGOOD(JGOOD-3)
      NFX = LMATX - (MGOOD(JGOOD-2)+N)
      NFY = LMATY - (MGOOD(JGOOD-1)+N)
      IF (NFX.NE.0)                GO TO 33
      IF (NFY.NE.0)                GO TO 33
      MGOOD(JGOOD-3) = N + NMAT
      NGOOD = NGOOD + NMAT
      GO TO 36

C--       Record this match

   33 MAXGAP = MAX (MAXGAP, MIN(NFX,NFY))
      IF (JGOOD+4.GE.JHAVEA)       GO TO 60
   34 MGOOD(JGOOD)   = NMAT
      MGOOD(JGOOD+1) = LMATX
      MGOOD(JGOOD+2) = LMATY
      JGOOD = JGOOD + 3
      NGOOD = NGOOD + NMAT

   36 LFAIX = LMATX + NMAT
      LFAIY = LMATY + NMAT

C--       Moving into the next HAVE region?

      N = 0
      IF (LFAIX.GT.JX_FEND)  N= 1
      IF (LFAIY.GT.JY_FEND)  N= N+1
      IF (N.EQ.0)                  GO TO 37
      IF (N.EQ.1)                  GO TO 31

      JHAVE = JHAVE + 3
      IF (JHAVE.GE.JHAVEE)         GO TO 49
      MATAFT  = MHAVE(JHAVE)
      JX_FEND = MHAVE(JHAVE+1) - 1
      JY_FEND = MHAVE(JHAVE+2) - 1

C--       Is the failure gap too small to contain a possible match,
C--       or is it too big for small MIMAT ?

   37 NFAIX = JX_FEND+1 - LFAIX
      NFAIY = JY_FEND+1 - LFAIY
      NGAP  = MIN (NFAIX, NFAIY)
      IF (NGAP.LE.MIMAT)           GO TO 31

      IF (MIMAT.LT.3)  THEN
          NDIS = MAX (NFAIX, NFAIY) - NGAP
          N    = 2*NDIS + NGAP
          IF (N.GE.24*(MIMAT+1))   GO TO 31
        ENDIF

C--       Find match, if any, in this failure gap

      CALL DIF_LX (LFAIX)
      CALL DIF_LY (LFAIY)
      CALL DIF_FMAT (MILILE,MIMAT)
      IF (NLI_MAT.EQ.0)            GO TO 31
      LMATX = LXV_MAT(1)
      LMATY = LYV_MAT(1)
      NMAT  = NLI_MAT

C--       Forward extension of imprecise match

      IF (MATAFT.LT.2)             GO TO 32
      LNNX  = LMATX + NMAT
      LNNY  = LMATY + NMAT
      IF   (LNNX.NE.JX_FEND+1)  THEN
        IF (LNNY.NE.JY_FEND+1)     GO TO 32
       ENDIF
      CALL DIF_SAME (LNNX,LNNY,IFSAME)
      IF (IFSAME.EQ.0)             GO TO 32

      NLEXT = 0
   42 NLEXT = NLEXT + 1
      IF (NLEXT.LT.MATAFT-1)  THEN
          CALL DIF_NX
          CALL DIF_NY
          CALL DIF_SAME (JX_NTH, JY_NTH, IFSAME)
          IF (IFSAME.NE.0)             GO TO 42
        ENDIF
      NMAT    = NMAT    + NLEXT
      JX_FEND = JX_FEND + NLEXT
      JY_FEND = JY_FEND + NLEXT
      MATAFT  = MATAFT  - NLEXT
      GO TO 32

C--       End of this iteration

   49 JGOODE = JGOOD
      IF (MIMAT.GT.9)          GO TO 21
      IF (IFTAIL.NE.0)         GO TO 21
      CALL DIF_TAIL (MGOOD(JGOODE-6))
      IFTAIL = 7
      GO TO 21

C--------     Convert MHAVE table to MFAID table          --------------

   60 CONTINUE
   61 IF (NQUICK.NE.0)  THEN
          IF (NGOOD.LT.NQUICK)  NQUICKP= 0
          RETURN
        ENDIF

      JHAVEE = JHAVEE - 3

      JHAVE = JHAVEA
      JFAID = JFAIDA
   64 JCODE = 0
      NMAT  = MHAVE(JHAVE)
      JFGOX = MHAVE(JHAVE+1) + NMAT
      JFGOY = MHAVE(JHAVE+2) + NMAT
      NLDEL = MHAVE(JHAVE+4) - JFGOX
      NLINS = MHAVE(JHAVE+5) - JFGOY

      IF (NLINS.EQ.0)  THEN
          JCODE = -1
        ELSE
          IF (NLDEL.EQ.0)  JCODE= 1
        ENDIF

      MFAID(JFAID)   = JCODE
      MFAID(JFAID+1) = JFGOX
      MFAID(JFAID+2) = JFGOY
      MFAID(JFAID+3) = NLDEL
      MFAID(JFAID+4) = NLINS
      JFAID = JFAID + 5
      JHAVE = JHAVE + 3
      IF (JHAVE.LT.JHAVEE)         GO TO 64
      JFAIDE = JFAID
      NFAID  = (JFAIDE-JFAIDA) /5

      IF (JFAIDE.GT.LIMQ)  CALL P_KILL ('Capacity exceeded')
      RETURN
      END
