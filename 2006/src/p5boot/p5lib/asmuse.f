CDECK  ID>, ASMUSE.
      SUBROUTINE ASMUSE

C-    End of blank deck of cradle, ready all ASM-streams for use
C.    started 23-jan-92

      PARAMETER      (JFO_TYP=1, JCC_TYP=2, JAS_TYP=3, JDA_TYP=4,
     +                JSH_TYP=5, JCR_TYP=6, JIN_TYP=7, MAX_TYP=100)
      CHARACTER*16    CH_TYP(MAX_TYP), ACT_TYP
      COMMON /CM_TYP/ LUN_TYP, N_TYP, NAL_TYP, NDV_TYP, JBK_TYP
     +,               JU_TYP(MAX_TYP), CH_TYP, ACT_TYP
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
      CHARACTER    MSG*56
      EQUIVALENCE (MSG,SLERRM)


C----         Resolve first level default binding

      JASM = 0
   22 JASM = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 31

      LASMT = LQ(LHASM-JASM)
      LASML = LQ(LASMT-2)

C--       check stream :0 or :1 is physical

      JPH  = 0
      IF (IQ(LASMT+1).EQ.0)        GO TO 24
      JPH  = 1
      IF (IQ(LASML+1).EQ.0)        GO TO 24

C--           default binding for ASMT

      IF (IQ(LASMT+1).LT.0)  THEN
          IF (JASM.EQ.JFO_TYP)  THEN
              MBIND = 0
            ELSEIF (JASM.LE.JCR_TYP)  THEN
              MBIND = 8*JFO_TYP
            ELSEIF (JASM.EQ.JIN_TYP)  THEN
              MBIND = 8*JCC_TYP
            ELSE
              MBIND = 8*JDA_TYP
            ENDIF
          IQ(LASMT+1) = MBIND
        ENDIF

C--       check stream :3 is physical

      JPH = 3
      LASML = LQ(LASMT-4)
      IF (IQ(LASML+1).NE.0)        GO TO 22

C--       if streams :0 or :1 or :3 are physical:
C-        bind subsidiary logical streams

   24 MBIND = 8*JASM + JPH
   25 JPH  = JPH + 1
      IF (JPH.GT.4)                GO TO 22
      LASML = LQ(LASMT-JPH-1)
      IF (IQ(LASML+1).LT.0)  THEN
          IQ(LASML+1) = MBIND
          GO TO 25
        ENDIF

      IF (JPH.EQ.2)                GO TO 25
      IF (IQ(LASML+1).EQ.0)        GO TO 24
      MBIND = IQ(LASML+1)
      GO TO 25

C----         Resolve threading of ASMT

   31 JASM = 0
   32 JASM = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 41
      LASMT = LQ(LHASM-JASM)
      MBIND = IQ(LASMT+1)
C-            =  0  if stream :0 is physical
C-              -1  if stream :1 is physical

      IF (MBIND.LT.0)              GO TO 32
      IF (MBIND.EQ.0)  THEN
          CALL ASMCRX (LASMT)
          IF (IQ(LASMT+2).EQ.3)  IFMODIF= 1
          GO TO 32
        ENDIF

      NSTEP = 0
   36 NSTEP = NSTEP + 1
      IF (NSTEP.GE.99)             GO TO 91
      JASMU  = MBIND / 8
      JDIVU  = MOD (MBIND,8)
      LASMTU = LQ(LHASM-JASMU)
      IF (JDIVU.EQ.0)  THEN
          LASMLU = LASMTU
        ELSE
          LASMLU = LQ(LASMTU-JDIVU-1)
        ENDIF

      IF (IQ(LASMLU+1).GT.0)  THEN
          MBIND = IQ(LASMLU+1)
          GO TO 36
        ENDIF

      IQ(LASMT+1) = MBIND
      GO TO 32

C----         Resolve threading of ASML

   41 NUNRES = 999
   42 NUNPRE = NUNRES
      NUNRES = 0
      JASM = 0
   43 JASM = JASM + 1
      IF (JASM.GT.N_TYP)           GO TO 59
      LASMT = LQ(LHASM-JASM)
      JDIV  = 0
   44 JDIV  = JDIV + 1
      IF (JDIV.GT.4)               GO TO 43
      LASML = LQ(LASMT-JDIV-1)
      IF (IQ(LASML+5).LT.0)  IQ(LASML+5)= IQ(LASMT+5)
      IF (MOPUPD.NE.0)  THEN
          IQ(LASML+6) = 0
          IQ(LASML+7) = 0
        ELSE
          IF (IQ(LASML+6).LT.0)  IQ(LASML+6)= IQ(LASMT+6)
          IF (IQ(LASML+7).LT.0)  IQ(LASML+7)= IQ(LASMT+7)
        ENDIF

C--           stream is physical

      MBIND = IQ(LASML+1)
      IF (MBIND.EQ.0)  THEN
          CALL ASMCRX (LASML)
          IF (IQ(LASML+2).EQ.3)  IFMODIF= 1
          GO TO 44
        ENDIF

C--           default binding

      IF (MBIND.LT.0)  MBIND = 8*JASM
      JASMU = JASM

      NSTEP = 0
   51 JASML = JASMU
      JASMU = MBIND / 8
      JDIVU = MOD (MBIND,8)
      NSTEP = NSTEP + 1
      IF (NSTEP.GE.99)             GO TO 91
      LASMTU = LQ(LHASM-JASMU)
      IF (JDIVU.NE.0)              GO TO 54

C--           binding to stream N:0, try N:jdiv first

      IF (JASMU.NE.JASML)  THEN
          JDIVU = JDIV
          GO TO 54
        ENDIF

      MBIND = IQ(LASMTU+1)
      IF (MBIND.EQ.0)              GO TO 57
      GO TO 51

C--           binding to stream N:i

   54 LASMLU = LQ(LASMTU-JDIVU-1)
      MBIND = IQ(LASMLU+1)
      IF (MBIND.EQ.0)              GO TO 57
      IF (MBIND.GT.0)              GO TO 51
      NUNRES = NUNRES + 1
      GO TO 44

   57 IQ(LASML+1) = 8*JASMU + JDIVU
      GO TO 44

   59 IF (NUNRES.NE.0)  THEN
          IF (NUNRES.LT.NUNPRE)    GO TO 42
          GO TO 91
        ENDIF

      RETURN

C------            Trouble

   91 CALL ASMDUMP ('for loop in binding')
      CALL P_KILL ('loop in ASM binding')
      END
