CDECK  ID>, DPEXE.
      SUBROUTINE DPEXE (LDO)

C-    EXE output of next lot
C.    started 21-jan-92

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
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
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      CHARACTER*8             CH_SUBS, CH_SUBT
      COMMON /DEPSUB/NCH_SUB, CH_SUBS, CH_SUBT
C--------------    End CDE              --------------------------------


      JSLX = IQ(LDO+1)
      NSLX = IQ(LDO+2)
      IF (JCCTYP.NE.0)  THEN
          JSLX = JSLX + 1
          NSLX = NSLX - 1
        ENDIF

      IF (NSLX.LE.0)               RETURN
      IF (JD_NEW.NE.0)  THEN
          CALL DPEXGO
          IF (NVEXDK(1).EQ.0)      RETURN
        ENDIF

      JD_NLI= JD_NLI + NSLX

C--           Control character substitution

      IF (IX_SUB.EQ.0)             GO TO 31
      JSL = JSLX
      N   = NCH_SUB

      DO 24  JJ=1,NSLX
      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+1) - JTX - NCHNEWL
      IF (NTX.LT.4)                GO TO 24

      J   = INDEX (CH_SUBS(1:N),TEXT(JTX))
      IF (J.EQ.0)                  GO TO 24
      IF (JPTYPE(TEXT(JTX)).EQ.0)  GO TO 24

      TEXT(JTX) = CH_SUBT(J:J)
   24 JSL = JSL + 1

C----         Normal output without MODIFY check

   31 IF (JD_MOD.NE.0)             GO TO 41
   32 JTX = MLIAD(JSLX)
      NTX = MLIAD(JSLX+NSLX) - JTX
      CALL CIPUT (JD_LUN,TEXT(JTX),NTX,ISTAT)
      IF (ISTAT.NE.0)  CALL P_FATAM ('CIO write fails')
      RETURN

C----         Output for MODIFY

   41 CALL DPEXMO (JSLX,NSLX)
      IF (JD_MOD.EQ.0)             GO TO 32
      RETURN
      END
