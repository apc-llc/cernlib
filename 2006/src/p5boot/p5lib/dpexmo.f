CDECK  ID>, DPEXMO.
      SUBROUTINE DPEXMO (JSLP,NSLP)

C-    MODIFY: check new lines against disk file
C-    if NSL zero: check we have actually reached the EoF
C.    started 7-OCT-93

      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      COMMON /DEPMOD/MO_JSA, MO_JSL, MO_JSE
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------


      JSLX = JSLP
      NSLX = NSLP
      IF (NSLX.EQ.0)               GO TO 41
      IF (MO_JSL+NSLX.GT.MO_JSE)   GO TO 44

      JTX = MLIAD(JSLX)
      JMO = MLIAD(MO_JSL)

      NTX = MLIAD(JSLX+NSLX)   - JTX
      NMO = MLIAD(MO_JSL+NSLX) - JMO
      IF (NMO.NE.NTX)              GO TO 44

      IF (ICEQU(TEXT(JTX),TEXT(JMO),NTX).NE.0)  GO TO 44
      MO_JSL = MO_JSL + NSLX
      RETURN

C--           Check Eof reached

   41 IF (MO_JSL.EQ.MO_JSE)        RETURN

C--           Comparison failure, re-write the material up to now

   44 JD_MOD = 0
      CALL CICLOS (JD_LUN)
      CALL CIOPEN (JD_LUN, 'w', CHLIFI(1:NLIFI), ISTAT)
      IF (ISTAT.NE.0)  CALL P_FATAM ('CIO open for re-write fails')

      JMO = MLIAD(MO_JSA)
      NMO = MLIAD(MO_JSL) - JMO
      IF (NMO.EQ.0)                RETURN

      CALL CIPUT (JD_LUN,TEXT(JMO),NMO,ISTAT)
      IF (ISTAT.NE.0)  CALL P_FATAM ('CIO write fails')
      RETURN
      END
