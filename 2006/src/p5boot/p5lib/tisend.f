CDECK  ID>, TISEND.
      SUBROUTINE TISEND

C-    Send the material of the current deck to output
C.    started 26-may-94

      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
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


      LDO = KQPREP + 1
   21 LDO = LQ(LDO-1)
      IF (LDO.EQ.0)                RETURN
      LNEW = LQ(LDO-2)
      IF (LNEW.LT.0)         GO TO 21
      IF (LNEW.EQ.0)  THEN
          JSL = IQ(LDO+1)
          NSL = IQ(LDO+2)
        ELSE
          JSL = IQ(LNEW+1)
          NSL = IQ(LNEW+2)
        ENDIF
      IF (NSL.EQ.0)                GO TO 21

      JTX = MLIAD(JSL)
      NTX = MLIAD(JSL+NSL) - JTX
      CALL CIPUT (JD_LUN,TEXT(JTX),NTX,ISTAT)
      IF (ISTAT.NE.0)  CALL P_KILLM ('CIO write fails')
      GO TO 21
      END
