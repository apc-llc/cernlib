CDECK  ID>, CCCOIX.
      SUBROUTINE CCCOIX (IXPARM)

C-    Put the comment field of the current c/line alrady stored
C-    into CCCOMF into the name-stack and return its index

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
C--------------    End CDE              --------------------------------


      IF (NCHCCC.LT.0) CALL P_CRASH ('CCCOIX - comment field not ready')
      IF (NCHCCC.LE.0)                GO TO 96

      IF (IXCCC.EQ.0)  IXCCC= NA_LONG (CCCOMF(1:NCHCCC))
      IXPARM = IXCCC
      RETURN

C--                Empty string

   96 IXPARM = 0
      RETURN
      END
