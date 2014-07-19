CDECK  ID>, CCJOIN.
      SUBROUTINE CCJOIN

C-    Check T=JOIN is present on current control line

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
C--------------    End CDE              --------------------------------


      IF (NQJOIN.LT.0)             RETURN
      NQJOIN = 0
      IF (NCCPT.EQ.0)              RETURN
      LOC = JCCPT
      NTH = NCCPT
   24 LOC = LOC + 3
      JXX = MCCPAR(LOC+1)
      NXX = MCCPAR(LOC+2)
      J   = ICNTH (CCKARD(JXX:JXX+NXX-1),'J*OIN ',1)
      IF (J.NE.0)                  GO TO 27
      NTH = NTH - 1
      IF (NTH.GT.0)                GO TO 24
      RETURN

   27 NQJOIN = 1
      RETURN
      END
