CDECK  ID>, CCOPT.
      SUBROUTINE CCOPT (POSS,NPOSS)

C-    Check T=opt parameters on current control line

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
C--------------    End CDE              --------------------------------
      CHARACTER    POSS(NPOSS)*(*)


      IF (NCCPT.EQ.0)              RETURN
      MM  = 0
      LOC = JCCPT
      NTH = NCCPT
   24 LOC = LOC + 3
      JXX = MCCPAR(LOC+1)
      NXX = MCCPAR(LOC+2)
      J   = ICNTH (CCKARD(JXX:JXX+NXX-1),POSS,NPOSS)
      IF (J.EQ.0)                  GO TO 96
      CALL SBIT1 (MM,J)
      NTH = NTH - 1
      IF (NTH.GT.0)                GO TO 24

   27 MCCPAR(JCCPT+2)= MM
      RETURN

C--                Fault

   96 JCCBAD = JXX
      RETURN
      END
