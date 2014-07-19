CDECK  ID>, CCCOMG.
      SUBROUTINE CCCOMG (MODE)

C-    Get the comment field of the current c/line stored into CCCOMF
C-    MODE = 0 : as is
C-           1 : ignore leading blanks

      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
C--------------    End CDE              --------------------------------


      CCCOMF = ' '
      IXCCC  = 0
      JTK = NCHCCD + 1
      IF (MODE.NE.0)    JTK = ICFNBL (CCKARD,JTK,NCHCCT)

      NCHCCC = NCHCCT+1 - JTK
      NCHCCC = MAX (NCHCCC,0)
      IF (NCHCCC.EQ.0)             RETURN

      CCCOMF(1:NCHCCC)= CCKARD(JTK:JTK+NCHCCC-1)
      NCHCCC = LNBLNK(CCCOMF(1:NCHCCC))
      RETURN
      END
