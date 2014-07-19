CDECK  ID>, CCCRAK.
      SUBROUTINE CCCRAK (LUNDES)

C-    The comment field of the current c/line is a file-name,
C-    krack it and store its name index into IXCCC
C-    LUNDES is the file type for FLKRAK
C-  = 1 cradle, 2 PAM, 4 print, 5 Fortran, 6 ASM write, 7 ASM read-write

C-    CCCOMG must have been called first

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------


      IF (NCHCCC.EQ.0)             RETURN

      CALL FLKRAK (1,LUNDES)
      IXCCC = IXFLUN

      CALL NA_GET (IXFLUN,CCCOMF,1)
      NCHCCC = NDSLAT
      RETURN
      END
