CDECK  ID>, DINEIN.
      SUBROUTINE DINEIN (JSLGO,LPAT)

C-    Read the patch pointed to by LPAT into memory starting at JSLGO
C.    started 21-june-94

      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
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


      IN_FD = IQ(LARRV+2)

C--       position the file

      NSLSKI = IQ(LPAT+7)
      NTXSKI = IQ(LPAT+8)

      CALL CISEEK (IN_FD,NTXSKI,ISTAT)
      IF (ISTAT.NE.0)  CALL P_KILLM ('CIO seek fails')

C--       read the patch

      JSLA = JSLGO
      NSLN = IQ(LPAT+2)
      NTXN = IQ(LPAT+3)
      JTXA = MLIAD(JSLA)

      CALL CIGET (IN_FD,TEXT(JTXA),NTXN,NTXR,IN_EOF)
      IF (IN_EOF.NE.0)       GO TO 93

      JTXE = JTXA + NTXR
      IF (ICHAR(TEXT(JTXE-1)) .NE. NEWLN)  THEN
          TEXT(JTXE) = CHAR(NEWLN)
          JTXE = JTXE + 1
          NTXR = NTXR + 1
        ENDIF

      IF (NTXR.NE.NTXN)      GO TO 92

C--       construct the line directory

      JSLE = JSLA

      DO 26  JTXU=JTXA,JTXE-1
      IF (ICHAR(TEXT(JTXU)) .EQ. NEWLN)  THEN
          JSLE = JSLE + 1
          MLIAD(JSLE) = JTXU + 1
        ENDIF
   26 CONTINUE
      IF (JSLE.NE.JSLA+NSLN)       GO TO 91
      RETURN

   91 CALL P_KILL ('Crash DINEIN: wrong number of lines')
   92 CALL P_KILL ('Crash DINEIN: wrong number of characters')
   93 CALL P_KILLM ('CIO read fails in DINEIN')

      END
