CDECK  ID>, SHWSTA.
      SUBROUTINE SHWSTA (JDO)

C-    Output of shell-commands for "start" and "end"
C-        JDO = 1  start of the script
C-              2  end of the script
C.    started 20-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER     (NTYSZ=4, NACSZ=4, NLNSZ=4,  NRTSZ=10000)
      CHARACTER      CHNLOG*80,    CODEX*8, CHTYP*8
      COMMON /SHMKC/ NNLOG,CHNLOG, CODEX,   CHTYP(17)
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
     +,    NLINST(4,NACSZ,NTYSZ), IXINST(NLNSZ,4,NACSZ,NTYSZ)
     +,    NCOUNTR(4,NTYSZ), NCSUMR(NTYSZ)
     +,    NCOUNTH(4,NTYSZ), NCSUMH(NTYSZ), IXOPTN(4,3), IXOPTP(4,3)
     +,    NRXQT, NROUT, NNAME(NRTSZ),  NRALLC, NRDEL
     +,    LUNSH, IXOBJ, IXSDIR,  JXTYP, JXACT, JXSTR,  LASTSH
C--------------    End CDE              --------------------------------
      CHARACTER    LINEWK*524
      EQUIVALENCE (LINEWK,SLLINE)


C----         Write part 1 of the end text

      JTYP = 4
      JACT = 3
      IF (JDO.NE.1)                GO TO 21

C----         Write part 1 of the start text

      WRITE (LUNSH,9020)
 9020 FORMAT ('#!/bin/csh -f')
      JACT = 1

C----         Send the CIL lines to the shell script

   21 DO 38  JSTR=1,4

      NLINES = NLINST(JSTR,JACT,JTYP)
      IF (NLINES.EQ.0)              GO TO 38

      DO 37  JLINE=1,NLINES

      CALL NA_GET (IXINST(JLINE,JSTR,JACT,JTYP), LINEWK,1)
      JE  = NESLAT - 1

      WRITE (LUNSH,9036) LINEWK(1:JE)
 9036 FORMAT (A)
   37 CONTINUE
   38 CONTINUE

      IF (JACT.EQ.2)               RETURN
      IF (JACT.EQ.4)               RETURN

C----         Write part 2 of the start or end text

      JACT = JACT + 1
      IF (JACT.EQ.2)  CALL SHWPAR
      GO TO 21
      END
