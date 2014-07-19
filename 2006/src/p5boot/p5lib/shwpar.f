CDECK  ID>, SHWPAR.
      SUBROUTINE SHWPAR

C-    Output of the parameter-defining statements for Unix
C.    started 22-feb-94

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
      CHARACTER    LINEWK*256, COLWK(256)*1
      EQUIVALENCE (LINEWK,SLLINE), (COLWK,SLLINE)

      CHARACTER    CHT(3)*1, CHS(4)*1, CHC(3)*2

      DATA  CHT    / 'F',  'C',  'A'  /
      DATA  CHC    / 'FC', 'CC', 'AS' /
      DATA  CHS    / '1',  '2',  '3',  '4' /


   21 LINEWK = 'set FILE =  ' // CHNLOG(1:NNLOG)
C-              _:.=+=.: 1_:.=+=
      N = NNLOG + 12
      WRITE (LUNSH,9000) LINEWK(1:N)

      LINEWK = 'set s    = "'
C-              _:.=+=.: 1_:.=+=
      CALL NA_GET (IXSDIR, LINEWK, 13)
      N  = NESLAT
      COLWK(N) = '"'
      WRITE (LUNSH,9000) LINEWK(1:N)
 9000 FORMAT (A)

C------       All data-types, all active streams

      DO 49  JTYP=1,3
      IF (NCSUMR(JTYP).EQ.0)        GO TO 49

      DO 48  JSTR=1,4
      IF (NCOUNTR(JSTR,JTYP).EQ.0)  GO TO 48

C--       define the compiler options

      LINEWK = 'set xO_y = "  '
C-              _:.=+=.: 1_:.=+=

      COLWK(5) = CHT(JTYP)
      COLWK(8) = CHS(JSTR)

      IX = IXOPTN(JSTR,JTYP)
      CALL NA_GET (IX,LINEWK,13)
      N = NESLAT

      COLWK(N) = '"'
      WRITE (LUNSH,9000) LINEWK(1:N)
   48 CONTINUE

C--       define the compiler name

      JACT = 3
      LINEWK = 'set xx   = "'
C-              _:.=+=.: 1_:.=+=

      LINEWK(5:6) = CHC(JTYP)
      IX = IXINST(1,1,JACT,JTYP)
      CALL NA_GET (IX,LINEWK,13)
      N = NESLAT
      COLWK(N) = '"'
      WRITE (LUNSH,9000) LINEWK(1:N)
   49 CONTINUE
      RETURN
      END
