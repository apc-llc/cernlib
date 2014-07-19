CDECK  ID>, SMJOPT.
      SUBROUTINE SMJOPT

C-    Join the stream-specific and general options for all types
C.    started  5-may-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
      CHARACTER    LINEWK*256, COLWK(256)*1, CHWK*4
      EQUIVALENCE (LINEWK,SLLINE), (COLWK,SLLINE)


      DO 49  JTYP=1,3
      DO 49  JSTR=1,4

      LINEWK = ' '
      JPUT = 1

C--       set the common options first

      JACT = 2
      NL = NLINST(1,JACT,JTYP)
      JL = 0
   24 JL = JL + 1
      IF (JL.GT.NL)                GO TO 31
      IX = IXINST(JL,1,JACT,JTYP)
      IF (IX.EQ.0)                 GO TO 31
      CALL NA_GET (IX,LINEWK,JPUT)
      JPUT = NESLAT + 1
      GO TO 24

C--       add the stream-specific options

   31 JACT = 1
      JSUS = JSTR
      NLOOP = 0
   32 NL = NLINST(JSUS,JACT,JTYP)
      IF (NL.EQ.0)                 GO TO 46
      IX = IXINST(1,JSUS,JACT,JTYP)
      IF (IX.EQ.0)                 GO TO 46

      CALL NA_GET (IX,CHWK,1)
      IF (CHWK(1:2).NE.'=:')       GO TO 35
      JSUS = ICDECI (CHWK,3,4)
      IF (JSUS.LE.0)               GO TO 91
      IF (JSUS.GT.4)               GO TO 91
      NLOOP = NLOOP + 1
      IF (NLOOP.GT.7)              GO TO 92
      GO TO 32

   35 JL = 1
   36 CALL NA_GET (IX,LINEWK,JPUT)
      JPUT = NESLAT + 1
      JL   = JL + 1
      IF (JL.GT.NL)                GO TO 46
      IX = IXINST(JL,JSUS,JACT,JTYP)
      IF (IX.NE.0)                 GO TO 36

C--       done, store this line

   46 N = LNBLNK(LINEWK(1:JPUT))
      IF (N.EQ.0)                  GO TO 48
      CALL CSQMBL (LINEWK,1,N)
      N = NDSLAT
      N = NA_LONG (LINEWK(1:N))
   48 IXOPTN(JSTR,JTYP) = N
   49 CONTINUE
      RETURN

C----         Faulty compiler instructions

   91 WRITE (IQPRNT,9091) CHWK
 9091 FORMAT (/' Trying to do: ',A)
      CALL P_KILL ('Faulty compile-stream re-direction')

   92 CALL P_KILL ('Compile-stream re-direction loop')
      END
