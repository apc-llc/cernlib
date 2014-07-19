CDECK  ID>, SMCIL.
      SUBROUTINE SMCIL (MODE, CODE, XTEXT)

C-    Store a compiler instructions line
C-        MODE = -1  start at line 1,  =0 add one line
C-        CODE  encoded: JXTYP type number, JXACT action, JXSTR stream
C-        XTEXT text to be stored
C.    started 20-jan-94

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

      CHARACTER    CODE*(*), XTEXT*(*), LINE*512
      EQUIVALENCE (LINE, TEXT)


      CALL SMCODE (CODE)
      IF (JXTYP.EQ.0)              GO TO 94
      NTXT  = LENOCC (XTEXT)
      IF (JXTYP.GE.5)              GO TO 31

      IF (MODE.LT.0)  THEN
          NLINST(JXSTR,JXACT,JXTYP) = 0
          IXINST(1,JXSTR,JXACT,JXTYP) = 0
        ENDIF

      IF (NTXT.EQ.0)               RETURN

      JLINE = NLINST(JXSTR,JXACT,JXTYP)

   25 JLINE = JLINE + 1
      IF (JLINE.GT.NLNSZ)          GO TO 91

      IXU = NA_LONG (XTEXT(1:NTXT))
      IXINST(JLINE,JXSTR,JXACT,JXTYP) = IXU
      NLINST(JXSTR,JXACT,JXTYP) = JLINE
      RETURN

C--           sdir name

   31 IXSDIR = 0
      IF (NTXT.GT.0)  IXSDIR= NA_LONG (XTEXT(1:NTXT))
      RETURN

C----         trouble

   91 NMAX = NLNSZ
      IF (JXTYP.NE.4)              GO TO 93
      IF (JXSTR.EQ.4)              GO TO 92
      JXSTR  = JXSTR + 1
      JLINE = 0
      GO TO 25

   92 IF (INDEX (CODEX,':').EQ.0)  NMAX = 8*NMAX
      IF (JXACT.EQ.2)              GO TO 93
      IF (JXACT.EQ.4)              GO TO 93
      JXACT = JXACT + 1
      JXSTR = 1
      JLINE = 0
      GO TO 25

   93 WRITE (IQPRNT,9091)  NMAX,CODEX
 9091 FORMAT (/' ***!!! Capacity of',I3,' lines exceeded for ',A)
      GO TO 97

   94 WRITE (IQPRNT,9091)  CODE,XTEXT
 9094 FORMAT (' ***!!! Faulty init with ',A,' for ',A,' !!!***')

   97 CALL P_KILL ('Abend')
      END
