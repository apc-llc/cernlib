CDECK  ID>, SMDISP.
      SUBROUTINE SMDISP (JTARG)

C-    Display the compiler instructions for data type JTYP
C.    started 20-jan-94

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

      CHARACTER    LINEWK*100,      CHWK*4
      EQUIVALENCE (LINEWK,SLLINE), (CHWK,LINEWK)


      JTYP  = JTARG
      NLEAD = 9
      IF (JTYP.GE.4)               GO TO 41
      WRITE (IQPRNT,9001)

C--           check if stream 3 and 4 need to be displayed

      NSTRDO = 4
      IF (MOPTIO(22).NE.0)         GO TO 24
      NSTRDO = 2

      DO 22  JSTR=3,4
      IXU = IXINST(1,JSTR,4,JTYP)
      IF (IXU.EQ.0)                GO TO 22
      CALL NA_GET (IXU, CHWK, 1)
      IF (CHWK(1:2).NE.'=:')  NSTRDO= JSTR
   22 CONTINUE

C----         Do all actions of type JTYP

   24 DO 29  JACT=1,4

      JTAC  = 4*(JTYP-1) + JACT
      IFSTR = 0
      NSTR  = 1
      IF (JACT.EQ.2)               GO TO 26
      IF (JACT.EQ.3)               GO TO 26
      IFSTR = 1
      NSTR  = NSTRDO

   26 JSTR = 0
   27 IF (JSTR.EQ.NSTR)            GO TO 29
      JSTR  = JSTR + 1
      NLINE = NLINST(JSTR,JACT,JTYP)
      JLINE = 1

      LINEWK = CHTYP(JTAC)
      IF (IFSTR.EQ.0)              GO TO 28

      CALL CSETDI (JSTR,LINEWK,7,NLEAD)
      CALL CLEFT  (LINEWK,4,NLEAD)

   28 CALL NA_GET (IXINST(JLINE,JSTR,JACT,JTYP), LINEWK, NLEAD+1)
      JE = NESLAT - 1
      WRITE (IQPRNT,9002) LINEWK(1:JE)

      IF (JLINE.GE.NLINE)          GO TO 27
      JLINE = JLINE + 1
      IF (JLINE.NE.2)              GO TO 28
      LINEWK(1:NLEAD) = ' '
      GO TO 28
   29 CONTINUE
      RETURN

C----         Display start or end

   41 IF (JTYP.GE.6)               GO TO 51
      JACDO = 1
      IF (JTYP.NE.4)  JACDO= 3
      JTYP   = 4
      JTAC   = 12 + JACDO
      LINEWK = CHTYP(JTAC)
      IFANY  = 0

      DO 49  JACT=JACDO,JACDO+1
      DO 49  JSTR=1,4
      JLINE = 0
      NLINE = NLINST(JSTR,JACT,JTYP)

   44 IF (JLINE.GE.NLINE)          GO TO 49
      JLINE = JLINE + 1

      CALL NA_GET (IXINST(JLINE,JSTR,JACT,JTYP), LINEWK, NLEAD+1)
      JE = NESLAT - 1

      IF (IFANY.EQ.0)  WRITE (IQPRNT,9001)
      WRITE (IQPRNT,9002) LINEWK(1:JE)

      IF (IFANY.NE.0)              GO TO 44
      LINEWK(1:NLEAD) = ' '
      IFANY = 7
      GO TO 44
   49 CONTINUE
      RETURN

C----         Display source directory

   51 IF (MOPTIO(22).EQ.0)  THEN
          IF (IXSDIR.EQ.0)         RETURN
        ENDIF

      LINEWK = CHTYP(17)
      CALL NA_GET (IXSDIR, LINEWK, NLEAD+1)
      JE = NESLAT - 1

      WRITE (IQPRNT,9001)
      WRITE (IQPRNT,9002) LINEWK(1:JE)
      RETURN

 9001 FORMAT (A)
 9002 FORMAT (1X,A)
      END
