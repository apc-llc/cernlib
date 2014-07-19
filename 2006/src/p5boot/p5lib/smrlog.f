CDECK  ID>, SMRLOG.
      SUBROUTINE SMRLOG

C-    Read the Patchy log file and crack each entry
C.    started 20-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      CHARACTER    LINE*256
      EQUIVALENCE (LINE, SLLINE)

      CHARACTER    SCDIR*80, FILEN*128, CHWK*4
      INTEGER      STATF


      IAND (IZV,IZW) = AND (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

      IF (NRXQT.EQ.0)  THEN
          MOPTIO(2) = MOPTIO(2) + MOPTIO(5)
          IF (MOPTIO(2).EQ.0)  MOPTIO(1)= 1
        ENDIF

C--           set the source directory

      IF (IXSDIR.NE.0)  THEN
          CALL NA_GET (IXSDIR,SCDIR,1)
          NDIR = NDSLAT
        ELSE
          NDIR = 0
        ENDIF

C--           get the .log file into memory

      NROUT  = NRXQT
      NIGN   = 0
      IFDOTH = 0

      CALL INIPAM (11, IXLUN(1), 0,0)

   20 LQLEND(2) = LQLSTA(2)
      IQ(LQHOLD+2) = 0
      CALL ARRIVE (3)

C----              Do next line

      JSLIN  = IQ(LQHOLD+1) - 1
      NSLIN  = IQ(LQHOLD+2)
      JSLEND = JSLIN + NSLIN

   21 JSLIN = JSLIN + 1
      IF (JSLIN.GT.JSLEND)         GO TO 39

      CALL LN_GET (JSLIN,LINE,512)
      NXX = NDSLAT
      IF (NXX.EQ.0)                GO TO 21
      NXX = LNBLNK (LINE(1:NXX))
      IF (NXX.EQ.0)                GO TO 21
      IF (IFDOTH.EQ.7)             GO TO 65

C--           get and analyse the logical stream  type:n

      JTA = ICNEXT (LINE,1,NXX)
      JTE = NESLAT
      NTA = NDSLAT
      IF (JTA.GE.11)               GO TO 21
      IF (JTA.GE.4)                GO TO 81
      IF (NTA.LT.3)                GO TO 81

      CALL SMCODE (LINE(JTA:JTA+NTA-1))
      IF (JXTYP.EQ.0)              GO TO 21
      IF (JXTYP.GT.3)              GO TO 21
      IF (JXACT.NE.4)              GO TO 81

C--           check if type:n  is bypassed or re-directed

      NLOOP = 0
   26 IXU = IXINST(1,JXSTR,JXACT,JXTYP)
      IF (IXU.EQ.0)                GO TO 21
      CHWK = ' '
      CALL NA_GET (IXU,CHWK,1)
      IF (CHWK     .EQ.'by  ')     GO TO 21
      IF (CHWK(1:2).NE.'=:')       GO TO 31

      JXSTR  = ICDECI (CHWK,3,4)
      IF (JXSTR.LT.1)              GO TO 92
      IF (JXSTR.GT.4)              GO TO 92
      NLOOP = NLOOP + 1
      IF (NLOOP.GT.7)              GO TO 91
      GO TO 26

C----         Compilable routine, get the routine name

   31 JNA = ICNEXT (LINE,JTE,NXX)
      JNF = NESLAT - 1
      NNA = NDSLAT
      IF (NNA.EQ.0)                GO TO 81

      JNE = ICFIND ('.', LINE,JNA,JNF) - 1
      IXNAME = NA_LONG (LINE(JNA:JNE))
      IFLNEW = 1
      IRCCO  = 0
      IF (MOPTIO(1) .NE.0)         GO TO 38

C--           check the "same" flag is present

      IF (JNF.EQ.NXX)                   GO TO 38
      JINC = ICNEXT (LINE,JNF+1,NXX)
      IF (NDSLAT.NE.4)                  GO TO 38
      IF (LINE(JINC:JINC+3).NE.'same')  GO TO 38
      JINCE  = NESLAT
      IF (MOPTIO(21).NE.0)         GO TO 41

C----         register the new routine name

   37 IFLNEW = 2
   38 NROUT  = NROUT + 1
      IF (NROUT.GT.NRTSZ)          GO TO 93
      NNAME(NROUT) = ((IXNAME*8 + JXTYP)*8 + JXSTR)*4 + IFLNEW

      NCOUNTH(JXSTR,JXTYP) = NCOUNTH(JXSTR,JXTYP) + 1
      NCSUMH(JXTYP)        = NCSUMH(JXTYP)        + 1
      IF (IFLNEW.EQ.2)             GO TO 21
      NCOUNTR(JXSTR,JXTYP) = NCOUNTR(JXSTR,JXTYP) + 1
      NCSUMR(JXTYP)        = NCSUMR(JXTYP)        + 1
      NRALLC = NRALLC + 1
      GO TO 21

C----         Done

   39 IF (JDKNEX.LT.4)             GO TO 20
      IF (NROUT.EQ.NRXQT)          GO TO 94
      CALL INIPAM (0,0, -1,0)
      RETURN

C----         check dependencies for "same" routine in UPTODATE mode

   41 IF (MOPTIO(2).NE.0)          GO TO 46

C--       find this routine in the .xqtlog file

      DO 42  JX=1,NRXQT
      JJ   = NNAME(JX)
      IXNA = ISHFTR (JJ,8)
      IF (IXNA.EQ.IXNAME)          GO TO 44
   42 CONTINUE
      IRCCO = 1
      GO TO 38

C--       check compiler options unchanged

   44 JJ   = ISHFTR(JJ,2)
      JSTR = IAND (JJ,7)
      JJ   = ISHFTR (JJ,3)
      JTYP = IAND (JJ,7)
      IRCCO = 2

      IF (JTYP.NE.JXTYP)           GO TO 38
      IF (IXOPTP(JSTR,JTYP).NE.IXOPTN(JXSTR,JXTYP))  GO TO 38

C--       get the date and length of the .o file

   46 CALL NA_GET (IXNAME,FILEN,1)
      NFI = NESLAT
      CALL NA_GET (IXOBJ, FILEN,NFI)
      NFI = NESLAT - 1

      ISTAO = STATF (FILEN(1:NFI), IQUEST)
      ILENO = IQUEST(7)
      ITIMO = IQUEST(9)
      IRCCO = 4
      IF (ISTAO.NE.0)              GO TO 38
      IRCCO = 5
      IF (ILENO.EQ.0)              GO TO 38
      ITIMOL = ISHFTR (ITIMO,3)
      ITIMOR = IAND (ITIMO,7)

C--       get the date of the .f | .c | .a file

      IF (NDIR.GT.0)  FILEN(1:NDIR)= SCDIR(1:NDIR)
      IRCCO = 6

   61 FILEN(NDIR+1:NDIR+NNA)= LINE(JNA:JNA+NNA-1)
      NFI = NDIR + NNA

C--       read the date and compare to that of the .o file

      ISTAF = STATF (FILEN(1:NFI), IQUEST)
      ITIMF = IQUEST(9)
      IF (ISTAF.NE.0)              GO TO 38

      ITIMFL = ISHFTR (ITIMF,3)
      IF (ITIMFL.LT.ITIMOL)        GO TO 64
      IF (ITIMFL.GT.ITIMOL)        GO TO 38

      ITIMFR = IAND (ITIMF,7)
      IF (ITIMFR.GE.ITIMOR)        GO TO 38

C--       get the name for the next include file, if any

   64 IF (JINCE.GT.NXX)            GO TO 37
      IRCCO = 7
   65 IFDOTH = 0
      JNA   = ICNEXT (LINE,JINCE,NXX)
      JINCE = NESLAT
      NNA   = NDSLAT
      IF (NNA.GT.1)                GO TO 61
      IF (LINE(JNA:JNA).NE.'+')    GO TO 61
      IFDOTH = 7
      JINCE  = 1
      GO TO 21

C----         Ignore meaningless input line

   81 NIGN = NIGN + 1
      WRITE (IQPRNT,9081) LINE(1:NXX)
 9081 FORMAT (' Ignored: ',A)

      IF (NIGN.LT.8)                 GO TO 21
      IF (NIGN.LT.(NROUT-NRXQT)/2)   GO TO 21

      CALL P_KILL ('Faulty Patchy log file')

C----         Faulty compiler instructions

   91 CALL P_KILL ('Compile-stream re-direction loop')

   92 WRITE (IQPRNT,9092) CHWK
 9092 FORMAT (/' Trying to do: ',A)
      CALL P_KILL ('Faulty compile-stream re-direction')

   93 WRITE (IQPRNT,9093) NRTSZ
 9093 FORMAT (/' ***!!!  More than',I7,' routines  !!!***'
     F/' complain to zoll@cern.ch')

      CALL P_KILL ('Too many routines')

   94 CALL P_KILL ('No compilable routines')
      END
