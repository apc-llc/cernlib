CDECK  ID>, SMRXQT.
      SUBROUTINE SMRXQT

C-    Read the Nyshell .xqtlog file and crack each entry
C.    started 5-may-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
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


C--           construct the name of the .xqtlog file

      CHLIFI = CHNLOG(1:NNLOG) // '.xqtlog'
      NLIFI  = NNLOG + 7

C--           get the .xqtlog file into memory

      IXTEMP = NA_LONG (CHLIFI(1:NLIFI))
      CALL INIPAM (11, IXTEMP, 0,2)
      IF (IXFLUN.LT.0)             RETURN

      MODE  = 0
   20 LQLEND(2) = LQLSTA(2)
      IQ(LQHOLD+2) = 0
      CALL ARRIVE (3)

C----              Do next line

      JSLIN  = IQ(LQHOLD+1) - 1
      NSLIN  = IQ(LQHOLD+2)
      JSLEND = JSLIN + NSLIN

   21 JSLIN = JSLIN + 1
      IF (JSLIN.GT.JSLEND)         GO TO 69

      CALL LN_GET (JSLIN,LINE,512)
      NXX = NDSLAT
      IF (NXX.EQ.0)                GO TO 21
      NXX = LNBLNK (LINE(1:NXX))
      IF (NXX.EQ.0)                GO TO 21

      IF (MODE.NE.0)               GO TO 22
      IF (LINE(1:8).NE.'>.xqtlog') GO TO 91
      MODE = 1
      GO TO 21

C--           get the logical stream  type:n

   22 JTA = ICNEXT (LINE,2,NXX)
      JTE = NESLAT
      NTA = NDSLAT
      IF (JTA.GE.7)                GO TO 91
      IF (NTA.LT.3)                GO TO 91
      CALL SMCODE (LINE(JTA:JTA+NTA-1))
      IF (JXTYP.EQ.0)              GO TO 91
      IF (JXTYP.GT.3)              GO TO 91
      IF (JXACT.NE.4)              GO TO 91
      IF (MODE.GE.2)               GO TO 24

C--           get the compiler options in the prelude

      IF (LINE(1:1).NE.'>')        GO TO 23
      IF (JTE.GT.NXX)              GO TO 21
      JNA = ICFNBL (LINE,JTE,NXX)
      IXOPTP(JXSTR,JXTYP) = NA_LONG (LINE(JNA:NXX))
      GO TO 21

C--           get the routine name

   23 MODE = 2
   24 JNA = ICNEXT (LINE,JTE,NXX)
      JNE = NESLAT
      NNA = NDSLAT
      IF (NNA.EQ.0)                GO TO 91

C----         register the new routine name

      IXNAME = NA_LONG (LINE(JNA:JNA+NNA-1))
      NROUT  = NROUT + 1
      IF (NROUT.GT.NRTSZ)          GO TO 93
      NNAME(NROUT) = ((IXNAME*8 + JXTYP)*8 + JXSTR)*4
      GO TO 21

C----         Done

   69 IF (JDKNEX.LT.4)             GO TO 20
      CALL INIPAM (0,0, -1,0)
      NRXQT = NROUT
      RETURN

C----         Fault

   91 CALL INIPAM (0,0, -1,0)
      CALL P_KILL ('Not a valid .xqtlog file')

   93 WRITE (IQPRNT,9093) NRTSZ
 9093 FORMAT (/' ***!!!  More than',I7,' routines  !!!***'
     F/' complain to zoll@cern.ch')

      CALL P_KILL ('Too many routines')
      END
