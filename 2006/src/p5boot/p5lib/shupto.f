CDECK  ID>, SHUPTO.
      SUBROUTINE SHUPTO

C-    For Uptodate mode: get the names of all .o files,
C-    check each to be in the .log list, if not delete it
C.    started 4-feb-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
      CHARACTER    LINE*64
      EQUIVALENCE (LINE, SLLINE)

      INTEGER      SYSTEMF

      IAND (IZV,IZW) = AND (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)


      CALL UNLINKF     ('temp_dir.fca')
      IXTEMP = NA_LONG ('temp_dir.fca')

      LINE   = 'ls *.o >'
C-              _:.=+=.: 1_:
      CALL NA_GET (IXTEMP, LINE,10)
      N = NESLAT - 1
      ISTAT = SYSTEMF (LINE(1:N))

      CALL INIPAM (11, IXTEMP, 0,2)
      IF (IXFLUN.LT.0)             GO TO 59

   20 LQLEND(2) = LQLSTA(2)
      IQ(LQHOLD+2) = 0
      CALL ARRIVE (3)


C----              Do next line

      JSLIN  = IQ(LQHOLD+1) - 1
      NSLIN  = IQ(LQHOLD+2)
      JSLEND = JSLIN + NSLIN

   21 JSLIN = JSLIN + 1
      IF (JSLIN.GT.JSLEND)         GO TO 49

      CALL LN_GET (JSLIN,LINE,512)
      NXX = NDSLAT
      IF (NXX.EQ.0)                GO TO 21
      NXX = LNBLNK (LINE(1:NXX))
      IF (NXX.EQ.0)                GO TO 21

C--           get the routine name

      JNA = ICNEXT (LINE,1,NXX)
      JNE = NESLAT - 1
      IF (LINE(JNE-1:JNE).NE.'.o') GO TO 21
      JNN = JNE - 2
      IF (JNN.LT.JNA)              GO TO 21
      IXU = NA_LONG (LINE(JNA:JNN))

C--           find this name in the list of routines

C-        NNAME(JR) = ((IXNAME*8 + JXTYP)*8 + JXSTR)*4 + IFLNEW

      DO 24  JR=NRXQT+1,NROUT
      MM  = NNAME(JR)
      IXN = ISHFTR (MM,8)
      IF (IXN.EQ.IXU)  THEN
          IF (IAND(MM,3).EQ.2)     GO TO 21
          GO TO 27
        ENDIF
   24 CONTINUE
      NRDEL = NRDEL + 1

C--           delete .o file if not in the list, if to be re-compiled

   27 CALL UNLINKF (LINE(JNA:JNE))
      IF (MOPTIO(22).EQ.0)         GO TO 21
      WRITE (IQPRNT,9028) LINE(JNA:JNE)
 9028 FORMAT (7X,'delete file ',A)
      GO TO 21

C----         Done

   49 IF (JDKNEX.LT.4)             GO TO 20
      CALL INIPAM (0,0, -1,0)
   59 CALL UNLINKF ('temp_dir.fca')
      RETURN
      END
