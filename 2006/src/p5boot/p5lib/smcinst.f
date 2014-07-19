CDECK  ID>, SMCINST.
      SUBROUTINE SMCINST

C-    Read the user-defined compiler instructions
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
      CHARACTER    LINE*512
      EQUIVALENCE (LINE, SLLINE)


      INCRAD = 3
      CALL ARRIVE (0)
      INCRAD = 0

      JSLIN  = IQ(LQHOLD+1) - 1
      JSLEND = JSLIN + IQ(LQHOLD+2)
      NIGN   = 0
      JXTYP  = 0
      NLINE  = 0

C----              Read next line

   21 JSLIN = JSLIN + 1
      IF (JSLIN.GT.JSLEND)         GO TO 49

      CALL LN_GET (JSLIN,LINE,512)
      NXX = NDSLAT
      IF (NXX.EQ.0)                GO TO 21
      NXX = LNBLNK (LINE(1:NXX))
      IF (NXX.EQ.0)                GO TO 21
      IF (MOPTIO(17).EQ.0)  THEN
          NLINE = NLINE + 1
          WRITE (IQPRNT,9023) NLINE,LINE(1:NXX)
        ENDIF
 9023 FORMAT (1X,I6,' - ',A)

      IF (LINE(1:1).EQ.'#')        GO TO 21

C--           get the logical stream  type:n

      IFL = 0
      JNA = 0
   24 JNA = ICNEXT (LINE,JNA+1,NXX)
      IF (JNA.GE.7)                GO TO 41
      IF (LINE(JNA:JNA).EQ.'+')  THEN
          IFL = 7
          GO TO 24
        ENDIF
      IF (NDSLAT.LT.2)             GO TO 71
      JTE = NESLAT

      CALL SMCODE (LINE(JNA:JTE-1))
      IF (JXTYP.EQ.0)              GO TO 71

      IF (IFL.EQ.0)  CALL SMCIL (-1, ' ', ' ')

      JNA = ICFNBL (LINE,JTE,NXX)
      IF (JNA.GT.NXX)              GO TO 21

C--           register the new compiler instruction line

   41 IF (JXTYP.EQ.0)              GO TO 71
      CALL SMCIL (0, ' ', LINE(JNA:NXX))
      GO TO 21

C----         Done

   49 IF (NIGN.NE.0)               GO TO 97
      IF (NLINE.NE.0)  WRITE (IQPRNT,9023)
      RETURN

C----         Fault

   71 NIGN = NIGN + 1
      WRITE (IQPRNT,9071) LINE(1:NXX)
 9071 FORMAT (/' Bad: ',A
     F/' ***!!!  Faulty compiler-instruction  !!!***')

      IF (NIGN.LT.8)               GO TO 21
   97 IQREAD = -1
      RETURN
      END
