CDECK  ID>, ARRIN.
      SUBROUTINE ARRIN

C-    Read / reposition current input file, using the C library,
C-    according to IN_DOX:
C-                = -1 close, 0 rewind, 1 reposition, 2 read
C.    started 10-feb-92

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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
      COMMON /ARRCOM/IN_LUN, IN_FD,  IN_FIO, IN_EOF
     +,              IN_DOX, IN_DO1, IN_DO2
C--------------    End CDE              --------------------------------

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      IN_FD = IQ(LARRV+2)
      IF (IN_DOX.LT.2)             GO TO 61

C----              Read

      JSLGO  = IQ(LQHOLD+1)
      JSLNEW = LQLEND(2)

      JSLA = LQLEND(2)
      NSLN = IN_DO1
      NTXN = IN_DO2
      JTXA = MLIAD(JSLA)

      CALL CIGET (IN_FD,TEXT(JTXA),NTXN,NTXR,IN_EOF)

      IF (IN_EOF.NE.0)             GO TO 41

C--       reading successful, get the current file position

      CALL CITELL (IN_FD,NCHPOS,ISTAT)

      IF (ISTAT.NE.0)  CALL P_KILLM ('CIO tell fails')

C--       construct the line directory

      JTXE  = JTXA + NTXR
      JSLE  = JSLA
      LIMIT = JSLA + NSLN

      DO 26  JTXU=JTXA,JTXE-1
      IF (ICHAR(TEXT(JTXU)) .EQ. NEWLN)  THEN
          JSLE = JSLE + 1
          MLIAD(JSLE) = JTXU + 1
          IF (JSLE.GT.LIMIT)       GO TO 31
        ENDIF
   26 CONTINUE
      IF (NTXR.EQ.NTXN)            GO TO 31

C--       file complete, make sure terminating NEWLINE present

      IF (MLIAD(JSLE).EQ.JTXE)     GO TO 34
      TEXT(JTXE) = CHAR(NEWLN)
      JTXE = JTXE + 1
      JSLE = JSLE + 1
      MLIAD(JSLE) = JTXE
      NCHPOS = NCHPOS + 1
      GO TO 34

C----     file too big, reposition to complete line

   31 JTXU = MLIAD(JSLE)
      N    = JTXE - JTXU
      IF (N.EQ.0)                  GO TO 34

      NCHPOS = NCHPOS - N
      CALL CISEEK (IN_FD,NCHPOS,ISTAT)

      IF (ISTAT.NE.0)  CALL P_KILLM ('CIO seek fails')

C--       is the end of the file in memory ?

   34 NSIZE = IQ(LARRV+6)
      IF (NSIZE.GT.0)  THEN
          IF (NCHPOS.GE.NSIZE)  IN_EOF= 1
        ENDIF

C--       if IQRRD lines have been read already by FLPARA

      IF (IQRRD.NE.0)  THEN
          JSLGO  = JSLGO + IQRRD
          JSLNEW = MAX (JSLGO,LQLEND(2))
          IQRRD  = 0
          IQ(LQHOLD+1) = JSLGO
          IF (JSLGO.GE.JSLE)  CALL P_CRASH ('bad start of cradle')
        ENDIF

C--       Done

      IQ(LARRV+7) = NCHPOS
      IQ(LARRV+8) = IN_EOF

   37 CALL NEXTDE (JSLNEW,JSLE,JSLF,JDKNEX,INCRAD)

      IF (JSLF.EQ.JSLGO)  THEN
      IF (JDKNEX.GT.0)    THEN
          JSLNEW = JSLF + 1
          JDKTYP = JDKNEX
          GO TO 37
        ENDIF
        ENDIF

      IF (JDKNEX.EQ.0)  THEN
          IF (IN_EOF.NE.0)  JDKNEX= 6
        ENDIF

      IQ(LQHOLD+2) = JSLE - JSLGO
      IQ(LQHOLD+3) = JSLF - JSLGO
      IQ(LQHOLD)   = IOR (IQ(LQHOLD), 1)

      IQ(LARRV+12) = IQ(LARRV+12) + (JSLE-LQLEND(2))
      LQLEND(2) = JSLE
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      RETURN

C----              EoF read, no material has come to memory

   41 IF (IN_EOF.NE.-1)  CALL P_KILLM ('CIO read fails')
      IQ(LARRV+8) = -1
      JDKNEX = 6
      IF (JSLNEW.EQ.JSLGO)  JDKTYP = 6
      RETURN

C----              Reposition the file

   61 IF (IN_DOX.NE.1)             GO TO 71
      JSLN   = IN_DO1
      JSLE   = LQLEND(2)
      NSLOV  = JSLE - JSLN
      NTKOV  = MLIAD(JSLE) - MLIAD(JSLN)
      NCHPOS = IQ(LARRV+7)

      NCHPOS = NCHPOS - NTKOV
      CALL CISEEK (IN_FD,NCHPOS,ISTAT)

      IF (ISTAT.NE.0)  CALL P_KILLM ('CIO seek fails')

      IQ(LARRV+7)  = NCHPOS
      IQ(LARRV+8)  = 0
      IQ(LARRV+12) = IQ(LARRV+12) - NSLOV
      LQLEND(2) = JSLN
      LQLSTA(3) = LQLEND(2)
      LQLEND(3) = LQLSTA(3)
      RETURN

C----              REWIND / CLOSE

   71 IF (IN_DOX.EQ.0)  THEN
          CALL CIREW (IN_FD)
        ELSE
          CALL CICLOS (IN_FD)
          IQ(LARRV+1) = 0
          IQ(LARRV+2) = 0
        ENDIF

      IF (IQ(LARRV+7).GE.0)  IQ(LARRV+7)= 0
      IQ(LARRV+5) = 0
      IQ(LARRV+8) = 0
      IQ(LARRV+9) = 0
      RETURN
      END
