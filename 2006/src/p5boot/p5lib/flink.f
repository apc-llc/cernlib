CDECK  ID>, FLINK.
      SUBROUTINE FLINK (LUNP, LUNDES, IXNAME, IFLMEX)

C-    Open file (for unit LUNP)
C-        LUNDES = 1  cradle input
C-                 2  PAM input
C-                 4  printed output
C-                 5  Fortran output
C-                 6  ASM output write-only (T=ATT)
C-                 7  ASM input-output (T=MODIF)
C-                -1  delete the file, only
C-
C-        IXNAME = -1 use file name pointed to by IXFLUN
C-                  0 use file name as is in CHLIFI
C-                 >0 use file name pointed to by IXNAME
C-
C-        IFLMEX = 2  the input file does not need to exist
C-                 0  OPEN failure is fatal
C-
C-    set LUNFD and the file name into /FLINKC/
C.    started 14-oct-91

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
C--------------    End CDE              --------------------------------
      EQUIVALENCE (LUN,LUNOP), (NN,NLIFI)
      DIMENSION    INFO(40)
      EQUIVALENCE (INFO(1),NDSLAT)
      INTEGER      STATF
C!    LOGICAL      ACCESSF

      CHARACTER      UST*8
      CHARACTER*(*)  CSOLD, CSNEW, CSAPP, CSRWR
      CHARACTER*(*)  YFM, APX
      PARAMETER   (YFM='FORMATTED')
      PARAMETER   (APX='APPEND')
      PARAMETER   (CSOLD='OLD')
      PARAMETER   (CSNEW='NEW')
      PARAMETER   (CSAPP='UNKNOWN')
      PARAMETER   (CSRWR='UNKNOWN')
      LOGICAL      THERE


      LUN    = LUNP
      LUNFD  = LUN
      LUNSIZ = 0
      MODEFI = LUNDES
      IF (MODEFI.LT.0)  MODEFI= 99

      CALL FLNAME (IXNAME,IFLAPP)
      IXFLUN = MAX (IXFLUN,0)

      ISTAT = STATF (CHLIFI(1:NN),INFO)
      THERE = .FALSE.
      IF     (ISTAT.EQ.0)  THEN
          IF (INFO(7).NE.0)  THERE = .TRUE.
	ENDIF
      IF (THERE)  THEN
          LUNOLD = 1
        ELSE
          LUNOLD = 0
          IFLAPP = 0
        ENDIF

      IF (MODEFI.GE.4)             GO TO 41

C-----------       OPEN  cradle / PAM file       ------------

   21 IF (LUNOLD.EQ.0)             GO TO 36
      CALL CIOPEN (LUNFD, 'r', CHLIFI(1:NN), ISTAT)
      IF (ISTAT.NE.0)              GO TO 37

      CALL CISIZE (LUNFD, LUNSIZ, ISTAT)
      IF (ISTAT.NE.0)  LUNSIZ= 0
      CALL CIREW (LUNFD)

      GO TO 99

C----         OPEN failure

   36 CONTINUE
   37 IF (IFLMEX.EQ.2)  THEN
          WRITE (IQTYPE,9037)  CHLIFI(1:NN)
          GO TO 39
        ENDIF

   38 WRITE (IQTYPE,9038) CHLIFI(1:NN)
      IF (LUNOLD.NE.0)  CALL PERRORF (' System msg')
      IF (NQINIT.EQ.0)  THEN
          IF (IQPRNT.NE.IQTYPE)  WRITE (IQPRNT,9038) CHLIFI(1:NN)
        ENDIF
   39 IXFLUN = -1
      IF (IFLMEX.EQ.0)       CALL P_KILLM ('OPEN failure')
      RETURN

 9037 FORMAT (16X,'(file ',A,' does not exist)')
 9038 FORMAT (/' ***!!! OPEN fails for file: ',A)

C-----------       OPEN  output file             ------------

C--       MODEFI = 4 printer, 5 Fortran, 6 ASM write, 7 ASM read-write

   41 IF (MODEFI.EQ.7)  THEN
          IF (LUNOLD.EQ.0)  MODEFI= 6
        ELSE
          IF (LUNOLD.NE.0)  THEN
              IF (IFLAPP.EQ.0)  CALL UNLINKF (CHLIFI(1:NN))
            ENDIF
        ENDIF
      IF (MODEFI.GE.99)            GO TO 99

      IF (MODEFI.EQ.7)             GO TO 21
      IF (MODEFI.LE.5)             GO TO 51
      IF (MODEFI.EQ.7)  THEN
          UST = 'r+'
        ELSEIF (IFLAPP.NE.0)  THEN
          UST = 'a'
        ELSE
          UST = 'w'
        ENDIF

      CALL CIOPEN (LUNFD, UST(1:2), CHLIFI(1:NN), ISTAT)
      IF (ISTAT.NE.0)              GO TO 37

      IF (MODEFI.NE.7)             GO TO 99
      CALL CISIZE (LUNFD, LUNSIZ, ISTAT)
      IF (ISTAT.NE.0)  LUNSIZ= 0
      CALL CIREW (LUNFD)
      GO TO 99

C----         Fortran OPEN for output file

   51 IF (MODEFI.EQ.7)  THEN
          UST = CSRWR
        ELSEIF (IFLAPP.NE.0)  THEN
          UST = CSAPP
        ELSE
          UST = CSNEW
        ENDIF

      IF (IFLAPP.NE.0)  THEN
        OPEN (LUN,FILE=CHLIFI(1:NN),STATUS=UST,ERR=38,FORM=YFM
     +,                             ACCESS=APX)
       ELSE
        OPEN (LUN,FILE=CHLIFI(1:NN),STATUS=UST,ERR=38,FORM=YFM)
       ENDIF
   99 RETURN
      END
