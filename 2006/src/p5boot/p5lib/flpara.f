CDECK  ID>, FLPARA.
      SUBROUTINE FLPARA (NFILEP,NAMEIP,CHTXOP)

C-    called from the Main programs to aquire the file names
C.    started 14-oct-91

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      PARAMETER   (NSLINE=256, NSTXKR=192)
      CHARACTER    IDPROG*8, LINECC*(NSLINE)
      CHARACTER    CHTERM*4, TXKR*(NSTXKR), TXKEY*8, CHWORK*4
      COMMON /FLKRAC/NLCUM, JLINC,NLINT, IXHOME, JFAULT,IFLLIT
     +,              IDPROG, LINECC, CHTERM, TXKR,TXKEY, CHWORK

      PARAMETER   (NCNAME=32)
          CHARACTER    NAMEIN(14)*(NCNAME)
          CHARACTER    KEYSTO(14)*8
          CHARACTER    COMD*256, CHSTRM*6
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
     +,    MPROTE(1000)
     +,    LUNUSE,LUNDES,LUNFLG,  MUSE(200)
     +,    NAMEIN, KEYSTO, COMD, CHSTRM
C--------------    End CDE              --------------------------------
      CHARACTER    CHTXOP*(*), NAMEIP(NFILEP)*(NCNAME)
      PARAMETER   (NWSTOR=3)
      LOGICAL      DIALOG, ALSO, READY
      PARAMETER   (NCOST=6, NCOEX=14)
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     +  'PATCHY  5.05 /3  1996/06/29 17.00   '
     +)



      NFILE = NFILEP
      DO 12  J=1,NFILE
      NAMEIN(J) = NAMEIP(J)
      IXLUN(J)  = 0
   12 CONTINUE
      CALL CLEFT (VIDQQ,1,0)

      JEOP = LEN(CHTXOP)
      JA   = ICNEXT (CHTXOP,1,JEOP)
      NPID = NDSLAT
      JA   = NESLAT
      JAOP = ICNEXT (CHTXOP,JA,JEOP)

      IDPROG = CHTXOP(1:NPID)

      COMD = IDPROG
      NCMD = NPID + 2
      COMD(NCMD:NCMD) = CQAPO
      CALL CUTOL (COMD(1:8))
      IF (COMD(1:8).NE.'nypatchy')  IFLAUX= 1

      CCKARD = ' '
      READY  = .FALSE.
      INIPRO = 0
      IFHELP = 0
      NOEJ   = 0
      NREAD  = 0

C--                Get the home directory

      SLLINE  = ' '
      CALL GETENVF ('HOME', SLLINE(1:128))
      N = NDSLAT
      IF (N.GT.0)  THEN
          IF (SLLINE(N:N).NE.'/') THEN
              N = N + 1
              SLLINE(N:N) = '/'
            ENDIF
        ENDIF
      IXHOME = NA_LONG (SLLINE(1:N+1))

C--                Is the user on-line ?  Yes if IQOFFL = 0

      DIALOG = IQOFFL.EQ.0

C--                No dialogue printing if .GO on program statement

      NARGS = IARGC()
      IF (NARGS.EQ.0)              GO TO 17
      IF (NARGS.GE.NFILE)          GO TO 16

      LINECC = ' '
      JARG = NARGS
      CALL GETARG (JARG,LINECC)
      NLINT = LENOCC (LINECC)
      IF (NLINT.LT.3)              GO TO 17
      CHWORK = LINECC(NLINT-2:NLINT)
      IF (CHWORK(1:1).EQ.':')  CHWORK(1:1)='.'
      CALL CUTOL (CHWORK)
      IF (CHWORK.NE.'.go ')        GO TO 17

   16 DIALOG = .FALSE.
      READY  = .TRUE.
   17 IF (DIALOG)  THEN
        WRITE (IQTYPE,9001) IDPROG(1:NPID)
        IF (JAOP.LT.JEOP)  WRITE (IQTYPE,9002) CHTXOP(JAOP:JEOP)
        WRITE (IQTYPE,9004) (NAMEIN(J)(9:NCOEX),J=1,NFILE)

        IF (NARGS.GT.0)  THEN
          WRITE (IQTYPE,9006) (NAMEIN(J)(1:NCOST),J=1,NFILE)
        ENDIF
       ENDIF

 9001 FORMAT (1X,A,' executing')
 9002 FORMAT (' Options  :     ',A)
 9004 FORMAT (' Default ext. : ',12A)
 9006 FORMAT (' Stream names : ',12A)
C--------          Aquire the program parameters

      CHTERM = ' '
      NLCUM  = 0
      IQRFD  = 0
      JUREAD = 0
      JUPRNT = 0
      NFILEX = 0
      JFILE  = 1
      JUSE   = 1

C----              Take parameters from the command line

      JARG  = 1
   31 IF (NLCUM.GT.0)  THEN
          COMD(NCMD+1:NCMD+NLCUM) = LINECC(1:NLCUM)
          CALL CSQMBL (COMD,1,NCMD+NLCUM)
          NCMD  = NDSLAT + 1
          NLCUM = 0
        ENDIF

      LINECC = ' '
      IF (JARG.GT.NARGS)           GO TO 34
      CALL GETARG (JARG,LINECC)
      NLINT = LENOCC (LINECC)
      JARG  = JARG + 1
      GO TO 38

C----              Take parameters from next input line

   34 IF (DIALOG)  THEN
          WRITE (IQTYPE,9006) (NAMEIN(J)(1:NCOST),J=JFILE,NFILE)
          IF (INIPRO.EQ.0)  CALL TMINIT (INIPRO)
          CALL TMPRO (' ny> ')
        ENDIF

      CALL TMREAD (NSLINE, LINECC, NLINT, ISTAT)
      NREAD = NREAD + 1
      IF (ISTAT.NE.0)              GO TO 91

   38 IF (NLINT.EQ.0)              GO TO 31

      JLINC = 0
      IF (JFILE.GT.NFILE)          GO TO 72

C----              Next parameter

   41 IF (JLINC.GT.NLINT)          GO TO 31
      CHSTRM =         NAMEIN(JFILE)(1:6)
      CHLIEX =         NAMEIN(JFILE)(9:16)
      LUNUSE = ICDECI (NAMEIN(JFILE),18,19)
      LUNDES = ICDECI (NAMEIN(JFILE),22,23)
      LUNFLG = ICDECI (NAMEIN(JFILE),26,27)
      IFLSCR = 0
      IF (CHLIEX.EQ.'.scr    ')  IFLSCR = 7

      CALL FLKRAK (0,LUNDES)
      IF (JFAULT.NE.0)  THEN
          WRITE (IQTYPE,9042) JFILE
          GO TO 49
        ENDIF

 9042 FORMAT (' Faulty parameter',I3)

      IF (TXKEY(1:4).EQ.'    ')    GO TO 31
      IF (TXKEY(1:8).EQ.'help    ')  THEN
          CALL HELPPR
          GO TO 31
        ENDIF

      IF (LUNUSE.GE.5)             GO TO 61
      GO TO (43,51,55,57), LUNUSE

C--                Reader input

   43 IF (IFHELP.NE.0)             GO TO 64
      IF (TXKEY(1:4).EQ.'-   ')    GO TO 44
      IF (TXKEY(1:4).NE.'tty ')    GO TO 45

   44 TXKEY     = '-   '
      TXKR(1:8) = 'tty     '
      IF (IQOFFL.EQ.0)   THEN
          IF (INIPRO.EQ.0)  CALL TMINIT (INIPRO)
        ENDIF
      IQREAD = IQTTIN
      LUNUSE = IQTTIN
      JUREAD = JUSE
      GO TO 64

   45 LUNUSE = IQREAD
      IQRTTY = 0
      IF (TXKEY(1:4).EQ.'eof ')    GO TO 47
      JUREAD = JUSE
      GO TO 64

   47 IF (LUNFLG.EQ.0)             GO TO 48
      IQREAD = 0
      LUNUSE = 0
      GO TO 64

   48 WRITE (IQTYPE,9048) IDPROG(1:NPID)
 9048 FORMAT (' ***!!! EOF not allowed with ',A)
   49 IF (IQOFFL.NE.0)       CALL EXITRC (2)
      DIALOG = .TRUE.
      JARG   = NARGS + 1
      GO TO 31

C--                Printer output

   51 IF (TXKEY(1:4).EQ.'-   ')    GO TO 52
      IF (TXKEY(1:4).NE.'tty ')    GO TO 54

   52 TXKEY     = '-   '
      TXKR(1:8) = 'tty     '
      IQPRNT = IQTYPE
      LUNUSE = IQTYPE
      GO TO 64

   54 JUPRNT = JUSE
      JFPRNT = JFILE
      LUNUSE = IQPRNT
      GO TO 64

C--                Option string

   55 IF (IXFLUN.EQ.0)             GO TO 57
      CALL NA_GET (IXFLUN, SLLINE,1)
      NOEJ   = INDEX (SLLINE(1:NDSLAT),'+')
      IFHELP = INDEX (SLLINE(1:NDSLAT),'H')
      IF (IFHELP.EQ.0)             GO TO 57
      CHTERM = '.go'
      IQREAD = 0
      JLINC  = 1
      READY  = .TRUE.

C--                CCH - string

   57 LUNUSE = 0
      GO TO 64

C--                Normal streams

   61 CONTINUE

C--   set scratch file
      IF (IFLSCR.NE.0)  TXKR = '-temp.scr'

C--                Store usage for final processing

   64 NLCUM = JLINC
      NTXT  = LENOCC (TXKR)
      IXLUN(JFILE) = IXFLUN

      KEYSTO(JFILE) = TXKR(1:8)
      CALL UCOPY (LUNUSE,MUSE(JUSE),NWSTOR)
      JUSE = JUSE + NWSTOR
      IF (.NOT.DIALOG)             GO TO 68

      IF (LUNUSE.EQ.0)  THEN
          WRITE (IQTYPE,9085) CHSTRM,TXKR(1:NTXT)
        ELSE
          WRITE (IQTYPE,9087) JFILE,CHSTRM,TXKR(1:NTXT)
        ENDIF

   68 JFILE = JFILE + 1
      IF (TXKEY(5:8).NE.' ...')  NFILEX = JFILE
      IF (JFILE.LE.NFILE)          GO TO 41

C----              Start execution ?

      IF (READY)                   GO TO 83
      IF (NLCUM.GT.0)  THEN
          COMD(NCMD+1:NCMD+NLCUM) = LINECC(1:NLCUM)
          CALL CSQMBL (COMD,1,NCMD+NLCUM)
          NCMD  = NDSLAT + 1
          NLCUM = 0
        ENDIF

      IF (CHTERM.NE.'    ')        GO TO 73
      IF (JARG.LE.NARGS)  THEN
          IF (JLINC.GE.NLINT)      GO TO 31
        ENDIF
   72 CALL FLKRAK (-1,11)

   73 COMD(NCMD+1:NCMD+4) = '.go' // CQAPO
      NCMD = NCMD + 4
      IF (IQOFFL.EQ.0)  WRITE (IQTYPE,9073) COMD(1:NCMD)
 9073 FORMAT (/'. ',A/)

      IF (CHTERM(2:3).EQ.'go')     GO TO 83
      CHTERM = ' '
      IF (INIPRO.EQ.0)  CALL TMINIT (INIPRO)
      CALL TMPRO (' Type  GO  or stop  ny> ')
      CALL TMREAD (4, CHTERM, NCH, ISTAT)
      IF (ISTAT.NE.0)              GO TO 91
      NREAD = NREAD + 1
      CALL CUTOL (CHTERM)
      IF (CHTERM.EQ.'.go ')        GO TO 83
      IF (CHTERM.NE.'go  ')            STOP

C--------          Final processing, OPEN standard input/output

   83 IF (JUPRNT.NE.0)  THEN
          CALL UCOPY (MUSE(JUPRNT),LUNUSE,NWSTOR)
          CALL FLINK  (LUNUSE, 4, IXLUN(JFPRNT), 0)
        ENDIF

      ALSO = .NOT.DIALOG
      IF (IQTYPE.EQ.IQPRNT)  THEN
          ALSO = .FALSE.
          NOEJ = 7
        ENDIF

      IF (NOEJ.EQ.0)  THEN
          WRITE (IQPRNT,9083) IDPROG(1:NPID)
        ELSE
          WRITE (IQPRNT,9084) IDPROG(1:NPID)
        ENDIF
      NQUSED = 3

      IF (ALSO)  WRITE (IQTYPE,9084) IDPROG(1:NPID)
      IF (NFILEX.EQ.NFILE)  NFILEX = NFILE + 1

      JUSE  = 1
      JFILE = 1
   84 CALL UCOPY (MUSE(JUSE),LUNUSE,NWSTOR)
      CHLIFI = ' '
      IXFLUN = IXLUN(JFILE)
      IF (IXFLUN.NE.0)  THEN
          CALL NA_GET (IXFLUN, CHLIFI, 1)
          NLIFI = NDSLAT
        ELSE
          CHLIFI(1:8) = KEYSTO(JFILE)
          NLIFI = 8
          IF (JFILE.NE.NFILEX)  THEN
              IF (CHLIFI(5:8).EQ.' ...')  NLIFI = 4
            ENDIF
          NLIFI = LNBLNK(CHLIFI(1:NLIFI))
        ENDIF
      CHSTRM = NAMEIN(JFILE)(1:8)
      NQUSED = NQUSED + 1

      IF (LUNUSE.EQ.0)  THEN
          WRITE (IQPRNT,9085) CHSTRM,CHLIFI(1:NLIFI)
          IF (ALSO)  WRITE (IQTYPE,9085) CHSTRM,CHLIFI(1:NLIFI)
          GO TO 89
        ENDIF

      WRITE (IQPRNT,9087) JFILE,CHSTRM,CHLIFI(1:NLIFI)
      IF (ALSO)  WRITE (IQTYPE,9087) JFILE,CHSTRM,CHLIFI(1:NLIFI)

C--           handle the cradle input stream

      IF   (JUSE.NE.JUREAD)        GO TO 89
      IF (LUNUSE.NE.IQTTIN)        GO TO 87
C--       get the size of the here-document

      IF (IQOFFL.EQ.0)             GO TO 89
      CALL CITELL (IQRFD,JPOS,ISTAT)
      IF (ISTAT.NE.0)              GO TO 89
      CALL CISIZE (IQRFD,IQRSIZ,ISTAT)

      CALL CIREW (IQRFD)
      IQRRD = NREAD
      GO TO 89

C--       open the file for stdin, if given

   87 CALL FLINK (LUNUSE, 1, 0, LUNFLG)
      IF (IXFLUN.LT.0)  THEN
          IQREAD = 0
        ELSE
          IQRFD  = LUNFD
          IQRSIZ = LUNSIZ
        ENDIF

   89 IF (JFILE.EQ.NFILEX)  JFILE=NFILE
      JUSE  = JUSE  + NWSTOR
      JFILE = JFILE + 1
      IF (JFILE.LE.NFILE)          GO TO 84
      IF (ALSO)  WRITE (IQTYPE,9089)
      CHTERM = ' '

      WRITE (IQPRNT,9099) CQDATEM(3:),CQTIME
      NQUSED = NQUSED + 3
      NQLLBL = 1
      NQINIT = 0
      RETURN

   91 CALL P_KILLM ('FLPARA - Trouble reading stdin')

 9083 FORMAT (1H1,A,' executing with files / options'/)
 9084 FORMAT (/1X,A,' executing with files / options'/)
 9085 FORMAT (6X,A,3X,A)
 9087 FORMAT (I4,2X,A,3X,A)
 9089 FORMAT (1X)
 9099 FORMAT        (/' Version: ',
     +  'PATCHY  5.05 /3  1996/06/29 17.00     '
     F,'.RJP, today: ',A,1X,A
     F/)
      END
