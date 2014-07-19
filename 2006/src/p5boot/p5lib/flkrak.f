CDECK  ID>, FLKRAK.
      SUBROUTINE FLKRAK (IPARA,LUNDES)

C-    Krack next parameter
C-    IPARA -ve :  see whether .GO present
C-            0 :  called from FLPARA
C-          +ve :  called from the running program

C-        LUNDES = 1  cradle input
C-                 2  PAM input
C-                 4  printed output
C-                 5  Fortran output
C-                 6  ASM output write-only (T=ATT)
C-                 7  ASM input-output (T=MODIF)
C-                11  option parameter
C-                12  control-character substitution parameter
C.    started 14-oct-91

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      PARAMETER   (NSLINE=256, NSTXKR=192)
      CHARACTER    IDPROG*8, LINECC*(NSLINE)
      CHARACTER    CHTERM*4, TXKR*(NSTXKR), TXKEY*8, CHWORK*4
      COMMON /FLKRAC/NLCUM, JLINC,NLINT, IXHOME, JFAULT,IFLLIT
     +,              IDPROG, LINECC, CHTERM, TXKR,TXKEY, CHWORK
C--------------    End CDE              --------------------------------

      CHARACTER    TXDO*(NSTXKR)
      CHARACTER    COLCC(NSLINE)*1, COLDO(NSTXKR)*1
      EQUIVALENCE (COLCC,LINECC),  (COLDO,TXDO)

      CHARACTER    MPUBLI*(*)
      PARAMETER   (MPUBLI = '/cern/pro/src/car/')


      TXKR  = ' '
      JFAULT = 0
      IXFLUN = 0
      MODEFI = LUNDES

C--           Entry from running program

      IF (IPARA.LE.0)              GO TO 21
      LINECC = CCKARD
      JLINC = NCHCCD
      NLINT = NCHCCT
      IF (MODEFI.EQ.1)  THEN
          CHLIEX  = '.cra'
        ELSEIF (MODEFI.EQ.2)  THEN
          CHLIEX  = '.car'
        ENDIF
      GO TO 22

C--           Entry from FLPARA

   21 IF (CHTERM.NE.'    ')        GO TO 39
      NLCUM = JLINC

C--           No more parameters

   22 JLINC = ICNEXT (LINECC,JLINC+1,NLINT)
      IF (JLINC.GT.NLINT)             GO TO 77

C----              Do parameter substitution

      JENDU = NESLAT
      NCHU  = NDSLAT

      CALL CENVIR (LINECC(JLINC:),NCHU, TXDO,1,NSTXKR, 1)
      NCHU = NDSLAT
      IF (NFSLAT.NE.0)             GO TO 91
      TXDO(NCHU+1:NCHU+4) = '    '

C----              '-' for Skip next parameter

      IF (COLDO(1).NE.'-')         GO TO 35
      TXKR(1:1) = COLDO(1)

      IF (MODEFI.NE.12)            GO TO 77
      IF (COLDO(2).EQ.'-')         GO TO 77
      IF (COLDO(2).EQ.' ')         GO TO 77
      GO TO 41

C----     Skip Remaining parameters for ". ", ".go", ": ", ":go"
C--       (but not "../name" or ":name")
C--       exit for ".no"

   35 IF (COLDO(1).NE.'.')  THEN
          IF (COLDO(1).NE.':')     GO TO 41
        ENDIF
      CHWORK = TXDO(2:5)
      CALL CUTOL (CHWORK)
      IF (CHWORK.EQ.'go  ')        GO TO 38
      IF (CHWORK.EQ.'no  ')        STOP
      IF (COLDO(2).NE.' ')         GO TO 41

      CHWORK = '    '
   38 CHTERM = '.' // CHWORK(1:3)
      JLINC = NLCUM
   39 TXKR(1:8) = '-    ...'
      GO TO 77

C------            Check special parameters

   41 IFLLIT = 0
      IF (IPARA.EQ.-1)             GO TO 77
      TXKR(1:NCHU) = TXDO(1:NCHU)
      TXKEY = TXKR(1:8)
      CALL CUTOL (TXKEY)
      IF (NCHU.LE.4)  THEN
          IF (TXKEY(1:5).EQ.'help ')     GO TO 78
          IF (TXKEY(1:5).EQ.'tty  ')     GO TO 78
          IF (TXKEY(1:5).EQ.'eof  ')     GO TO 78
        ENDIF

C--           Handle literal option parameter opened by /:

      IF (MODEFI.LT.11)            GO TO 42
      IF (TXDO(1:2).EQ.'/:')  THEN
          IF (NCHU.LT.3)  THEN
              TXKR(1:4) = '-   '
              GO TO 76
            ENDIF
          TXKR(1:NCHU) = TXDO(3:NCHU) // '  '
          NCHU  = NCHU - 2
        ENDIF
      IF (MODEFI.EQ.11)  CALL CLTOU (TXKR(1:NCHU))
      NTXU = NCHU
      GO TO 69

C------            Analyse file name

   42 JDO  = 1
      NTXU = 0
      TXKR(1:NCHU) = ' '

C--           ignore prefix  = :

   43 IF (INDEX ('=:', COLDO(JDO)).NE.0)  THEN
          JDO = JDO + 1
          GO TO 43
        ENDIF

C--           check prefix  +

      IF (COLDO(JDO).EQ.'+')  THEN
          TXKR(1:1) = COLDO(JDO)
          JDO  = JDO + 1
          NTXU = 1
        ENDIF

C--           check prefix /:

      IF (TXDO(JDO:JDO+1).EQ.'/:')  THEN
          N   = NCHU+1 - JDO
          IF (N.LE.2)              GO TO 91
          TXKR(NTXU+1:NTXU+N) = TXDO(JDO:JDO+N-1)
          NTXU   = NTXU+ N
          IFLLIT = 1
          GO TO 67
        ENDIF

      NTXU = MAX (NTXU,1)

C--           is the file PUBLIC ?

      IF (TXDO(JDO:JDO+1).EQ.'_/')  THEN
          N  = LEN (MPUBLI)
          TXKR(NTXU+1:NTXU+N) = MPUBLI
          NTXU = NTXU  + N
          JDO  = JDO + 2
          GO TO 48
        ENDIF

C--           does the file name start with '~/' ?

C!      IF (TXDO(JDO:JDO+1).EQ.'~/')  THEN
C!        IF (IXHOME.GT.0)   THEN
C!          CALL NA_GET (IXHOME, TXKR, NTXU+1)
C!          NTXU = NESLAT - 1
C!          JDO  = JDO + 2
C!          GO TO 48
C!        ENDIF
C!        ENDIF

C--           is the file-name of the old form (X)/fname ?

      IF (COLDO(JDO).NE.'(')       GO TO 48
      JE = ICFIND (')', TXDO,JDO+2,NCHU)
      IF (NGSLAT.EQ.0)             GO TO 48
      N = JE - JDO - 1
      SLLINE(1:N+3) = '${' // TXDO(JDO+1:JDO+N) // '}'
      N = N + 3
      CALL CENVIR (SLLINE,N, TXKR,NTXU+1,NSTXKR, 0)
      IF (NFSLAT.NE.0)             GO TO 48
      NTXU = NTXU + NDSLAT
      JDO  = JE + 1
C--           copy the tail

   48 N = NCHU+1 - JDO
      IF (N.GT.0)  THEN
          TXKR(NTXU+1:NTXU+N) = TXDO(JDO:JDO+N-1)
          NTXU = NTXU + N
        ENDIF


C----            Look for Directory and Extension

      JFILN = 2
      J     = ICFILA ('/', TXKR,JFILN,NTXU)
      IF (NGSLAT.NE.0)  THEN
          IF (J.EQ.NTXU)           GO TO 66
          JFILN = J + 1
        ENDIF

      JEXT  = ICFILA ('.', TXKR,JFILN,NTXU)
      LEXT  = NTXU+1 - JEXT
      LFILN = JEXT   - JFILN

C--           do the File extension

      IF (LEXT.EQ.0)    THEN
          N = LNBLNK (CHLIEX)
          IF (N.EQ.0)              GO TO 66
          IF (CHLIEX(1:1).NE.'.')  THEN
              NTXU = NTXU + 1
              TXKR(NTXU:NTXU) = '.'
            ENDIF
          TXKR(NTXU+1:NTXU+N) = CHLIEX(1:N)
          NTXU = NTXU + N
        ELSEIF (LEXT.EQ.1)  THEN
          TXKR(NTXU:NTXU) = ' '
          NTXU = NTXU - 1
        ENDIF

   66 CALL CLEFT (TXKR,2,NTXU)
      NTXU   = NESLAT - 1
   67 IF (NTXU.GE.NSLIFI-4)        GO TO 91
      NLIFI  = NTXU
      CHLIFI = TXKR(1:NTXU)
   69 IXFLUN = NA_LONG (TXKR(1:NTXU))

C----              Done

   76 JLINC = JENDU
   77 TXKEY = TXKR(1:8)
      RETURN

   78 JLINC = JENDU
      RETURN

C----              Trouble

   91 JFAULT = 7
      IF (IPARA.LE.0)  THEN
          IF (IQOFFL.EQ.0)         RETURN
        ENDIF

      WRITE (IQPRNT,9092)
      IF (IQTYPE.NE.IQPRNT)  WRITE (IQTYPE,9092)
 9092 FORMAT (/' ***!!! Faulty file name syntax !!!***')
      CALL EXITRC (2)
      RETURN
      END
