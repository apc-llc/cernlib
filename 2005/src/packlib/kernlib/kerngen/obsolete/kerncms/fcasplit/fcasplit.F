*
* $Id: fcasplit.F,v 1.1.1.1 1996/02/15 17:52:00 mclareni Exp $
*
* $Log: fcasplit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:00  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      PROGRAM FCASPLIT
 
C CERN PROGLIB#         FCASPLIT        .VERSION KERNFOR  4.32  920229
C ORIG. 01/10/88  JZ
 
      PARAMETER   (NARADD = 0)
      CHARACTER    CHIDF*8, CHIDC*8, CHIDA*8, CHIDD*4
      CHARACTER    CHIDX*7, CHIDY*6, CHOVER*(*)
      CHARACTER    CHPOF*(*), CHPOC*(*), CHPOA*(*)
      CHARACTER    CHEXFOR*(*), CHEXCC*(*), CHEXAS*(*)
      CHARACTER    CHWHF*(*), CHWHC*(*), CHWHA*(*)
      CHARACTER    CHEXOBJ*(*)
 
      PARAMETER   (CHIDF = 'CDECK  I' )
      PARAMETER   (CHIDC = '/*DECK I' )
      PARAMETER   (CHIDX =  'DECK  I' )
      PARAMETER   (CHIDY =   'DECK I' )
      PARAMETER   (CHIDD =         'D>, ' )
 
      PARAMETER   (CHIDA = '|DECK  I' )
      PARAMETER   (CHOVER= 'UNKNOWN')
 
      PARAMETER   (CHPOF = '( OPT(2)')
      PARAMETER   (CHPOC = '( ALIAS($*) ')
      PARAMETER   (CHPOA = ' ')
 
      PARAMETER   (CHEXFOR = '.fortran ')
      PARAMETER   (CHEXCC  = '.c ')
      PARAMETER   (CHEXAS  = '.assemble ')
      PARAMETER   (CHEXOBJ = ' TEXT ')
 
      PARAMETER   (CHWHF = 'FORTVS2 ')
      PARAMETER   (CHWHC = 'EXEC CC ')
      PARAMETER   (CHWHA = 'HASM ')
 
      PARAMETER   (MLMKLN=100)
      PARAMETER   (MXLENG=128, MXMKLN=64)
      CHARACTER    CHHOLD*(MXLENG)
      CHARACTER    CHOPT(7)*(MXLENG)
      DIMENSION    NCHOPT(7)
      EQUIVALENCE (NOPF,NCHOPT(1))
      EQUIVALENCE (NOPC,NCHOPT(2))
      EQUIVALENCE (NOPA,NCHOPT(3))
 
      CHARACTER    CHCMP(3)*(MXLENG)
      DIMENSION    NCHCMP(3)
      EQUIVALENCE (CHCMP(1),CHOPT(4)), (NCHCMP(1),NCHOPT(4))
      EQUIVALENCE (NNF,NCHCMP(1))
      EQUIVALENCE (NNC,NCHCMP(2))
      EQUIVALENCE (NNA,NCHCMP(3))
 
      CHARACTER    CHFIN*(MXLENG)
      EQUIVALENCE (CHFIN,CHOPT(7)), (NCHFIN,NCHOPT(7))
 
      CHARACTER    CHFSH*(MXLENG), CHFMK*(MXLENG)
      CHARACTER    CHMKLN*(MXMKLN+4)
 
      PARAMETER   (MXCBUF=100)
      DIMENSION    LXCBUF(MXCBUF)
      CHARACTER*80 CHCBUF(MXCBUF)
 
      CHARACTER    CHLINE*80, CHNAME*80, CHTEXT*511
      CHARACTER    CHUSE*1
      CHARACTER*21 UXTOVM,FILNAM
      CHARACTER*2  OFM
      LOGICAL      LEXIST
 
      PARAMETER   (NKEYS=11)
      CHARACTER    CHKEYS(NKEYS)*4
      DATA CHKEYS  / '-noh', '-log'
     +,              '-fo ', '-co ', '-ao '
     +,              '-f  ', '-c  ', '-a  '
     +,              '+fo ', '+co ', '+ao ' /
 
 
      PRINT 9001
 9001 FORMAT (' FCASPLIT executing.')
 
 
 9002 FORMAT (
     F ' FCASPLIT     [-f  nmft] [-c  nmcc] [-a  nmas]   [-noh] [-log]'
     F/'              [+fo incf] [+co incc] [+ao incs]'
     F/'              [-fo optf] [-co optc] [-ao opts]'
     F/'     f.e  [fca_n] [optf      [optc      [opts]]]'/
     F/' splits file  f.e  having a mixture of Fortran / C / assembler'
     F/' routines into separate files n.f or n.c or n.s, "n" being the'
     F/' name of each routine, creating at the same time a Shell script'
     F/' y.shfca  and a Make file  y.mkfca  either of which can be used'
     F/' to compile all routines individually.'/
     F/' Defaults are defined in fcasplit for the names by which the'
     F/' compilers are called; with the -f, -c, -a options they could'
     F/' be changed.'/
     F/' Defaults are also defined for the options with which they are'
     F/' called; with the -fo, -co, -ao options they can be re-defined;'
     F/' with the +fo, +co, +ao options they can be incremented.'/
     F/' To be backward compatible the options can also be specified by'
     F/' the positional parameters after the file-name.')
 9003 FORMAT (1X
     F/' If the  -noh  option is given (or if the first parameter after'
     F/' the file-name is "fca_n") the identifying header line of each'
     F/' routine is not written out. If the -log option is given'
     F/' the name of each routine is printed on standard output.')
 9004 FORMAT (1X
     F/' Each routine must start with an identifying line :'
     F/'  "', A,  'D>, "      in cols.  1-12  for Fortran'
     F/'  "', A,  'D>, "      in cols.  1-12  for C'
     F/'  "', A,  'D>, "      in cols.  1-12  for assembler'
     F/'   "DECK  ID>, "      in cols.  2-12  or'
     F/'    "DECK ID>, "      in cols.  3-12  for anything else'
     F/'              "name"  in cols. 13-40  gives the name'/
     F/'        In the last two cases, or if "name" contains an'
     F/'        extension,  the file created will be "name"'
     F/'        without extension .f, .c or .s added to it'
     F/'        and without an entry into the script.'/
     F/'        A trailing blank terminates the name,'
     F/'        symbol . followed by blank also terminates,'
     F/'        symbols  ; < # !   all terminate,'
     F/'        symbol   */        also  terminates.'
     F/1X)
 
C------            Acquire the parameters
 
      NARGS = IARGC() + NARADD
 
      CHOPT(1) = CHPOF
      CHOPT(2) = CHPOC
      CHOPT(3) = CHPOA
 
      CHCMP(1) = CHWHF
      CHCMP(2) = CHWHC
      CHCMP(3) = CHWHA
      CHFIN    = 'f.e'
 
      IFINFI = 0
      IFLNH  = 0
      IFLLOG = 0
 
C--       Get the keyed options, until the input file name
 
      OFM  ='A'
      JARG = 0
   11 JOPT = 0
   12 IF (JARG.GE.NARGS)           GO TO 21
      JARG = JARG + 1
      CALL GETARG (JARG, CHHOLD)
 
      IF(CHHOLD(1:5).EQ.'-mode') THEN
        JARG=JARG+1
        CALL GETARG(JARG,OFM)
        GOTO 11
      ENDIF
      IF (JOPT.EQ.0)               GO TO 13
 
C----     Store option values read
 
C--       positional trailing options
      IF (JOPT.LT.0)  THEN
          IF (JOPT.EQ.-1)  THEN
              IF (CHHOLD(1:5).EQ.'fca_n')  THEN
                  IFLNH = 7
                  GO TO 12
                ENDIF
            ENDIF
          CHOPT(-JOPT) = CHHOLD
          JOPT = JOPT - 1
          GO TO 12
        ENDIF
 
C--       keyed options
      IF (JOPT.LE.6)  THEN
          CHOPT(JOPT) = CHHOLD
          IF(CHHOLD(1:1).NE.'('.AND.JOPT.LE.3)
     +    CHOPT(JOPT) = '('//CHHOLD
        ELSE
                                  IPAR=1
          IF (CHHOLD(1:1).EQ.'(') IPAR=2
          IF (JOPT.EQ.7)  CHOPT(1) = CHPOF // ' ' // CHHOLD(IPAR:)
          IF (JOPT.EQ.8)  CHOPT(2) = CHPOC // ' ' // CHHOLD(IPAR:)
          IF (JOPT.EQ.9)  CHOPT(3) = CHPOA // ' ' // CHHOLD(IPAR:)
        ENDIF
      GO TO 11
 
C----     Analyse key, if any
 
C-         -noh -log  -fo -co -ao   -f  -c  -a  +fo +co +ao
C-            1    2    3   4   5    6   7   8    9  10  11
C-                      1   2   3    4   5   6    7   8   9
 
   13 DO 14  JKEY=1,NKEYS
      IF (CHHOLD(1:4).EQ.CHKEYS(JKEY))  THEN
          IF (JKEY.EQ.1)  THEN
              IFLNH = 7
              GO TO 11
            ENDIF
          IF (JKEY.EQ.2)  THEN
              IFLLOG = 7
              GO TO 11
            ENDIF
          JOPT = JKEY - 2
          GO TO 12
        ENDIF
   14 CONTINUE
 
C----     File-name read
 
      CHFIN = CHHOLD
      IFINFI = 7
 
C----     Get the positional options if given
 
      JOPT = -1
      GO TO 12
 
C------            Parameters have all been read
 
C--       Length of the option strings = true length + 2 (blanks)
 
   21 DO  24  JJ=1,7
      DO  23  J=MXLENG,1,-1
      IF (CHOPT(JJ)(J:J).NE.' ')     GO TO 24
   23 CONTINUE
      J = 0
   24 NCHOPT(JJ) = J + 2
      NCHFIN = NCHFIN - 2
 
C--       Derive the name of the script  yyy.shfca
C-        from the input file,  say  dir/yyy.ext
 
      N  = NCHFIN
      JA = 1
      JE = N
      J  = N
   26 IF (CHFIN(J:J).EQ.'/')  THEN
          JA = J + 1
          GO TO 27
        ELSEIF (CHFIN(J:J).EQ.'.')  THEN
          IF (JE.EQ.N)  JE = J - 1
        ENDIF
      J = J - 1
      IF (J.NE.0)                  GO TO 26
   27 CONTINUE
C---  Remove filetype if CHFIN=fn.ft.fm
      IDOT=INDEX(CHFIN(JA:JE),'.')
      IF(IDOT.GT.0) JE=IDOT-1
      CHFSH  = 'sh'//CHFIN(JA:JE) // '.exec'
      CHFMK  = 'mk'//CHFIN(JA:JE) // '.exec'
      NCHFSH = JE+8 - JA
 
      IF  (IFINFI.EQ.0)  THEN
          PRINT 9002
          PRINT 9003
          PRINT 9004, CHIDF, CHIDC, CHIDA
        ENDIF
 
      PRINT 9026, CHFIN(1:NCHFIN), CHFSH(1:NCHFSH), CHFMK(1:NCHFSH)
      PRINT 9027, CHCMP(1)(1:NNF),CHOPT(1)(1:NOPF)
      PRINT 9028, CHCMP(2)(1:NNC),CHOPT(2)(1:NOPC)
      PRINT 9029, CHCMP(3)(1:NNA),CHOPT(3)(1:NOPA)
 9026 FORMAT (5X,'        Input file : ',A/
     F        5X,'      Shell script : ',A/
     F        5X,'         Make file : ',A)
 9027 FORMAT (5X,'   Fortran    name : ',A/
     F        5X,'   Fortran options : ',A)
 9028 FORMAT (5X,'        CC    name : ',A/
     F        5X,'        CC options : ',A)
 9029 FORMAT (5X,' Assembler    name : ',A/
     F        5X,' Assembler options : ',A)
 
C----        Stop if no file-name, help information printed
 
      IF (IFINFI.EQ.0)  THEN
          PRINT 9030
          STOP
        ENDIF
 
 9030 FORMAT (' !!! No file-name given, no execution !!!')
 
C------------      Process the input file       ------------------
 
      FILNAM=UXTOVM(CHFIN(1:NCHFIN))
      LF=LENOCC(FILNAM)
      INQUIRE(FILE=FILNAM(1:LF),EXIST=LEXIST)
      IF (.NOT.LEXIST)  THEN
          PRINT 9031,FILNAM(1:LF)
          STOP
        ENDIF
 
 9031 FORMAT (' fcasplit: input file ',A,' not found ')
 
C--                Open input and .shfca file
      OPEN (11, FILE=FILNAM(1:LF),STATUS='OLD',ACTION='READ',
     +          FORM='UNFORMATTED')
      FILNAM=UXTOVM(CHFSH(1:NCHFSH)//'.'//OFM)
      LF=LENOCC(FILNAM)
      CALL FILEINF(I,'RECFM','U')
      OPEN (21, FILE=FILNAM(1:LF),STATUS=CHOVER)
      FILNAM=UXTOVM(CHFMK(1:NCHFSH)//'.'//OFM)
      LF=LENOCC(FILNAM)
      CALL FILEINF(I,'RECFM','U')
      OPEN (22, FILE=FILNAM(1:LF),STATUS=CHOVER)
      REWIND 11
      REWIND 21
      REWIND 22
      WRITE(21,*) '/* Compilation exec */'
      WRITE(21,*) '   Address Command'
C     WRITE(21,*) '   Trace C'
      WRITE(21,*) '   cc=0'
 
      CHMKLN = 'ROUTINES = '
      NXMKLN = 11
 
      NTEXT  = -12
      NIGNOR = 0
      NROUT  = 0
      NLINES = 0
      NTCBUF = 0
      NXCBUF = 0
 
      NLMKLN = 0
      NPCSMK = 0
 
      CALL CLTOU(CHFIN(JA:JE))
C-------           Read next line        -------------------------
 
   31 CONTINUE
      READ (11,NUM=N,END=81) CHLINE
C     N = LEN(CHLINE)
 
      DO  33  NCHLN=N,2,-1
      IF (CHLINE(NCHLN:NCHLN).NE.' ')  GO TO 34
   33 CONTINUE
      NCHLN = 1
 
   34 IF (NCHLN.GE.13)  THEN
          IF (CHLINE(9:12).EQ.CHIDD)   GO TO 59
        ENDIF
   35 IF (NTEXT.LT.0)              GO TO 58
   36 IF (JTYPE.NE.1)              GO TO 41
 
C--       Check new line is a Fortran comment line
 
      IF (CHLINE(1:1).EQ.'C')      GO TO 38
      IF (CHLINE(1:1).EQ.'c')      GO TO 38
      IF (CHLINE(1:1).EQ.'*')      GO TO 38
      IF (NCHLN.NE.1)              GO TO 41
      IF (CHLINE(1:1).NE.' ')      GO TO 41
 
   38 IF (NXCBUF.EQ.MXCBUF)        GO TO 41
      NXCBUF = NXCBUF + 1
      CHCBUF(NXCBUF)(1:NCHLN) = CHLINE(1:NCHLN)
      LXCBUF(NXCBUF) = NCHLN
      GO TO 31
 
C-------           Write next line       -------------------------
 
C--        NTEXT = <0  idle unheaded lines (not coming here)
C-                  0  normal running
C-                 >0  about to write the first line of new routine
C-                     if =1 : no entry to script and make file
 
C-        start a new routine only when the first true line is ready
 
   41 IF (NTEXT.EQ.0)              GO TO 46
      IF (NTEXT.EQ.1)              GO TO 44
 
C--       compilation command for this routine to the shell script
 
C--   Substitute $* by filename in command line
      INM=INDEX(CHTEXT(1:NTEXT),'$*')
      IF(INM.GT.0) THEN
C       CALL UCOPY2(CHTEXT(INM+2:),CHTEXT(INM+NPUT:),NTEXT-INM-1)
        DO 42 N=NTEXT+NPUT-2,INM+NPUT,-1
           CHTEXT(N:N)=CHTEXT(N-NPUT+2:N-NPUT+2)
  42    CONTINUE
        CHTEXT(INM:INM+NPUT-1)=CHNAME(1:NPUT)
        NTEXT=NTEXT+NPUT-2
      ENDIF
 
      WRITE (21,8000) 'cmd="'//CHTEXT(1:NTEXT)//'"'
      WRITE (21,8000) 'Say "     "cmd ; cmd ; If rc>cc Then cc=rc'
      WRITE (21,8000) 'If rc=0 Then Do'
      WRITE (21,8000) '   tfile="'//CHNAME(1:NPUT)//' TEXT A"'
      WRITE (21,8000)
     +      '"COPY" tfile "'//CHFIN(JA:JE)//' = = (APP" ;'
     +    //'"ERASE" tfile'
      WRITE (21,8000) 'End'
 
C--       register the routine name for the Make file
 
      CHTEXT =  CHNAME(1:NPUT) // CHEXOBJ // ' '
      NTEXT  =  NPUT + LEN(CHEXOBJ) + 1
      IF (NXMKLN+NTEXT.GT.MXMKLN) THEN
          NLMKLN = NLMKLN + 1
          IF (NLMKLN.LT.MLMKLN) THEN
              NXMKLN = NXMKLN + 1
              CHMKLN(NXMKLN:NXMKLN) = CHAR(92)
              WRITE (22,8000) CHMKLN(1:NXMKLN)
              NXMKLN = 0
            ELSE
              WRITE (22,8000) CHMKLN(1:NXMKLN)
              NXMKLN = 0
              NLMKLN = 0
              WRITE (22,9042)
              NPCSMK = NPCSMK + 1
              WRITE (CHMKLN,9043) NPCSMK
              NXMKLN = 11
            ENDIF
        ENDIF
 
 9042 FORMAT('#')
 9043 FORMAT('ROUTINE',I1,' = ')
 
      CHMKLN(NXMKLN+1:NXMKLN+NTEXT) = CHTEXT(1:NTEXT)
      NXMKLN = NXMKLN + NTEXT
 
C--       Open the output file if first line ready
   44 CONTINUE
      IF(INDEX(CHNAME(1:NCUM),'.assemble').EQ.0)
     +  CALL FILEINF(I,'RECFM','U')
      OPEN  (27, FILE=UXTOVM(CHNAME(1:NCUM)//'.'//OFM),STATUS=CHOVER)
      REWIND 27
 
      NTEXT = 0
      NROUT = NROUT + 1
      IF (IFLLOG.NE.0)  PRINT 9044, NROUT,CHNAME(1:NCUM)
 9044 FORMAT (' make',I4,1X,A)
 
C--       Transfer the comment lines from the buffer
 
   46 IF (NXCBUF.EQ.0)             GO TO 48
      DO  47  J=1,NXCBUF
      WRITE (27,8000) CHCBUF(J)(1:LXCBUF(J))
   47 CONTINUE
      NLINES = NLINES + NXCBUF
      NXCBUF = 0
 
C--       Transfer the current line
 
   48 WRITE (27,8000) CHLINE(1:NCHLN)
      NLINES = NLINES + 1
      GO TO 31
 
C--                Ignore leading unheaded lines
 
   58 NIGNOR = NIGNOR + 1
      IF (NTEXT.EQ.-1)             GO TO 31
      PRINT 9058, CHLINE(1:NCHLN)
      NTEXT = NTEXT + 1
      IF (NTEXT.NE.-1)             GO TO 31
      PRINT 9059
      GO TO 31
 9058 FORMAT (' ignored: ',A)
 9059 FORMAT (10X,'...')
 
C-------           Start new routine ?   -------------------------
 
   59 JTYNX = 0
      IF (CHLINE(1:8).EQ.CHIDF)    GO TO 61
      IF (CHLINE(1:8).EQ.CHIDC)    GO TO 62
      IF (CHLINE(1:8).EQ.CHIDA)    GO TO 63
      IF (CHLINE(2:8).EQ.CHIDX)    GO TO 60
      IF (CHLINE(3:8).EQ.CHIDY)    GO TO 60
      GO TO 35
 
C--                Yes, start new routine
 
   63 JTYNX = 1
   62 JTYNX = JTYNX + 1
   61 JTYNX = JTYNX + 1
   60 JPUT = 0
      JDOT = 0
      JDOP = 0
      NUS  = MIN(NCHLN,40)
 
C--       skip leading blanks before the name
 
      JGO = 13
   65 IF (CHLINE(JGO:JGO).EQ.' ')  THEN
          JGO = JGO + 1
          GO TO 65
        ENDIF
 
      IF (JGO.GT.NUS)              GO TO 35
 
C--       Convert name to lower case and find termination
 
      DO 66  JTK=JGO,NUS
      CHUSE = CHLINE(JTK:JTK)
      JV    = ICHAR(CHUSE)
      IF (CHUSE.EQ.' ')            GO TO 67
      IF (CHUSE.EQ.'.')  THEN
          IF (CHLINE(JTK+1:JTK+1).EQ.' ')   GO TO 67
          JDOP = JDOT
          JDOT = JPUT+1
        ENDIF
      IF (CHUSE.EQ.';')            GO TO 67
      IF (CHUSE.EQ.'<')            GO TO 67
      IF (CHUSE.EQ.'#')            GO TO 67
      IF (CHUSE.EQ.'!')            GO TO 67
      IF (CHUSE.EQ.'*')  THEN
          IF (CHLINE(JTK+1:JTK+1).EQ.'/')   GO TO 67
        ENDIF
C     IF (JV.LT.91)  THEN
C         IF (JV.GE.65)  CHUSE = CHAR(JV+32)
C       ENDIF
      JPUT = JPUT + 1
      CHNAME(JPUT:JPUT) = CHUSE
   66 CONTINUE
 
C--                Handle explicit extension
 
   67 JTYPE = JTYNX
      IF (JDOT.EQ.JPUT)  THEN
          JDOT = JDOP
          JPUT = JPUT - 1
        ENDIF
 
      NTEXT = 1
      NPUT  = JPUT
      NCUM  = NPUT
      IF (JDOT.NE.0)  JTYPE = -JTYPE
 
C----              Handle normal case with standard extensions
 
      IF (JTYPE.LE.0)              GO TO 80
 
C--       Fortran
 
      IF (JTYPE.NE.1)              GO TO 74
      N  =  LEN(CHEXFOR)
      CHNAME(NPUT+1:NPUT+N) = CHEXFOR
      NCUM   =  NPUT + N
      CHTEXT =  CHCMP(1)(1:NNF)//CHNAME(1:NPUT)//' '//CHOPT(1)(1:NOPF)
      NTEXT  =  NNF + NOPF + NPUT+1
      GO TO 80
 
C--       CC
 
   74 IF (JTYPE.NE.2)              GO TO 77
      N  =  LEN(CHEXCC)
      CHNAME(NPUT+1:NPUT+N) = CHEXCC
      NCUM   =  NPUT + N
      CHTEXT =  CHCMP(2)(1:NNC)//CHNAME(1:NPUT)//' '//CHOPT(2)(1:NOPC)
      NTEXT  =  NNC + NOPC + NPUT+1
      GO TO 80
 
C--       Assembler
 
   77 IF (JTYPE.NE.3)              GO TO 80
      N  =  LEN(CHEXAS)
      CHNAME(NPUT+1:NPUT+N) = CHEXAS
      NCUM   =  NPUT + N
      CHTEXT =  CHCMP(3)(1:NNA)//CHNAME(1:NPUT)//' '//CHOPT(3)(1:NOPA)
      NTEXT  =  NNA + NOPA + NPUT+1
 
   80 CLOSE (27)
      NTCBUF = NTCBUF + NXCBUF
      NXCBUF = 0
      IF (JTYPE.LT.1)              GO TO 31
      IF (IFLNH.NE.0)              GO TO 31
      GO TO 36
 
C-------           Done                  -------------------------
 
C----           Complete the Make file
 
   81 WRITE (22,8000) CHMKLN(1:NXMKLN-1)
 
C--       Fortran inference
 
      CHNAME = CHEXFOR // CHEXOBJ // ':'
      NPUT   = LEN(CHEXFOR) + LEN(CHEXOBJ) + 1
      CHTEXT = CHAR(9) // CHCMP(1)(1:NNF) // CHOPT(1)(1:NOPF) //
     +            '$*' // CHEXFOR
      NTEXT   = 3 + NNF + NOPF + LEN(CHEXFOR)
      WRITE (22,9083) CHNAME(1:NPUT), CHTEXT(1:NTEXT)
 
C--       CC inference
 
      CHNAME = CHEXCC // CHEXOBJ // ':'
      NPUT   = LEN(CHEXCC) + LEN(CHEXOBJ) + 1
      CHTEXT = CHAR(9) // CHCMP(2)(1:NNC) // CHOPT(2)(1:NOPC) //
     +            '$*' // CHEXCC
      NTEXT   = 3 + NNC + NOPC + LEN(CHEXCC)
      WRITE (22,9083) CHNAME(1:NPUT), CHTEXT(1:NTEXT)
 
C--       Assembler inference
 
      CHNAME = CHEXAS // CHEXOBJ // ':'
      NPUT   = LEN(CHEXAS) + LEN(CHEXOBJ) + 1
      CHTEXT = CHAR(9) // CHCMP(3)(1:NNA) // CHOPT(3)(1:NOPA) //
     +            '-o $*.o  $*' // CHEXAS
      NTEXT   = 12 + NNA + NOPA + LEN(CHEXAS)
      WRITE (22,9083) CHNAME(1:NPUT), CHTEXT(1:NTEXT)
 
      IF (NPCSMK.EQ.0) THEN
         WRITE (22,9084) CHFIN(JA:JE)
      ELSE
         WRITE (22,9085) CHFIN(JA:JE),(I,I=1,NPCSMK)
         WRITE (22,9086) (I,I,I=1,NPCSMK)
      ENDIF
 
      WRITE(21,*) '   Exit cc'
C--                Print summary
 
      PRINT 9087, NLINES,NROUT
      IF (NIGNOR.NE.0)  PRINT 9088, NIGNOR
      IF (NTCBUF.NE.0)  PRINT 9089, NTCBUF
 8000 FORMAT (A)
 9083 FORMAT (/A/A)
 9084 FORMAT(/A,': $(ROUTINES)',/)
 9085 FORMAT(/A,':',10(' rout',I1))
 9086 FORMAT(/,'rout: $(ROUTINES)',/,
     +        ('rout',I1,': $(ROUTINE',I1,')',/))
 9087 FORMAT (1X,I6,' lines written for',I6,' decks')
 9088 FORMAT (1X,I6,' leading unheaded lines ignored.')
 9089 FORMAT (1X,I6,' trailing comment lines ignored.')
      END
