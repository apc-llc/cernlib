CDECK  ID>, EXSHELL.
      SUBROUTINE EXSHELL

C-    Run nyshell
C.    started 20-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144,
     +  KM20=524288,  KM21=1048576,  KM22=2097152,  KM23=4194304,
     +  KM24=8388608, KM25=16777216, KM26=33554432, KM27=67108864,
     +  KM28=134217728, KM29=268435456, KM30=536870912, KM31=1073741824)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256,  KMJ=512,  KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19, KMT=KM20,
     +  KMU=KM21, KMV=KM22, KMW=KM23, KMX=KM24, KMY=KM25, KMZ=KM26)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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
      CHARACTER    CHFSH*64, CHFIN*128, CHNAME*32, CHDO*2
      PARAMETER   (MASK= KMA + KMB + KME + KMH + KMQ + KMS + KMU + KMV)

      IAND (IZV,IZW) = AND (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)


      CALL INIAUX (LASTSH)
      CALL KROPT (IXLUN(2),MASK)

      MOPTIO(2) = MAX (MOPTIO(1), MOPTIO(2))

      IXFILE = IXLUN(1)
      IF (IXFILE.LE.0)  IXFILE = NA_LONG (' f.e')
      CALL FLSPLIT (IXFILE, IXSDIR, IXSFN, IXSEXT)

      CHNLOG = ' '
      CALL NA_GET (IXSFN,CHNLOG,1)
      NNLOG = NDSLAT

      CHFIN = ' '
      CALL NA_GET (IXFILE,CHFIN,1)
      NCHFIN = NDSLAT
      IF (MOPTIO(8).NE.0)  IXLUN(1) = -7

C----         Load the defaults

      CALL SMDEF

      CALL SMCIL (-1, 'start' , '# Script from nyshell for file '
     +                           // CHFIN(1:NCHFIN))
      CALL SMCIL (-1, 'end'   , '# End of the shell script')

C----         Load the compile-instructions from the cradle

      IF (IQREAD.NE.0)  CALL SMCINST
      IF (IQREAD.LT.0)  IXLUN(1)= -1

C----         Pre-delete the script  yyy.shfca

      CHFSH  = CHNLOG(1:NNLOG) // '.shfca'
      NCHFSH = NNLOG + 6

      LUNSH = 21
      CHLIFI = CHFSH(1:NCHFSH)
      NLIFI  = NCHFSH
      CALL FLINK (LUNSH, -1, 0, 0)

C----         Print the Help information

      IF  (IXLUN(1).LE.0)  THEN
          IF (MOPTIO(19).EQ.0)  THEN
              WRITE (IQPRNT,9002)
              WRITE (IQPRNT,9003)
              WRITE (IQPRNT,9004)
              WRITE (IQPRNT,9005)
              WRITE (IQPRNT,9006)
              WRITE (IQPRNT,9007)
            ENDIF
          MOPTIO(17) = 0
          MOPTIO(22) = 1
        ENDIF

C----         Print the run information

      WRITE (IQPRNT,9026) CHFIN(1:NCHFIN), CHFSH(1:NCHFSH)
     +,                                    CHNLOG(1:NNLOG)
 9026 FORMAT ('   Input file: ',A
     F/       ' Shell script:  ',A
     F/       '     Log file:  ',A,'.xqtlog')

      IF (MOPTIO(17).NE.0)         GO TO 29
      WRITE (IQPRNT,9027)
 9027 FORMAT (/' Actual set-up used:')
      CALL SMDISP (4)
      DO 28 JTYP=1,3
      CALL SMDISP (JTYP)
   28 CONTINUE
      CALL SMDISP (5)
      CALL SMDISP (6)
   29 IF  (IXLUN(1).LE.0)          GO TO 81

C----         Load the .xqtlog and .log files with the routine names

      CALL SMJOPT
      IF (MOPTIO(21).NE.0)  THEN
          IF (MOPTIO(2).EQ.0)  CALL SMRXQT
        ENDIF
      CALL SMRLOG

C----         Delete all .o files which are not part of the program

      IF (MOPTIO(21).NE.0)  CALL SHUPTO

C------       Write the shell script      ------------------

C--       open the  .shfca file

      CHLIFI = CHFSH(1:NCHFSH)
      NLIFI  = NCHFSH
      CALL FLINK (LUNSH, 5, 0, 0)

C--       write the script header

      CALL SHWSTA (1)
      IF (NRALLC.EQ.0)             GO TO 49

C--       write the compile instructions for each file

C-        NNAME(JR) = ((IXNAME*8 + JXTYP)*8 + JXSTR)*4 + IFLNEW
C-                IFLNEW has bit 2 set if .o file exists
C-                           bit 1 set if recompilation forced
      DO  47  JRT=NRXQT+1,NROUT
      JJ   = NNAME(JRT)
      IF (IAND(JJ,3).EQ.2)         GO TO 47
      JJ   = ISHFTR(JJ,2)
      JSTR = IAND (JJ,7)
      JJ   = ISHFTR (JJ,3)
      JTYP = IAND (JJ,7)
      IXNA = ISHFTR (JJ,3)

      CALL NA_GET (IXNA, CHNAME, 1)
      NN = NDSLAT
      CALL SHWCOMP (JTYP,JSTR, CHNAME(1:NN))
   47 CONTINUE

C--       write the apreslude

      CALL SHWSTA (2)

   49 CLOSE (LUNSH)
      CALL SYSTEMF ('chmod 755 *.shfca')
C----         Print the summary

      WRITE (IQPRNT,9053)
      IF (NRALLC.EQ.0)             GO TO 57

      CHDO = 'Do'
      JA = 4
      DO 54  JT=1,3
      DO 53  JS=1,4
      N = NCOUNTR(JS,JT)
      IF (N.EQ.0)                  GO TO 53
      CHNAME(1:8) = ' '
      CALL SMSID (JT,JS,CHNAME,1)
      WRITE (IQPRNT,9053) CHDO,N,CHNAME(1:6)
 9053 FORMAT (6X,A,I5,' files for ',A)
      CHDO = '  '
   53 CONTINUE
   54 CONTINUE

C----         Write the new .xqtlog file

   57 CALL SHWXQT
      IF (NRALLC+NRDEL.EQ.0)       GO TO 94

C----         Done

      IF (MOPTIO(21).EQ.0)         RETURN
      IF (MOPTIO(22).EQ.0)  THEN
          IF (NRDEL.EQ.0)          RETURN
        ENDIF
      CALL NA_GET (IXOBJ, CHNAME,1)
      NC = NDSLAT
      WRITE (IQPRNT,9057) NRDEL, CHNAME(1:NC)
 9057 FORMAT (8X,I5,' orphan ',A,' files deleted')
      RETURN

 9002 FORMAT (' nyshell  x.log  options  read  print'/
     F/' translates the nypatchy output file x.log for SPLIT or MODIFY'
     F/' mode into the shell-script x.shfca to compile the wanted'
     F/' routines as specified by the options, and according to the'
     F/' compilation rules defined by the "set-up". Nyshell has a'
     F/' default set-up which can be modified by the user with control'
     F/' lines given on the "read" stream. Nyshell writes the script'
     F/' x.shfca and also the file x.xqtlog recording the state which'
     F/' should be achieved with this run, to be used in the next run.'/
     F/' Nyshell and the resulting script x.shfca must be run in the'
     F/' directory where the resulting .o files are to be.'/
     F/' "options": A  all - compile or re-compile all files'
     F/'            B  bypass the use of the input .xqtlog file'
     F/'            E  empty - bypass the .xqtlog file if it is empty'
     F/'            H  help - print this help information only'
     F/'            Q  quick - do not display the set-up information'
     F/'            U  uptodate - check that all .o files in the'
     F/'                          current directory are ready to be'
     F/'                          put into the last-version library'
     F/'            V  verbose - display the complete set-up.')
 9003 FORMAT (
     F ' "read":  name of the file with the user set-up commands,'
     F/'          tty  if standard input to be used,'
     F/'          eof  if default set-up to be used as is.'
     F/' "print"  printed output file.'/
     F/' The set of routines to be handled is defined by the log file.'
     F/' If the "A" option is given all routines are to be compiled.'
     F/' If the "U" option is not given only the routines which do not'
     F/' have the "same" flag on the log are to be compiled.'
     F/' If the "U" option is given nyshell will make sure that all'
     F/' the .o files are up-to-date: a routine does not need'
     F/' recompilation only if it meets all these criteria:'
     F/'   1) the "same" flag is present,'
     F/'   2) the compiler options are the same as last time,'
     F/'   3) a re-compilation ordered last time has been done,'
     F/'   4) the .o file is more recent than the .f file,'
     F/'   5) the .o file is more recent than any .h file called.'
     F/' Also: delete all .o files in the current directory whose'
     F/'       names to not appear in the log file.')
 9004 FORMAT (
     F/' Set-up commands, given on "read", may be used to modify'
     F/' the default set-up. The "tag", starting before column 7,'
     F/' identifies the set-up parameter to be changed.'
     F/' An empty tag signals a continuation line. The text after'
     F/' gives the new content.'
     F/' The commands which would generate the set-up used will be'
     F/' printed at the end, unless the Quick option has been given.'
     F/' These are the possible set-up commands:'//' for Fortran:'/
     F/' fo:1    options special to stream :1'
     F/' fo:2                    to stream :2'
     F/' fo:3                    to stream :3'
     F/' fo:4                    to stream :4'
     F/' fopt    compiler options common to all streams'
     F/' fc      the name of the Fortran compiler'
     F/' fort:1  command to compile a file for stream :1'
     F/' fort:2                            for stream :2'
     F/' fort:3                            for stream :3'
     F/' fort:4                            for stream :4')
 9005 FORMAT (/' for the C language:'/
     F/' co:1    options special to stream :1'
     F/' co:2                    to stream :2'
     F/' co:3       etc.'
     F/' copt    compiler options common to all streams'
     F/' cc      the name of the C compiler'
     F/' cc:1    command to compile a file for stream :1'
     F/' cc:2                              for stream :2'
     F/' cc:3       etc.'/
     F/' for the assembler:'/
     F/' ao:1    options special to stream 1'
     F/' ao:2       etc.'
     F/' aopt    assembler options common to all streams'
     F/' as      the name of the assembler'
     F/' as:1    command to assemble a file for stream :1'
     F/' as:2       etc.')
 9006 FORMAT (/' start and end of the shell-script:'/
     F/' start   shell commands ahead'
     F/' end     shell commands added at the end'/
     F/' where-abouts of the source files:'/
     F/' sdir    name, default is directory part of the input file'
     F/'         (normally this is empty, ie. the current w.dir.)')
 9007 FORMAT (
     F/' The special set-up parameter "by" for fort:i, cc:i, as:i'
     F/' requests not to compile the files of this stream.'
     F/' The special set-up parameter "=:1", for example, given'
     F/' for fort:i, cc:i, as:i requests the files of this stream'
     F/' to be processed as for stream :1.'
     F/' Given for fo:i, co:i, as:i it is a request to use for this'
     F/' stream the same options as for stream :1.'/
     F/' Nyshell converts the set-up parameters into shell parameters'
     F/' and puts their definitions at the beginning of the script.'/
     F/' Status returned by nyshell on Unix is =0 normally,'
     F/' =1 for zero routines to be recompiled, >1 for bad data.'/)

C----         Trouble

   81 IF  (IXLUN(1).EQ.0)  THEN
          WRITE (IQPRNT,9080)
        ELSEIF  (IXLUN(1).EQ.-1)  THEN
          WRITE (IQPRNT,9081)
        ENDIF
      CALL EXITRC (2)

 9080 FORMAT (/' ***!!! No file-name given, no execution !!!***')
 9081 FORMAT (/' ***!!! Errors on input, no execution !!!***')
 9094 FORMAT (/' **!! Zero routines to be compiled !!**')
 9095 FORMAT (/' **!! No routines to be compiled or deleted !!**')

   94 IF (MOPTIO(21).EQ.0)  THEN
          WRITE (IQPRNT,9094)
        ELSE
          WRITE (IQPRNT,9095)
        ENDIF
      CALL EXITRC (1)
      END
