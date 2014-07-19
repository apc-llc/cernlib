*
* $Id: kuopen.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kuopen.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.06/03 02/12/94  12.19.07  by  Gunter Folger
*-- Author :
      SUBROUTINE KUOPEN(LUN,FILE,CHSTAT,ISTAT)
*
********************************************************************************
*
* To open a FORMATTED file
*
*     Input :
*
      INTEGER LUN
      CHARACTER*(*) FILE
      CHARACTER*(*) CHSTAT
*
*     Output :
*
      INTEGER ISTAT
*
*     CHSTAT      action
*     =====       ======
*     'OLD'     : open existing file for readonly access
*                 error if file does not exist
*                 KUCLOS(LUN,'DELETE',ISTAT) is not allowed
*     'NEW'     : create new file and open for write access
*                 error if file already exists
*                 (except on VAX/VMS where a new cycle is created)
*     'UNKNOWN' : like 'NEW' but no error if file already exists
*     'APPEND'  : like 'UNKNOWN' but write pointer is positioned at EOF
*                 if file already exists
*     'DONTKNOW': use if it is not know whether the file will be read or
*                 written;  on VMS it will overwrite an existing cycle
*
*     'VERYOLD' : like 'OLD' but open with RECFM F on VM/CMS
*                 (if reading with formats is required)
*     'VERYNEW' : like 'UNKNOWN' but open with RECFM F on VM/CMS
*                 (if writing with formats is required)
*
********************************************************************************
*
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen1.inc"
*
*
      CHARACTER*20   STATUS
      CHARACTER*80   CHLINE
*
#if defined(CERNLIB_UNIX)||defined(CERNLIB_APOLLO)||defined(CERNLIB_CRAY)
      LOGICAL        KUQCAS
      CHARACTER*256  CHFILE
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*1    CHDISK, DISK, CMXDSK
      CHARACTER*8    CHNAME, CHTYPE
      CHARACTER*9    ACTION
      CHARACTER*11   FORM
      CHARACTER*80   CHFILE
      LOGICAL        FEXIST
      SAVE           DISK
      DATA           DISK   /' '/
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*20   STATU
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSFIL
      CHARACTER*5    IUNIT, ITRK, IRECFM
      CHARACTER*9    CACTIO
      LOGICAL*4      EXS, OPN
      INTEGER*4      IRC,LUNOLD,NCH

      INTEGER*4      ISPACE(3), IDCB(2)

      DATA           ISPACE / 10, 10, 0 /
      DATA           IDCB   / 80, 8000 /
      DATA           ITRK   / 'TRK' /, IRECFM / 'FB' /

*--       THE FOLLOWING DATAS ARE INSTALLATION DEPENDANT

#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))&&(!defined(CERNLIB_GSI))
      DATA           IUNIT  / 'HSM' /
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_GSI))
      DATA           IUNIT  / 'SYSDA' /
#endif
#if defined(CERNLIB_NEWLIB)
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSFIL
      CHARACTER*4    IUNIT, ITRK, IRECFM
      CHARACTER*9    CACTIO
      LOGICAL*4      EXS, OPN
      INTEGER*4      IRC

      INTEGER*4      ISPACE(3), IDCB(2)

      CHARACTER*70   RTAREA
      INTEGER        CMATOI

      DATA           ISPACE / 10, 10, 0 /
      DATA           IDCB   / 80, 9040 /
      DATA           ITRK   / 'TRK' /, IRECFM / 'FB' /

*--       THE FOLLOWING DATAS ARE INSTALLATION DEPENDANT
      DATA           IUNIT  / 'FAST' /
*
#endif
      STATUS=CHSTAT
      IF (CHSTAT.EQ.'APPEND')  STATUS = 'UNKNOWN'
      IF (CHSTAT.EQ.'VERYNEW') STATUS = 'UNKNOWN'
      IF (CHSTAT.EQ.'VERYOLD') STATUS = 'OLD'
      IF (CHSTAT.EQ.'DONTKNOW') STATUS = 'UNKNOWN'
#if defined(CERNLIB_IBMMVS)
      STATU=STATUS
      IF (STATUS.EQ.'OLD') STATU = 'READ'

#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_APOLLO)||defined(CERNLIB_CRAY)
      CHFILE = FILE
      IF (.NOT.KUQCAS()) CALL CUTOL(CHFILE)
      CALL KUHOME(CHFILE,NCH)
      OPEN(UNIT=LUN,FILE=CHFILE,STATUS=STATUS,IOSTAT=ISTAT,ERR=999)

#endif
#if defined(CERNLIB_VMS)
*--- allow multiple cycles (i.e. change status UNKNOWN ---> NEW)
      IF (STATUS.EQ.'UNKNOWN' .AND. CHSTAT.NE.'APPEND'
     +                        .AND. CHSTAT.NE.'DONTKNOW') STATUS = 'NEW'
      IF (STATUS.EQ.'OLD') THEN
        OPEN(UNIT=LUN,FILE=FILE,IOSTAT=ISTAT,ERR=999,
     +       CARRIAGECONTROL='LIST',STATUS='OLD',READONLY)
      ELSE
        OPEN(UNIT=LUN,FILE=FILE,IOSTAT=ISTAT,ERR=999,
     +       CARRIAGECONTROL='LIST',STATUS=STATUS)
      ENDIF

#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_SFS))
*--- courtesy Wojciech Wojcik
      CALL DMSCSL('DMSGETWU',IRET,IREASON,IWUID)
      CALL DMSCSL('DMSPUSWU',IRET,IREASON,IWUID)
#endif
#if defined(CERNLIB_IBMVM)
      IF (STATUS.EQ.'OLD') THEN
        CHDISK = '*'
      ELSE
        IF (DISK.EQ.' ') DISK = CMXDSK()
        CHDISK = DISK
      ENDIF
      CHFILE=FILE
      CALL CLTOU(CHFILE)
      L = LENOCC(CHFILE)
      I = INDEX(CHFILE(:L),'.')
      IF (I.EQ.0) I = INDEX(CHFILE(:L),' ')
      IF (I.EQ.0) THEN
        CHNAME = CHFILE
        CHTYPE = 'DAT'
      ELSE
        CHNAME = CHFILE(:I-1)
        CHFILE = CHFILE(I+1:)
        L = LENOCC(CHFILE)
        I = INDEX(CHFILE(:L),'.')
        IF (I.EQ.0) I = INDEX(CHFILE(:L),' ')
        IF (I.EQ.0) THEN
          CHTYPE = CHFILE
        ELSE
          CHTYPE = CHFILE(:I-1)
          CHDISK = CHFILE(I+1:)
        ENDIF
      ENDIF
      CHFILE = '/'//CHNAME//' '//CHTYPE//' '//CHDISK
      CALL FILEINF
      INQUIRE(FILE=CHFILE,EXIST=FEXIST)
      IF (STATUS.EQ.'OLD') THEN
        IF (.NOT.FEXIST) THEN
*
* If STATUS='OLD' must check if file exists
* (otherwise at the end we get an error number count > 0)
*
          ISTAT = 1
          GOTO 999
        ENDIF
      ELSEIF (STATUS.EQ.'NEW') THEN
        IF (FEXIST) THEN
          ISTAT = 1
          GOTO 999
        ENDIF
      ELSEIF (CHSTAT.EQ.'VERYNEW') THEN
        IF (FEXIST) THEN
*
* delete file in case the existing version is RECFM V
*
          CALL VMCMS('ERASE '//CHFILE(2:),ISTAT)
*
* continue because INQUIRE is sometimes lying
*
***       IF (ISTAT.NE.0) GOTO 999
        ENDIF
      ELSE
        CALL FILEINF(I,'RECFM','U','LRECL',80)
      ENDIF
      IF (STATUS.EQ.'OLD') THEN
        ACTION = 'READ'
      ELSE
        ACTION = 'READWRITE'
      ENDIF
*--- RECFM V files must be handled unformatted
      IF (CHSTAT.EQ.'OLD' .OR. CHSTAT.EQ.'APPEND') THEN
        FORM = 'UNFORMATTED'
      ELSE
        FORM = 'FORMATTED'
      ENDIF
      OPEN(UNIT=LUN,FILE=CHFILE,IOSTAT=ISTAT,ERR=999,
     +     STATUS=STATUS,ACTION=ACTION,FORM=FORM)

#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      CACTIO = 'READWRITE'
      IF ( Status .EQ. 'SCRATCH' ) GOTO 15

*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT

      CALL KPREFI ( PREFIX, NCHPRE )
      NCH    = LENOCC(FILE)
      IF ( FILE(1:1) .EQ. '.' )         THEN
           MVSFIL = FILE(2:NCH)
           NCH = NCH -1
      ELSE
           MVSFIL = PREFIX(1:NCHPRE)//FILE
           NCH = NCH + NCHPRE
      ENDIF
      PRINT 8000,  MVSFIL(1:NCH), STATUS, LUN
 8000 FORMAT ( '  KUOPEN - OPEN FILE ', A, ' WITH STATUS ', A8
     +,        ' ON LUN ', I3 )

      INQUIRE ( FILE='/'//MVSFIL(1:NCH), ERR=110, IOSTAT=ISTAT
     +,                  EXIST=EXS, OPENED=opn, NUMBER=lunold )
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      IF ( ISTAT .NE. 0 )      GO TO 120
*
      IF ( EXS ) THEN
         IF ( STATU .EQ. 'READ' ) THEN
            CACTIO = 'READ'
            STATUS = 'OLD'
         ENDIF
         IF ( STATU .EQ. 'NEW' ) THEN
*           GOTO 160
            STATUS = 'OLD'
*           PRINT *,'KUOPEN: Warning, '//MVSFIL(1:nch)//'already exists'
         ENDIF
      ELSE                ! not existing
*        IF ( STATUS .EQ. 'OLD' ) GOTO 180
         IF ( STATUS .EQ. 'OLD' ) STATUS = 'NEW'
*           e.g. LAST 0 LASTCMZ opens with status OLDA, but dataset may
*           not exist
         IF ( STATU .EQ. 'READ' ) GOTO 190
         IF ( STATUS .EQ. 'UNKNOWN' ) STATUS = 'NEW'
      ENDIF
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
 15   IF ( OPN ) THEN
         CALL KUCLOS(LUNOLD,' ',IRC)
         IF (IRC .NE. 0) GOTO 150
      ENDIF
*
* --- for a new/scratch dataset, FileInf is needed
*
      IF ( Status .EQ. 'NEW' .OR. Status .EQ. 'SCRATCH' ) THEN
         iSpace(3) = 0
         IF( INDEX(MVSFIL(1:NCH),'(') .NE. 0 ) ISPACE(3) = 28
         CALL FILEINF ( ISTAT, 'DEVICE', IUNIT, ITRK, ISPACE(1)
     +,                 'SECOND', ISPACE(2), 'DIR', ISPACE(3)
     +,                 'RECFM', IRECFM, 'LRECL', IDCB(1)
     +,                 'BLKSIZE', IDCB(2) )
         IF ( ISTAT .NE. 0 )          GO TO 130
      ENDIF
*
* --- no dataset name allowed for SCRATCH dataset
*
      IF ( Status .EQ. 'SCRATCH' ) THEN
         OPEN ( UNIT=LUN, ERR=140, STATUS=STATUS
     +,      ACCESS='SEQUENTIAL'
     +,      FORM='FORMATTED', IOSTAT=ISTAT
     +,      ACTION=CACTIO )
      ELSE
         OPEN ( UNIT=LUN, ERR=140, STATUS=STATUS
     +,      FILE='/'//MVSFIL(1:NCH), ACCESS='SEQUENTIAL'
     +,      FORM='FORMATTED', IOSTAT=ISTAT
     +,      ACTION=CACTIO )
      ENDIF
      IF ( ISTAT .NE. 0 )      GO TO 170
*
      IF ( STATUS .EQ. 'NEW' .OR. STATUS .EQ. 'SCRATCH' ) THEN
*--            INITIALIZE FILE CONTENT, TO AVOID PROBLEMS WHEN READING
*--            IF NOT PDS MEMBER
         IF ( INDEX(MVSFIL(1:NCH),'(') .EQ. 0 ) THEN
            ENDFILE LUN
            REWIND LUN
         ENDIF
         IF ( STATUS .EQ. 'NEW' ) THEN
           PRINT *, 'KUOPEN: File ', MVSFIL(1:NCH),' created'
         ENDIF
         IF ( STATUS .EQ. 'SCRATCH' ) THEN
           PRINT *, 'KUOPEN: Scratch File created on LUN ',LUN
         ENDIF
      ENDIF
      GO TO 900

*--       ERROR MESSAGES

  110 PRINT *, ' KUOPEN - INQUIRE ERROR'
      ISTAT = -1
      GO TO 999
  120 PRINT *, ' KUOPEN - INQUIRE ERROR - IOSTAT = ', ISTAT
      GO TO 999
  130 PRINT *, ' KUOPEN - FILEINF ERROR - IOSTAT = ', ISTAT
      GO TO 999
  140 PRINT *, ' KUOPEN - OPEN ERROR'
      ISTAT = -1
      GO TO 999
  150 PRINT *, ' KUOPEN - CLOSE ERROR'
      ISTAT = -1
      GO TO 999
  160 PRINT *, ' KUOPEN - ERROR: FILE DECLARED ''NEW'' PRE-EXISTS'
      ISTAT = -1
      GO TO 999
  170 PRINT *, ' KUOPEN - OPEN ERROR - IOSTAT = ', ISTAT
      GO TO 999
  180 PRINT *, ' KUOPEN - ERROR: FILE ', MVSFIL(1:NCH)
     +,' DOESN''T EXIST - BUT WAS TRIED TO BE OPENED WITH STATUS OLD'
      ISTAT = -1
      GO TO 999
  190 PRINT *, ' KUOPEN - ERROR: FILE ', MVSFIL(1:NCH)
     +,' DOESN''T EXIST - BUT WAS TRIED TO BE OPENED WITH STATUS READ'
      ISTAT = -1
      GO TO 999

#endif
#if defined(CERNLIB_NEWLIB)

      NCH=LENOCC(FILE)
      IF (FILE(1:2) .EQ. 'FT'
     +    .AND. FILE(5:8) .EQ. 'F001'
     +    .AND. NCH .EQ. 8) THEN
         RTAREA=' '
         CALL DYNAM(0,'INFO',RTAREA,'DDN='//FILE(1:8)//';')
         IF (RTAREA(9:12).NE.'SYS9') THEN
           CALL DYNAM (0,
     +     'UNALLOC;','DDN='//FILE(1:8)//' UNALLOC;')
           CALL DYNAM (0,
     +     'ALLOC;','DDN='//FILE(1:8)//' BLKSIZE=9040
     +      LRECL=80 RECFM=FB TRK PRIM=50 SEC=20 DSORG=PS;')
         ENDIF
         GOTO 999
      ELSE
*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT

         IF ( FILE(1:1) .EQ. '.' )         THEN
              MVSFIL = FILE(2:NCH)
         ELSE
              CALL KPREFI ( PREFIX, NCHPRE )
              MVSFIL = PREFIX(1:NCHPRE)//FILE
         ENDIF
      ENDIF

      IF     ( STATUS .EQ. 'NEW' )      THEN
*--            CHECK FILE EXISTENCE
               INQUIRE ( FILE='/'//MVSFIL, ERR=110, IOSTAT=ISTAT
     +,                  EXIST=EXS)
               IF ( ISTAT .NE. 0 )      GO TO 120
               IF ( EXS )               GO TO 160
*--            DEFINE FILE PARAMETERS IF MVSFIL IS NOT A MEMBER OF A PDS
   10          IF ( INDEX(MVSFIL,'(') .NE. 0 )    GO TO 15
               CALL FILEINF ( ISTAT, 'DEVICE', IUNIT, ITRK, ISPACE(1)
     +,                       'SECOND', ISPACE(2), 'DIR', ISPACE(3)
     +,                       'RECFM', IRECFM, 'LRECL', IDCB(1)
     +,                       'BLKSIZE', IDCB(2) )
               IF ( ISTAT .NE. 0 )      GO TO 130
*--            OPEN FILE
   15          OPEN ( UNIT=LUN, ERR=140, STATUS='NEW'
     +,               FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,               FORM='FORMATTED', IOSTAT=ISTAT
     +,               ACTION='READWRITE' )
               IF ( ISTAT .NE. 0 )      GO TO 170
*--            INITIALIZE FILE CONTENT, TO AVOID PROBLEMS WHEN READING
*--            IF NOT PDS MEMBER
               IF ( INDEX(MVSFIL,'(') .EQ. 0 )    THEN
                    ENDFILE LUN
                    REWIND LUN
               ENDIF

      ELSEIF ( STATUS .EQ. 'OLD' .OR.
     +         STATUS .EQ. 'READ' .OR.
     +         STATUS .EQ. 'UNKNOWN' )  THEN
*--            CHECK FILE EXISTENCE
               INQUIRE ( FILE='/'//MVSFIL, ERR=110, IOSTAT=ISTAT
     +,                  EXIST=EXS, OPENED=OPN, NUMBER=LUNOLD )
               IF ( ISTAT .NE. 0 )      GO TO 120
               IF ( .NOT. EXS )         THEN
                  IF (STATUS .EQ. 'UNKNOWN' )  GO TO 10
                  PRINT *, ' KUOPEN - FILE ', MVSFIL
     +,                    'DOESN''T EXIST'
                  ISTAT=-1
                  GOTO 999
               ENDIF
*--            IF PREVIOUSLY OPENED, CLOSE IT
               IF ( OPN )
     +              CLOSE ( UNIT=LUNOLD, ERR=150, STATUS='KEEP'
     +,                     IOSTAT=IRC )
*--            OPEN FILE IN READ/WRITE MODE, READ MODE ONLY IF FILE
*--                 NAME START WITH A DOT (FILE BELONGING TO AN OTHER USERID
*--                 FROM AN OTHER GROUP - RACF PROTECTION)
               IF ((FILE(1:1) .EQ. '.'            .AND.
     +              FILE(2:7) .NE. PREFIX(1:6) )  .OR.
     +             (STATUS    .EQ. 'READ'      ) )  THEN
                    CACTIO = 'READ'
               ELSE
                    CACTIO = 'READWRITE'
               ENDIF
               OPEN ( UNIT=LUN, ERR=140, STATUS='OLD'
     +,               FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,               FORM='FORMATTED', IOSTAT=ISTAT
     +,               ACTION=CACTIO )
               IF ( ISTAT .NE. 0 )      GO TO 170

      ELSEIF ( STATUS .EQ. 'SCRATCH' )  THEN
*--            DEFINE FILE PARAMETERS
               CALL FILEINF ( ISTAT, 'DEVICE', IUNIT, ITRK, ISPACE(1)
     +,                       'SECOND', ISPACE(2), 'DIR', ISPACE(3)
     +,                       'RECFM', IRECFM, 'LRECL', IDCB(1)
     +,                       'BLKSIZE', IDCB(2) )
               IF ( ISTAT .NE. 0 )      GO TO 130
*--            OPEN FILE
               OPEN ( UNIT=LUN, ERR=140, STATUS='SCRATCH'
     +,               ACCESS='SEQUENTIAL'
     +,               FORM='FORMATTED', IOSTAT=ISTAT
     +,               ACTION='READWRITE' )
               IF ( ISTAT .NE. 0 )      GO TO 170

      ELSE
               PRINT *, ' KUOPEN - OPEN STATUS ', STATUS, ' UNFORESEEN'
               ISTAT = -1

      ENDIF
      GO TO 900

*--       ERROR MESSAGES

  110 PRINT *, ' KUOPEN - INQUIRE ERROR'
      ISTAT = -1
      GO TO 999
  120 PRINT *, ' KUOPEN - INQUIRE ERROR - IOSTAT = ', ISTAT
      GO TO 999
  130 PRINT *, ' KUOPEN - FILEINF ERROR - ISTAT = ', ISTAT
      GO TO 999
  140 PRINT *, ' KUOPEN - OPEN ERROR'
      ISTAT = -1
      GO TO 999
  150 PRINT *, ' KUOPEN - CLOSE ERROR'
      ISTAT = -1
      GO TO 999
  160 PRINT *, ' KUOPEN - ERROR: FILE DECLARED ''NEW'' PRE-EXISTS'
      ISTAT = -1
      GO TO 999
  170 PRINT *, ' KUOPEN - OPEN ERROR - IOSTAT = ', ISTAT
      GO TO 999

#endif

900   CONTINUE
      IF (CHSTAT.EQ.'APPEND') THEN
910      CONTINUE
*--- skip lines until end of file
            CALL KUREAD(LUN,CHLINE,NCH)
         IF(NCH.GE.0) GOTO 910
#if (defined(CERNLIB_IBMALL)||defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY))&&(!defined(CERNLIB_APOLLO))
         BACKSPACE(LUN)
#endif
      ENDIF

999   END
