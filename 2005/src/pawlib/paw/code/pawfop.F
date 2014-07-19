*
* $Id: pawfop.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfop.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/11 11/01/94  11.05.55  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWFOP
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        OPEN A FZ FILE INTERACTIVELY                            *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawlun.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
      CHARACTER*8 CHOPT,STAT
      DIMENSION IOPT(6)
      EQUIVALENCE (IOPT(1),IOPTA),(IOPT(2),IOPTX)
      EQUIVALENCE (IOPT(3),IOPTI),(IOPT(4),IOPTO)
      EQUIVALENCE (IOPT(5),IOPTY),(IOPT(6),IOPTL)
#if defined(CERNLIB_IBM)
      CHARACTER*2 CHLUN
      CHARACTER*5 CHRECL
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSFIL
      CHARACTER*4    IUNIT, ITRK, IRECFM
      CHARACTER*9    IACTIO
      LOGICAL*4      EXS, OPN

      INTEGER*4      ISPACE(3), IDCB(2)

      DATA           ISPACE / 10, 10, 0 /
      DATA           IDCB   / 0, 3600 /
      DATA           ITRK   / 'TRK' /, IRECFM / 'VBS' /
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      DATA           IUNIT  / 'HSM' /
#endif
#if defined(CERNLIB_NEWLIB)
      DATA           IUNIT  / 'FAST' /
#endif
C.
C.    ------------------------------------------------------------------
C.
      IQUEST(1)=0
      CALL KUGETI(LUN)
      CALL PALUNF(LUN,1,IFREE)
      IF(IFREE.NE.0)GO TO 99
      CALL KUGETF(CHTITL,NCH)
      CALL KUGETI(LRECL)
      CALL KUGETC(CHOPT,NCH)
      CALL UOPTC(CHOPT,'AXIOYL',IOPT)
#if !defined(CERNLIB_IBM)
      IF(IOPTL.NE.0)THEN
         MEDIUM=0
         NWREC=0
         NBUF=0
         CHFILE=CHTITL
         CALL CFOPEN(LUNPRT,MEDIUM,NWREC,'r',NBUF,CHFILE,ISTAT)
         IQUEST(1)=LUNPRT
         GO TO 90
      ENDIF
#endif
      IF(IOPTI.NE.0)THEN
         STAT='OLD'
      ELSE
         STAT='UNKNOWN'
      ENDIF
      IF(IOPTA.NE.0)THEN
         CALL KUOPEN(LUN,CHTITL,STAT,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CALL FZFILE(LUN,LRECL,CHOPT)
         GO TO 99
      ENDIF
      IF(IOPTX.EQ.0)THEN
         CHFILE='X'//CHOPT
         CHOPT=CHFILE
      ENDIF
      IF(IOPTY.EQ.0)THEN
         CHFILE='Y'//CHOPT
         CHOPT=CHFILE
      ENDIF
*
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
*     CALL CUTOL(CHTITL)
      OPEN(UNIT=LUN,FILE=CHTITL,FORM='UNFORMATTED',
#endif
#if defined(CERNLIB_APOLLO)
     +                                             RECL=4*LRECL,
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
     +     STATUS=STAT,IOSTAT=ISTAT)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      IF(IOPTI.NE.0)THEN
         CHFILE='STATE '//CHTITL
         DO 5 I=7,64
            IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
   5     CONTINUE
         CALL VMCMS(CHFILE,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
      ENDIF
      WRITE(CHLUN,'(I2)')LUN
      IF(CHLUN(1:1).EQ.' ')CHLUN(1:1)='0'
      CHFILE='FILEDEF IOFILE'//CHLUN//'  DISK '//CHTITL
      DO 10 I=17,64
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
  10  CONTINUE
      NCH=LENOCC(CHFILE)
      IF(LRECL.EQ.0)LRECL=900
      IRECL=4*LRECL
      WRITE(CHRECL,'(I5)')IRECL
      CHTITL=CHFILE(1:NCH)//' (RECFM U BLKSIZE '//CHRECL//' PERM'
      CALL VMCMS(CHTITL,ISTAT)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
*--       CONSTRUCT MVS FILE NAME
*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT
      CALL KPREFI ( PREFIX, NCHPRE )
      IF ( CHTITL(1:1) .EQ. '.' )       THEN
           NCH    = LEN(CHTITL)
           MVSFIL = CHTITL(2:NCH)
      ELSE
           MVSFIL = PREFIX(1:NCHPRE)//CHTITL
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      PRINT 8000,  MVSFIL(1:40), LUN
 8000 FORMAT ( ' PAWFOP - OPEN FILE ', A40, ' WITH STATUS '
     +,        'UNKNOWN ON LUN ', I3 )
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
      IBLKS = LRECL * 4
      IF ( IBLKS .LE. 0 )               IBLKS = IDCB(2)
*--       CHECK FILE EXISTENCE
      INQUIRE ( FILE='/'//MVSFIL, ERR=2, IOSTAT=ISTAT
     +,         EXIST=EXS, OPENED=OPN, NUMBER=LUNOLD )
      IF ( ISTAT .NE. 0 )               GO TO 4
      IF ( .NOT. EXS )                  THEN
*--        MVSFIL IS A NEW FILE
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
           PRINT *, ' PAWFOP - FILE ',MVSFIL, 'DOESN''T EXIST - CREATED'
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
*--        DEFINE FILE PARAMETERS
           CALL FILEINF ( ISTAT, 'DEVICE', IUNIT, ITRK, ISPACE(1)
     +,                   'SECOND', ISPACE(2), 'DIR', ISPACE(3)
     +,                   'RECFM', IRECFM, 'LRECL', IDCB(1)
     +,                   'BLKSIZE', IBLKS )
           IF ( ISTAT .NE. 0 )          GO TO 6
*--        OPEN FILE
           OPEN ( UNIT=LUN, ERR=8, STATUS='NEW'
     +,           FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,           FORM='UNFORMATTED', IOSTAT=ISTAT
     +,           ACTION='READWRITE' )
           IF ( ISTAT .NE. 0 )          GO TO 14
      ELSE
*--        MVSFIL IS AN OLD FILE
*--        IF PREVIOUSLY OPENED, CLOSE IT
           IF ( OPN )
     +          CLOSE ( UNIT=LUNOLD, ERR=10, STATUS='KEEP', IOSTAT=IRC )
*--        OPEN FILE IN READ/WRITE MODE, READ MODE ONLY IF FILE
*--            NAME START WITH A DOT (FILE BELONGING TO AN OTHER USERID
*--            FROM AN OTHER GROUP - RACF PROTECTION)
           IF ( CHTITL(1:1) .EQ. '.'              .AND.
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
     +          CHTITL(2:5) .NE. PREFIX(1:4) )    THEN
#endif
#if defined(CERNLIB_NEWLIB)
     +          CHTITL(2:7) .NE. PREFIX(1:6) )    THEN
#endif
#if defined(CERNLIB_IBMMVS)
                IACTIO = 'READ'
           ELSE
                IACTIO = 'READWRITE'
           ENDIF
           OPEN ( UNIT=LUN, ERR=8, STATUS='OLD'
     +,           FILE='/'//MVSFIL, ACCESS='SEQUENTIAL'
     +,           FORM='UNFORMATTED', IOSTAT=ISTAT
     +,           ACTION=IACTIO )
           IF ( ISTAT .NE. 0 )          GO TO 14
      ENDIF
      GO TO 20

*--       ERROR MESSAGES

    2 PRINT *, ' PAWFOP - INQUIRE ERROR'
      ISTAT = -1
      GO TO 20
    4 PRINT *, ' PAWFOP - INQUIRE ERROR - IOSTAT = ', ISTAT
      GO TO 20
    6 PRINT *, ' PAWFOP - FILEINF ERROR - ISTAT = ', ISTAT
      GO TO 20
    8 PRINT *, ' PAWFOP - OPEN ERROR'
      ISTAT = -1
      GO TO 20
   10 PRINT *, ' PAWFOP - CLOSE ERROR'
      ISTAT = -1
      GO TO 20
   14 PRINT *, ' PAWFOP - OPEN ERROR - IOSTAT = ', ISTAT
   20 CONTINUE
#endif
#if defined(CERNLIB_VAX)
      OPEN(UNIT=LUN,FILE=CHTITL,FORM='UNFORMATTED',RECL=LRECL,
     +     BLOCKSIZE=4*LRECL,RECORDTYPE='FIXED',STATUS=STAT,
     +     SHARED,IOSTAT=ISTAT)
#endif
  90  IF(ISTAT.NE.0)THEN
         IQUEST(1)=1
         CALL HBUG('Cannot open file','PAWFOP',0)
         GO TO 99
      ENDIF
*
      LUNIT(LUN)=2-IOPTI
      CALL FZFILE(LUN,LRECL,CHOPT)
99    RETURN
      END
