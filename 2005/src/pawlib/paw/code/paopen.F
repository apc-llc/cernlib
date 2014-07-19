*
* $Id: paopen.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: paopen.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.11/03 03/06/91  22.50.31  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAOPEN(LUN,CHNAME,CHOPT,LREC,IERR)
*
*                To open a file CHFILE on unit LUN
*
#include "paw/pcchar.inc"
      CHARACTER*(*) CHNAME,CHOPT
      CHARACTER*8 STATUS
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPT(1),IOPTU),(IOPT(2),IOPTN)
      EQUIVALENCE (IOPT(3),IOPTO)
#if defined(CERNLIB_IBM)
      CHARACTER*3 CHLUN
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
*________________________________________________________
*
      CALL UOPTC(CHOPT,'UNO',IOPT)
      STATUS='UNKNOWN'
      IF(IOPTN.NE.0)STATUS='NEW'
      IF(IOPTO.NE.0)STATUS='OLD'
      LRECL=4*LREC
      IF(LRECL.EQ.0)LRECL=9764
#if !defined(CERNLIB_IBM)
      CHFILE=CHNAME
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      WRITE(CHLUN,'(I2)')LUN
      NCH=LENOCC(CHNAME)
      CHFILE='FILEDEF '//CHLUN//' DISK '//CHNAME(1:NCH)
      DO 10 I=1,64
         IF(CHFILE(I:I).EQ.'.')CHFILE(I:I)=' '
  10  CONTINUE
      NCH=LENOCC(CHFILE)
      CHTITL=' '
      WRITE(CHTITL,1000)CHFILE(1:NCH),LRECL
 1000 FORMAT(A,' (RECFM VBS LRECL',I5,' BLKSIZE 6232 PERM')
      CALL VMCMS(CHTITL,IERR)
      IF(IERR.EQ.0)THEN
         OPEN(UNIT=LUN,FILE='/'//CHFILE(18:NCH),STATUS='UNKNOWN',
     +        FORM='UNFORMATTED')
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))
*--       CONSTRUCT MVS FILE NAME
*--       DON'T ADD PREFIX IF THE FIRST CHARACTER OF FILE NAME IS A DOT
      CALL KPREFI ( PREFIX, NCHPRE )
      IF ( CHNAME(1:1) .EQ. '.' )       THEN
           NCH    = LEN(CHNAME)
           MVSFIL = CHNAME(2:NCH)
      ELSE
           MVSFIL = PREFIX(1:NCHPRE)//CHNAME
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      PRINT 8000,  MVSFIL(1:40), LUN
 8000 FORMAT ( ' PAOPEN - OPEN FILE ', A40, ' WITH STATUS '
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
           PRINT *, ' PAOPEN - FILE ',MVSFIL, 'DOESN''T EXIST - CREATED'
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
           IF ( CHNAME(1:1) .EQ. '.'              .AND.
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

    2 PRINT *, ' PAOPEN - INQUIRE ERROR'
      ISTAT = -1
      GO TO 20
    4 PRINT *, ' PAOPEN - INQUIRE ERROR - IOSTAT = ', ISTAT
      GO TO 20
    6 PRINT *, ' PAOPEN - FILEINF ERROR - ISTAT = ', ISTAT
      GO TO 20
    8 PRINT *, ' PAOPEN - OPEN ERROR'
      ISTAT = -1
      GO TO 20
   10 PRINT *, ' PAOPEN - CLOSE ERROR'
      ISTAT = -1
      GO TO 20
   14 PRINT *, ' PAOPEN - OPEN ERROR - IOSTAT = ', ISTAT
   20 CONTINUE
#endif
*
#if defined(CERNLIB_APOLLO)
      CALL CUTOL(CHFILE)
      OPEN(UNIT=LUN,FILE=CHFILE,FORM='UNFORMATTED',STATUS=STATUS,
     +     RECL=LRECL,IOSTAT=IERR)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
      CALL CUTOL(CHFILE)
      OPEN(UNIT=LUN,FILE=CHFILE,FORM='UNFORMATTED',STATUS=STATUS,
     +     IOSTAT=IERR)
#endif
*
      END
