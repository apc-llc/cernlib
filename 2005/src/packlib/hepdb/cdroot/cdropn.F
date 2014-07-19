*
* $Id: cdropn.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdropn.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDROPN (LUN, CHOPT, CFNAM, NRECL, IOERR)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDROPN (LUN, CHOPT, CFNAM, NRECL, IOERR*)               *
*                                                                      *
*   Opens a random access file                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*     CHOPT    Character string with any of the following characters   *
*          O   Output file (default is input)                          *
*          Z   File to be overwritten if exists                        *
*     CFNAM    File name                                               *
*     NRECL    Record length in bytes                                  *
*     IOERR    Error code (0 if no error)                              *
*                                                                      *
*   Called by DBEXAMn                                                  *
*                                                                      *
************************************************************************
*
      DIMENSION       IOPT(2)
      CHARACTER       CHOPT*(*), CFNAM*(*), NAME*80, STATE*10
      EQUIVALENCE     (IOPTO, IOPT(1)), (IOPTZ, IOPT(2))
#if defined(CERNLIB_IBM)
      CHARACTER       CHACT*10
#endif
#if defined(CERNLIB_SGI)||defined(CERNLIB_DECS)
      PARAMETER       (JBYTES=4)
#endif
#if (defined(CERNLIB_WINNT))&&(!defined(CERNLIB_F2C))
      PARAMETER       (JBYTES=4)
#endif
#if (!defined(CERNLIB_SGI))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_WINNT)||defined(CERNLIB_F2C))
      PARAMETER       (JBYTES=1)
#endif
*
*     ------------------------------------------------------------------
*
* *** Check if Filename is meaningful
*
      NCH    = LENOCC (CFNAM)
      IF (NCH.LT.1)        GO TO 999
      NCHM   = MIN (50, NCH)
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB__P3CHILD))
      NAME   = '/'//CFNAM
#endif
#if defined(CERNLIB__P3CHILD)
      NAME   = CFNAM
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB__P3CHILD)
   10 L      = INDEX (NAME, '.')
      IF (L.NE.0) THEN
        NAME(L:L) = ' '
        GO TO 10
      ENDIF
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB__P3CHILD))
      NAME   = CFNAM
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB__P3CHILD))
      CALL CUTOL (NAME)
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Analyse the option
*
      CALL UOPTC  (CHOPT, 'OZ', IOPT)
      IF (IOPTZ.NE.0) IOPTO = 1
*
* *** Decide on the record length
*
      IF (NRECL.GT.0) THEN
        NREC   = NRECL
      ELSE
        NREC   = 4096
      ENDIF
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB__P3CHILD))
      NBLK   = NREC
      NREC   = NREC/4
#endif
#if !defined(CERNLIB__P3CHILD)
*
* *** Decide on state and form
*
      IF (IOPTO.EQ.0) THEN
#endif
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB__P3CHILD))
        STATE  = 'READONLY'
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB__P3CHILD))
        CHACT  = 'READ'
#endif
#if (!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB__P3CHILD))
        STATE  = 'OLD'
#endif
#if !defined(CERNLIB__P3CHILD)
      ELSE
        STATE  = 'UNKNOWN'
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB__P3CHILD))
        CHACT  = 'READWRITE'
#endif
#if !defined(CERNLIB__P3CHILD)
      ENDIF
*
* *** Now open the file
*
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB__P3CHILD))
      CALL FILEINF (IOERR, 'MAXREC', 2)
      IF (IOERR.NE.0)      GO TO 50
      IF (IOPTZ.NE.0) THEN
        OPEN (UNIT=LUN, FILE=NAME, FORM='UNFORMATTED', RECL=NREC,
     +        ACTION=CHACT, ACCESS='DIRECT', STATUS=STATE, IOSTAT=IOERR)
        IF (IOERR.NE.0)    GO TO 50
        CLOSE (LUN)
      ENDIF
      CALL FILEINF (IOERR, 'MAXREC', 16777215)
      IF (IOERR.NE.0)      GO TO 50
      OPEN (UNIT=LUN, FILE=NAME, FORM='UNFORMATTED', RECL=NREC,
     +      ACTION=CHACT, ACCESS='DIRECT', STATUS=STATE, IOSTAT=IOERR)
      IF (IOERR.NE.0)      GO TO 50
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB__P3CHILD))
      OPEN (UNIT=LUN, FILE=NAME, ACCESS='DIRECT', STATUS=STATE,
     +      RECL=NREC/JBYTES, FORM='UNFORMATTED', ERR=50, IOSTAT=IOERR)
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB__P3CHILD))
      IF (IOPTO.EQ.0) THEN
        OPEN (UNIT=LUN, FILE=NAME, ACCESS='DIRECT', STATUS=STATE,
     +        READONLY, SHARED, FORM='UNFORMATTED', ERR=50,
     +        IOSTAT=IOERR)
      ELSE
        OPEN (UNIT=LUN, FILE=NAME, ACCESS='DIRECT', STATUS=STATE,
     +        SHARED, RECL=NREC, FORM='UNFORMATTED', ERR=50,
     +        IOSTAT=IOERR)
      ENDIF
#endif
#if defined(CERNLIB__P3CHILD)
      CALL APOPNF (LUN, NAME, 'DIRECT', 'UNFORMATTED', 'F', NREC, 0,
     +             IOERR)
      IF (IOERR.NE.0)      GO TO 50
#endif
      IOPT(1) = LUN
      IOPT(2) = NREC
      CALL CDPRNT (6, '('' CDROPN : '//NAME(1:NCHM)//' opened on Unit'//
     +     ' '',I4,'' with RECL '',I8)', IOPT, 2)
      GO TO 999
*
   50 IOPT(1) = IOERR
      IOPT(2) = LUN
      CALL CDPRNT (6, '('' CDROPN Error : IOSTAT = '',I11,'' in openi'//
     +     'ng '//NAME(1:NCHM)//' on unit '',I4)', IOPT, 2)
*                                                             END CDROPN
  999 END
