*
* $Id: cdsopn.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdsopn.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSOPN (LUN, CHOPT, CFNAM, NRECL, IOERR)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSOPN (LUN, CHOPT, CFNAM, NRECL, IOERR*)               *
*                                                                      *
*   Opens a sequential FZ file                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*     CHOPT    Character string with any of the following characters   *
*          A   ASCII file format                                       *
*          O   Output file (default is input)                          *
*          X   Exchange binary format                                  *
*          Z   File to be overwritten if exists                        *
*     CFNAM    File name                                               *
*     NRECL    Record length in bytes                                  *
*     IOERR    Error code (0 if no error)                              *
*                                                                      *
*   Called by DBEXAMn                                                  *
*                                                                      *
************************************************************************
*
      DIMENSION       IOPT(5)
      CHARACTER       CHOPT*(*), CFNAM*(*), NAME*80, STATE*10, FORMT*12
      CHARACTER       CHOP*8
      EQUIVALENCE     (IOPTA, IOPT(1)), (IOPTI, IOPT(2))
     +              , (IOPTO, IOPT(3)), (IOPTX, IOPT(4))
     +              , (IOPTZ, IOPT(5))
      CHARACTER*80    CHFMAT
#if defined(CERNLIB_IBMVM)
      CHARACTER       FILE*96, CHLUN*2, CHREC*5, CHFIL*96
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
* *** Access the file name, etc.
*
      NCH    = LENOCC (CFNAM)
      IF (NCH.LT.1)          GO TO 999
      NCHM   = MIN (50, NCH)
      NAME   = CFNAM
#if defined(CERNLIB_IBMVM)||defined(CERNLIB__P3CHILD)
   10 L      = INDEX (NAME, '.')
      IF (L.NE.0) THEN
        NAME(L:L) = ' '
        GO TO 10
      ENDIF
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB__P3CHILD))
      CALL CUTOL (NAME)
#endif
*
* *** Analyse the option
*
      CALL UOPTC  (CHOPT, 'AIOXZ', IOPT)
*
* *** Decide on the record length
*
      IF (NRECL.GT.0) THEN
        NREC   = NRECL
      ELSE IF (IOPTX.NE.0) THEN
        NREC   = 3600
      ELSE IF (IOPTA.NE.0) THEN
        NREC   = 80
      ELSE
        NREC   = 10000
      ENDIF
      CHOP   = CHOPT
#if defined(CERNLIB_CRAY)
      LREC   = NREC/8
#endif
#if !defined(CERNLIB_CRAY)
      LREC   = NREC/4
#endif
*
* *** Decide on state and form
*
      IF (IOPTO.EQ.0) THEN
        STATE  = 'OLD'
      ELSE
#if defined(CERNLIB_VAX)
        IF (IOPTZ.EQ.0) THEN
          STATE  = 'NEW'
        ELSE
          STATE  = 'UNKNOWN'
        ENDIF
#endif
#if !defined(CERNLIB_VAX)
        STATE  = 'UNKNOWN'
#endif
      ENDIF
      IF (IOPTA.NE.0) THEN
        FORMT = 'FORMATTED'
      ELSE
        FORMT = 'UNFORMATTED'
      ENDIF
*
* *** Now open the file
*
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB__P3CHILD))
      WRITE (CHLUN, '(I2.2)') LUN
      WRITE (CHREC, '(I5)')   NREC
      IF (IOPTX.NE.0) THEN
        FILE   = 'FILEDEF IOFILE'//CHLUN//' DISK '//NAME
        NCH    = LENOCC (FILE)
        CHFIL  = FILE(1:NCH)//' (RECFM U BLKSIZE '//CHREC//' PERM'
        CHOP   = 'Y'//CHOPT
      ELSE
        FILE   = 'FILEDEF '//CHLUN//' DISK '//NAME
        NCH    = LENOCC (FILE)
        IF (IOPTA.NE.0) THEN
          CHFIL  = FILE(1:NCH)//' (RECFM FB BLKSIZE '//CHREC//' PERM'
        ELSE
          CHFIL  = FILE(1:NCH)//' (RECFM U BLKSIZE '//CHREC//' PERM'
        ENDIF
      ENDIF
      CALL VMCMS (CHFIL, IOERR)
      IF (IOERR.NE.0)        GO TO 50
      OPEN (UNIT=LUN, FORM=FORMT)
#endif
#if (defined(CERNLIB_APOLLO))&&(!defined(CERNLIB__P3CHILD))
      OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +      RECL=NREC/JBYTES, ERR=50, IOSTAT=IOERR)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))&&(!defined(CERNLIB__P3CHILD))
      OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +                        ERR=50, IOSTAT=IOERR)
#endif
#if (defined(CERNLIB_VAX))&&(!defined(CERNLIB__P3CHILD))
      IF (STATE.EQ.'OLD') THEN
        OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +        READONLY, SHARED, ERR=50, IOSTAT=IOERR)
      ELSE IF (IOPTX.NE.0) THEN
        OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +        RECORDTYPE='FIXED', RECL=LREC, BLOCKSIZE=NREC, ERR=50,
     +        IOSTAT=IOERR)
      ELSE
        OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS='UNKNOWN',
     +        RECL=NREC, ERR=50, IOSTAT=IOERR)
      ENDIF
#endif
#if defined(CERNLIB__P3CHILD)
      CALL APOPNF (LUN, NAME, 'SEQUENTIAL', FORMT, 'F', NREC, 0, IOERR)
      IF (IOERR.NE.0)        GO TO 50
#endif
      IOPT(1) = LUN
      IOPT(2) = NREC
 
      CHFMAT= ' ''('''' CDSOPN : '//NAME(1:NCHM)//' opened on Unit'//
     &        ' '''',I4,'''' with RECL '''',I8)'' '
      CALL CDPRNT (6, CHFMAT, IOPT, 2)
 
*     CALL CDPRNT (6, '('' CDSOPN : '//NAME(1:NCHM)//' opened on Unit'//
*    +     ' '',I4,'' with RECL '',I8)', IOPT, 2)
*
      CALL FZFILE (LUN, LREC, CHOP)
      GO TO 999
*
   50 IOPT(1) = IOERR
      IOPT(2) = LUN
      CHFMAT = ' ''('''' CDSOPN Error : IOSTAT = '''',I11,//
     +          '''' in opening '//NAME(1:NCHM)//' on unit '''',I4)'' '
      CALL CDPRNT (6, CHFMAT, IOPT, 2)
*                                                             END CDSOPN
  999 END
