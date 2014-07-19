*
* $Id: cdfopn.F,v 1.1.1.1 1996/02/28 16:24:17 mclareni Exp $
*
* $Log: cdfopn.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:17  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFOPN (LUN, IRC)
*     ============================
*
************************************************************************
*                                                                      *
*        SUBR. CDFOPN (LUN, IRC*)                                      *
*                                                                      *
*   Opens an file ascii file on the specified unit                     *
*   with time upto second embedded in it to ensure a unique filename   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number for opening the file                        *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDINIT, CDSTSV                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = -8 : Error in opening file                           *
*                                                                      *
************************************************************************
*
#include "hepdb/cduscm.inc"
      INTEGER         YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
      CHARACTER       CYEAR*2, CMON*2, CDAY*2, CHOUR*2
      CHARACTER       CMIN*2, CSEC*2, FNAME*80
#if defined(CERNLIB_IBMVM)
      CHARACTER*1     CHMODE,CMXDSK
#endif
      LOGICAL         EXIST
*
*     ------------------------------------------------------------------
*
#if defined(CERNLIB_IBMVM)
      CHMODE = CMXDSK()
#endif
*
* *** Get date and time for constructing the file name
*
   10 CALL CDUVTX (IDAY, ITIM)
      YEAR   = MOD (IDAY/10000, 100)
      MONTH  = MOD (IDAY/100  , 100)
      DAY    = MOD (IDAY      , 100)
      HOUR   = MOD (ITIM/3600 , 100)
      MINUTE = MOD (ITIM/60   ,  60)
      SECOND = MOD (ITIM      ,  60)
*
      WRITE (CYEAR, '(I2.2)') YEAR
      WRITE (CMON,  '(I2.2)') MONTH
      WRITE (CDAY,  '(I2.2)') DAY
      WRITE (CHOUR, '(I2.2)') HOUR
      WRITE (CMIN,  '(I2.2)') MINUTE
      WRITE (CSEC,  '(I2.2)') SECOND
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      FNAME = 'qq'//CYEAR//CMON//CDAY//'.t'//CHOUR//CMIN//CSEC
#endif
#if defined(CERNLIB_IBMVM)
      FNAME = '/QQ'//CYEAR//CMON//CDAY//' T'//CHOUR//CMIN//CSEC
     +        // ' ' // CHMODE
#endif
*
* ** If file already exists then go back to get a new time
*
      INQUIRE (FILE=FNAME, EXIST=EXIST)
      IF (EXIST) GO TO 10
*
* *** Open the file with an Open Statement
*
      OPEN (UNIT=LUN, ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED', STATUS ='NEW', IOSTAT=ISTAT,
#if defined(CERNLIB_IBMVM)
     +      ACTION='READWRITE',
#endif
     +      FILE  = FNAME)
      IF (ISTAT .NE. 0) THEN
#if defined(CERNLIB_IBMVM)
#endif
        IRC = -8
        IF(IDEBCD.GE.0) THEN
           WRITE(LPRTCD,9001) ISTAT,FNAME(1:LENOCC(FNAME)),LUN
 9001 FORMAT(' CDFOPN. error ',I10,' opening journal file ',A,
     +       ' on unit ',I2)
           WRITE(LPRTCD,9002)
 9002 FORMAT(' CDFOPN. check quota, file permissions (and tokens',
     +       ' for AFS/DFS file systems)')
         ENDIF
      ELSE
        IF(IDEBCD.GE.1) WRITE(LPRTCD,9003) FNAME(1:LENOCC(FNAME)),LUN
 9003 FORMAT(' CDFOPN. opened journal file ',A,' on unit ',I2)
        IRC = 0
      ENDIF
*                                                             END CDFOPN
      END
