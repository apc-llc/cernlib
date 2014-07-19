*
* $Id: cdopfl.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdopfl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPFL (LUN, CHNAM, STATE, ISTAT)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDOPFL (LUN, CHNAM, STATE, ISTAT*)                      *
*                                                                      *
*   Opens a sequential formatted file through a FOTRAN OPEN statement  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*     CHNAM    Character string specifying the file name               *
*     STATE    Status of the file                                      *
*     ISTAT    Error code for opening the file (0 if no error)         *
*                                                                      *
*   Called by CDACPL, CDAIWR, CDAUXI, CDEDAS, CDEDKY, CDPEEK, CDPRES,  *
*             CDRDIO                                                   *
*                                                                      *
************************************************************************
*
      CHARACTER      CHNAM*(*), STATE*(*)
#if defined(CERNLIB_IBM)
      CHARACTER      NAME*128
#endif
*
*     ------------------------------------------------------------------
*
* *** Open the file
*
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAX))
      OPEN (UNIT=LUN, FILE=CHNAM, ACCESS='SEQUENTIAL', STATUS=STATE,
     +      FORM='FORMATTED', IOSTAT=ISTAT, ERR=20)
#endif
#if defined(CERNLIB_IBM)
      NAME   = '/'//CHNAM
   10 L = INDEX (NAME, '.')
      IF (L.NE.0) THEN
        NAME(L:L) = ' '
        GO TO 10
      ENDIF
      IF (STATE.EQ.'OLD') THEN
        OPEN (UNIT=LUN, FILE=NAME, ACCESS='SEQUENTIAL', STATUS=STATE,
     +        ACTION='READ', FORM='FORMATTED', IOSTAT=ISTAT, ERR=20)
      ELSE
        OPEN (UNIT=LUN, FILE=NAME, ACCESS='SEQUENTIAL', STATUS=STATE,
     +        ACTION='WRITE', FORM='FORMATTED', IOSTAT=ISTAT, ERR=20)
      ENDIF
#endif
#if defined(CERNLIB_VAX)
      IF (STATE.EQ.'OLD') THEN
        OPEN (UNIT=LUN, FILE=CHNAM, ACCESS='SEQUENTIAL', STATUS=STATE,
     +        FORM='FORMATTED', READONLY, IOSTAT=ISTAT, ERR=20)
      ELSE
        OPEN (UNIT=LUN, FILE=CHNAM, ACCESS='SEQUENTIAL', STATUS=STATE,
     +        FORM='FORMATTED', IOSTAT=ISTAT, ERR=20)
      ENDIF
#endif
      ISTAT  = 0
   20 CONTINUE
*                                                             END CDOPFL
  999 END
