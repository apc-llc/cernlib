*
* $Id: cdchti.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdchti.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCHTI (CVAL, IVAL, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCHTI (CVAL, IVAL*, IRC*)                              *
*                                                                      *
*   Converts character string into integer value                       *
*                                                                      *
*   This routine is taken from R.Brun                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CVAL     Character string (input)                                *
*     IVAL     Integer value (output)                                  *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCTOR, CDDCKH, CDDCKV, CDRDDA                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =100 : Error in decoding                               *
*                                                                      *
************************************************************************
*
      PARAMETER       (MXD=15)
      CHARACTER       CVAL*(*)
      CHARACTER*(MXD) CTEMP, BLANK
      INTEGER         IVAL
*
*     ------------------------------------------------------------------
*
      IRC = 0
      L   = LENOCC(CVAL)
      IF (L.EQ.0) GO TO 999
      IF (L.GT.MXD) THEN
        IRC = 100
        GO TO 999
      ENDIF
      BLANK = ' '
      IF (L.EQ.MXD) THEN
        CTEMP = CVAL
      ELSE
        CTEMP = BLANK(1:MXD-L)//CVAL
      ENDIF
      READ (CTEMP, '(I15)', ERR=100) IVAL
      GO TO 999
  100 IRC = 100
*                                                             END CDCHTI
  999 END
