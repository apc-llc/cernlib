*
* $Id: cdctob.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cdctob.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCTOB (CVAL, ZVAL, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCTOB (CVAL, ZVAL*, IRC*)                              *
*                                                                      *
*   Converts character string into hexadecimal bit string              *
*                                                                      *
*   This routine is taken from R.Brun                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CVAL     Character string (input)                                *
*     ZVAL     Hexadecimal bit string (output)                         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDCKH, CDDCKV, CDRDDA                                   *
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
      REAL*4          ZVAL
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
      READ (CTEMP, '(Z15)', ERR=100) ZVAL
      GO TO 999
  100 IRC = 100
*                                                             END CDCTOB
  999 END
