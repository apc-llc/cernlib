*
* $Id: cdctor.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cdctor.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCTOR (CVAL, RVAL, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCTOR (CVAL, RVAL*, IRC*)                              *
*                                                                      *
*   Converts character string into real value                          *
*                                                                      *
*   This routine is taken from R.Brun                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CVAL     Character string (input)                                *
*     RVAL     Real value (output)                                     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDCKV, CDRDDA                                           *
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
      REAL            RVAL
*
*     ------------------------------------------------------------------
*
      IRC   = 0
      L     = LENOCC(CVAL)
      IF (L.EQ.0)       GO TO 999
      IF (L.GT.MXD)     GO TO 900
      BLANK = ' '
      IF (L.EQ.MXD) THEN
        CTEMP = CVAL
      ELSE
        CTEMP = BLANK(1:MXD-L)//CVAL
      ENDIF
*
      CALL CLTOU (CTEMP)
*
      I1    = INDEX (CTEMP, '.')
      II1   = 0
      IF (I1.LT.L) II1 = INDEX (CTEMP(I1+1:MXD), '.')
      I2    = INDEX (CTEMP, 'E')
      II2   = 0
      IF (I2.LT.L) II2 = INDEX (CTEMP(I2+1:MXD), 'E')
*
      IF (II1+II2.NE.0) GO TO 900
*
      IF (I1+I2.EQ.0) THEN
        CALL CDCHTI (CTEMP, IVAL, IRC)
        RVAL = IVAL
      ELSE IF (I1.EQ.L.AND.I2.EQ.0) THEN
        CALL CDCHTI (CTEMP(:I1-1), IVAL, IRC)
        RVAL = IVAL
      ELSE
        READ (CTEMP, '(G15.9)', END=999, ERR=900) RVAL
      ENDIF
      GO TO 999
*
  900 IRC   = 100
*                                                             END CDCTOR
  999 END
