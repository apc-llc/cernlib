*
* $Id: cdxopt.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdxopt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDXOPT (CHOPT, MASK)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. CDXOPT (CHOPT, MASK*)                                   *
*                                                                      *
*   Decode the character option and fill up the vector MASK            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHOPT    Character option supplied by user                       *
*     MASK     A vector set to 1 if the corresponding option is on     *
*              (numeric part of CHOPT)                                 *
*                                                                      *
*   Called by CDAUXI                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/cdcblk.inc"
      CHARACTER       CHOPT*(*), CTEST*1, BLANK*1
      DIMENSION       ICOPT(300), MASK(9)
      DATA            BLANK / ' '/
*
*     ------------------------------------------------------------------
*
* *** Convert CHOPT into a list of integer variables
*
      MAXL   = LEN (CHOPT)
      I1     = 0
      DO 20 I0 = 1, MAXL
        CTEST  = CHOPT(I0:I0)
        DO 10 I = 1, 62
          IF (CTEST.EQ.CALFCA(I)) GO TO 15
   10   CONTINUE
        IF (CTEST.NE.BLANK)       GO TO 999
        GO TO 20
   15   I1     = I1 + 1
        IF (I.GT.36) I = I - 36
        ICOPT(I1) = I
   20 CONTINUE
      MAXU   = I1
*
* *** Now fill up the common /CAOPTS/
*
      I0     = 0
   30 I0     = I0 + 1
      IF (I0.GT.MAXU)             GO TO 999
      I      = ICOPT(I0)
*
*  ** Alphabetic options
*
      IF (I.LE.26)                GO TO 30
*
*  ** Numeric options (at the moment 3-29 only)
*  ** If first digit = 1 or 2, it has to be a 2 digit number
*  **                = 3 .. 9, it can only be one digit number
*  ** First digit can never be 0.
*
      I1     = I - 27
      IF (I1.EQ.0)                GO TO 30
      IF (I1.GE.3.AND.I1.LE.9) THEN
        MASK(I1) = 1
        GO TO 30
      ENDIF
      IF (I0.GE.MAXU)             GO TO 999
      I      = ICOPT(I0+1) - 27
      IF (I.LT.0)                 GO TO 999
      MASK(I1*10+I) = 1
      I0     = I0 + 1
      GO TO 30
*                                                             END CDXOPT
  999 END
