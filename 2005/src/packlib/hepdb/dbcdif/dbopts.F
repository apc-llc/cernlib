*
* $Id: dbopts.F,v 1.1.1.1 1996/02/28 16:25:04 mclareni Exp $
*
* $Log: dbopts.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:04  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBOPTS (CHOPT, IRC)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. DBOPTS (CHOPT, IRC*)                                    *
*                                                                      *
*   Decode the character option and fill up the common block /CAOPTS/  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHOPT    Character option supplied by user                       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by various routines in the patch DBCDFC                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 91 : Illegal Character Option                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      CHARACTER       CHOPT*(*), CTEST*1, BLANK*1, CTMP*80
      DIMENSION       ICOPT(300), IALFA(55)
      EQUIVALENCE     (IALFA(1), IOPADX)
      DATA            BLANK / ' '/
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      DO 5 K = 1, 26+29
        IALFA(K) = 0
    5 CONTINUE
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
        IF (CTEST.NE.BLANK)       GO TO 991
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
      IF (I.LE.26) THEN
        IALFA(I) = 1
        GO TO 30
      ENDIF
*
*  ** Numeric options (at the moment 3-29 only)
*  ** If first digit = 1 or 2, it has to be a 2 digit number
*  **                = 3 .. 9, it can only be one digit number
*  ** First digit can never be 0.
*
      I1     = I - 27
      IF (I1.EQ.0)                GO TO 991
      IF (I1.GE.3.AND.I1.LE.9) THEN
        IOKYDX(I1) = 1
        GO TO 30
      ENDIF
      IF (I0.GE.MAXU)             GO TO 991
      I      = ICOPT(I0+1) - 27
      IF (I.LT.0)                 GO TO 991
      IOKYDX(I1*10+I) = 1
      I0     = I0 + 1
      GO TO 30
*
* *** Illegal Character option
*
  991 IRC        = 91
      IQUEST(11) = I0
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) THEN
        CTMP   = CHOPT
        CALL CDPRNT (LPRTCD, '('' DBOPTS : Illegal Character option a'//
     +       'fter '',I2,'' in '//CTMP//''')', IQUEST(11), 1)
      ENDIF
#endif
*                                                             END DBOPTS
  999 END
