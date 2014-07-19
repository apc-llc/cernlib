*
* $Id: cdopts.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdopts.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPTS (CHOPT, IRC)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDOPTS (CHOPT, IRC*)                                    *
*                                                                      *
*   Decode the character option and fill up the common block /CAOPTS/  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHOPT    Character option supplied by user                       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 91 : Illegal Character Option                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
      CHARACTER       CHOPT*(*), CTEST*1, BLANK*1, CTMP*80
      DIMENSION       ICOPT(300), IALFA(MXKYCA+28)
      EQUIVALENCE     (IALFA(1), IOPACA)
      DATA            BLANK / ' '/
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      DO 5 K = 1, MXKYCA+28
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
* *** Illegal Character option
*
  991 IRC        = 91
      IQUEST(11) = I0
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) THEN
        CTMP   = CHOPT
        CALL CDPRNT (LPRTCD, '('' CDOPTS : Illegal Character option a'//
     +       'fter '',I2,'' in '//CTMP//''')', IQUEST(11), 1)
      ENDIF
#endif
*                                                             END CDOPTS
  999 END
