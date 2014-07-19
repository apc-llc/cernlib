*
* $Id: cdinin.F,v 1.1.1.1 1996/02/28 16:24:21 mclareni Exp $
*
* $Log: cdinin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:21  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDININ (IA1, IA2, N, IPREC, IER)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDININ (IA1, IA2*, N, IPREC, IER*)                      *
*                                                                      *
*   Truncates integer to signed integer according to the given         *
*   precision                                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IA1      Input array of integer numbers                          *
*     IA2      Output array of signed integers                         *
*     N        Length of the array                                     *
*     IPREC    Signed precision word; the data are truncated after     *
*              having multiplied by 10**IPREC                          *
*     IER      Error Code : 0    No error                              *
*                           1    Precision not given or not correct    *
*                           3    Data is not uncompressed              *
*                                                                      *
*   Called by CDCMPR                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IER       =  0 : No error (see above)                            *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      DIMENSION       IA1(N), IA2(N)
*
*     ------------------------------------------------------------------
*
      IER    = 0
*
* *** Is data uncompressed ?
*
      IF (IA1(1).NE.0) THEN
        IER    = 3
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDININ : Data is'//
     +  ' not uncompressed - no compression'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Precision ?
*
      IPREC  = IA1(2)
*
      IF (IPREC.GT.0) THEN
        IER    = 1
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDININ : Precisi'//
     +  'on is not correctly given - no compression'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IPR1   = -IPREC
*
      IF (IPR1.EQ.0) THEN
        CALL UCOPY (IA1, IA2, N)
        GO TO 999
      ENDIF
      XMULT  = 1./10**IPR1
      DO 10 I = 3, N
   10 IA2(I) = IA1(I)*XMULT
      IA2(1) = IA1(1)
      IA2(2) = IA1(2)
*                                                             END CDININ
  999 END
