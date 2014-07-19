*
* $Id: cdflin.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdflin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFLIN (A, IA, N, IPREC, IER)
*     ========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFLIN (A, IA*, N, IPREC, IER*)                         *
*                                                                      *
*   Converts floating to signed integer according to the given         *
*   precision                                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     A        Input array of real numbers                             *
*     IA       Output array of signed integers                         *
*     N        Length of the array                                     *
*     IPREC    Signed precision word; the data are truncated after     *
*              having multiplied by 10**IPREC                          *
*     IER      Error Code : 0    No error                              *
*                           1    Precision not given or not correct    *
*                           2    Data exceeds 32 bits                  *
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
      DIMENSION       A(N), IA(N)
      DATA            TOOBIG /2.1474E9/
*
*     ------------------------------------------------------------------
*
      IER    = 0
*
* *** Is data uncompressed ?
*
      IF (A(1).NE.0.) THEN
        IER    = 3
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFLIN : Data is'//
     +  ' not uncompressed !'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Precision ?
*
      IPREC  = A(2)
      IF (IPREC.LT.-99.OR.IPREC.GT.99) THEN
        IER    = 1
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFLIN : Precisi'//
     +  'on is not correctly given - no compression'',/)', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IF (IPREC.EQ.0) THEN
        IER    = 1
#if defined(CERNLIB__DEBUG)
CCC     IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFLIN : Precisi'//
CCC  +  'on is not given - Compression cannot be made'',/)', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IPR1   = 0
      IPR2   = 0
      IF (IPREC.GT.0) IPR2 = IPREC
      IF (IPREC.LT.0) IPR1 = -IPREC
*
      IF (IPR1.NE.0) THEN
        XMULT  = 1./10.**IPR1
        DO 10 I = 3, N
          XXX    = A(I)*XMULT
*
*  **     Test overflow of IA - if overflow occurs, do not pack !
*
          IF (ABS(XXX).GT.TOOBIG) THEN
            IER    = 2
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFLIN : Qua'//
     +      'ntity to be packed exceeds 32-Bit Integer size - no pack'//
     +      'ing'',/)', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
*
          IA(I) = XXX
   10   CONTINUE
      ELSE
        XMULT  = 10.**IPR2
        DO 20 I = 3, N
          XXX    = A(I)*XMULT
*
*  **     Test overflow of IA - if overflow occurs, do not pack !
*
          IF (ABS(XXX).GT.TOOBIG) THEN
            IER    = 2
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDFLIN : Qua'//
     +      'ntity to be packed exceeds 32-Bit Integer size - no pack'//
     +      'ing'',/)', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
*
          IF (XXX.GE.0.) THEN
            IA(I) = IFIX (XXX + 0.5)
          ELSE
            IA(I) = IFIX (XXX - 0.5)
          ENDIF
   20   CONTINUE
      ENDIF
*                                                             END CDFLIN
  999 END
