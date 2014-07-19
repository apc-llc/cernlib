*
* $Id: cdprky.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdprky.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if (defined(CERNLIB__P3CHILD))&&(defined(CERNLIB_IBM))
* Ignoring t=dummy
#endif
      SUBROUTINE CDPRKY (NW, KY, KT, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPRKY (NW, KY, KT, IRC*)                               *
*                                                                      *
*   Prints a vector KY of elements NW according to its type KT         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NW       Number of elements in the array KY (and KT)             *
*     KY       Vector to be printed                                    *
*     KT       Type of the vector elements                             *
*          1 - Binary                                                  *
*          2   Integer                                                 *
*          3   Floating                                                *
*          4   Double Precision (not yet implemented)                  *
*          5   Hollerith                                               *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDENFZ, CDENTB, CDKOUT, CDPART, CDPRDT, CDPURP           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 39 : Illegal data type                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHPRT*132
      DIMENSION       KY(NW), KT(NW)
*
*     ------------------------------------------------------------------
*
      I1    = 3
      IRC   = 0
      CHPRT = ' '
      DO 10 I = 1, NW
        IF (I1.GT.120) THEN
          CALL CDPRNT (LPRTCD, '('''//CHPRT//''')', IARGCD, 0)
          I1    = 3
          CHPRT = ' '
        ENDIF
*
        IF (KT(I).EQ.1) THEN
*
*  **     Binary
*
          I2 = I1 + 11
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CHPRT(I1:I2), '(2X,Z10)') KY(I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CALL UTWRIT (CHPRT(I1:I2), '(2X,Z10)', KY(I), 1)
#endif
*
        ELSE IF (KT(I).EQ.2) THEN
*
*  **     Integer
*
          I2 = I1 + 11
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CHPRT(I1:I2), '(2X,I10)') KY(I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CALL UTWRIT (CHPRT(I1:I2), '(2X,I10)', KY(I), 1)
#endif
*
        ELSE IF (KT(I).EQ.3) THEN
*
*  **     Floating
*
          I2 = I1 + 11
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CHPRT(I1:I2), '(G12.4)') KY(I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CALL UTWRIT (CHPRT(I1:I2), '(G12.4)', KY(I), 1)
#endif
*
        ELSE IF (KT(I).EQ.5.OR.KT(I).EQ.6) THEN
*
*  **     Hollerith
*
          I2 = I1 + 11
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
          WRITE (CHPRT(I1:I2), '(8X,A4)') KY(I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
          CALL UTWRIT (CHPRT(I1:I2), '(8X,A4)', KY(I), 1)
#endif
*
        ELSE
*
*  **     Error
*
          IRC = 39
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPRKY : '//
     +       'Illegal data type to be printed = '',I10/)', KT(I), 1)
#endif
          GO TO 999
*
        ENDIF
        I1 = I2 + 1
*
   10 CONTINUE
      IF (I1.GT.3) CALL CDPRNT (LPRTCD, '('''//CHPRT//''')', IARGCD, 0)
*                                                             END CDPRKY
  999 END
