*
* $Id: cddkyv.F,v 1.1.1.1 1996/02/28 16:24:48 mclareni Exp $
*
* $Log: cddkyv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:48  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDKYV (LUN, NW, CT, KY, KT, ICON, IRC)
*     ==================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDKYV (LUN, NW, CT, KY, KT, ICON, IRC*)                *
*                                                                      *
*   Displays a vector KY of elements NW according to its type KT       *
*   in Vertical Mode (Concatenated keys are printed in 1 line)         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of the file on which to display             *
*     NW       Number of elements in the array KY (and KT)             *
*     CT       Character-Name of the Key to be printed                 *
*     KY       Vector to be printed                                    *
*     KT       Type of the vector elements                             *
*          1 - Binary                                                  *
*          2   Integer                                                 *
*          3   Floating                                                *
*          4   Double Precision (not yet implemented)                  *
*          5   Hollerith                                               *
*          6   Hollerith concatenetad to previous hollerith            *
*          7   Packed integer - time packed upto seconds               *
*          8   Packed integer - time packed upto minutes               *
*          9   Data which should not be displayed                      *
*     ICON     Number of keys concatenated to the present one          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDISV, CDPEEK, CDPRES                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =103 : Illegal data type                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHPRT*80, CT(*)*8
      DIMENSION       KY(9), KT(9), ICON(9)
*
*     ------------------------------------------------------------------
*
      I     = 1
   10 CHPRT = ' '
      I1    = 20
      CHPRT(3:10) = CT(I)
      IF (KT(I).EQ.1) THEN
*
*  **   Binary
*
        CHPRT(13:13) = 'B'
        I1 = I1 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1001) KY(I)
        I = I + 1
*
      ELSE IF (KT(I).EQ.2) THEN
*
*  **   Integer
*
        CHPRT(13:13) = 'I'
        I1 = I1 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1002) KY(I)
        I = I + 1
*
      ELSE IF (KT(I).EQ.3) THEN
*
*  **   Floating
*
        CHPRT(13:13) = 'F'
        I1 = I1 + 1
        I2 = I1 + 11
        WRITE (CHPRT(I1:I2), 1003) KY(I)
        I = I + 1
*
      ELSE IF (KT(I).EQ.5.OR.KT(I).EQ.6) THEN
*
*  **   Hollerith
*
         CHPRT(13:13) = 'H'
         I1 = I1 + 5
         I2 = I1 + 3
         WRITE (CHPRT(I1:I2), 1005) KY(I)
         I = I + 1
*          Is Key  concatenated with the next one ?
         NCONC = ICON(I-1)
         IF (NCONC.EQ.0)                 GO TO 20
         DO 15 J = 1, NCONC
           I1 = I2 + 1
           I2 = I1 + 3
           WRITE (CHPRT(I1:I2), 1005) KY(I)
           I = I + 1
   15    CONTINUE
*
      ELSE IF (KT(I).EQ.7) THEN
*
*  **   Packed Integer - Time packed upto seconds
*
        CHPRT(13:13) = 'S'
        CALL CDUPTS (IDATE, ITIME, KY(I), IRC)
        I1 = I1 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1002) IDATE
        I1 = I2 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1002) ITIME
        I = I + 1
*
      ELSE IF (KT(I).EQ.8) THEN
*
*  **   Packed Integer - Time packed upto minutes
*
        CHPRT(13:13) = 'M'
        CALL CDUPTM (IDATE, ITIME, KY(I), IRC)
        I1 = I1 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1002) IDATE
        I1 = I2 + 2
        I2 = I1 + 9
        WRITE (CHPRT(I1:I2), 1002) ITIME
        I = I + 1
*
      ELSE IF (KT(I).EQ.9) THEN
*
*  **   Key_value should not be visible
*
        CHPRT = ' '
        CHPRT(13:13) = 'U'
        I = I + 1
*
      ELSE
*
        IRC    = 103
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDKYV : Illeg'//
     +     'al data type to be printed = '',I10,/)', KT(I), 1)
#endif
        GO TO 999
      ENDIF
*
   20 CONTINUE
      WRITE (LUN, 2001) CHPRT
      IF (I.LE.NW)                         GO TO 10
      IRC    = 0
*
 1001 FORMAT (Z10)
 1002 FORMAT (I10)
 1003 FORMAT (E12.4)
 1005 FORMAT (A4)
 2001 FORMAT (A)
*                                                             END CDDKYV
  999 END
