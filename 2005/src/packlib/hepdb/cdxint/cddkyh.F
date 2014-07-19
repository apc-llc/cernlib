*
* $Id: cddkyh.F,v 1.1.1.1 1996/02/28 16:24:48 mclareni Exp $
*
* $Log: cddkyh.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:48  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDKYH (LUN, NW, KY, KT, ICON, IKEY, IRC)
*     ====================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDKYH (LUN, NW, KY, KT, ICON, IKEY, IRC*)              *
*                                                                      *
*   Displays a vector KY of elements NW according to its type KT       *
*   in Horizontal Mode                                                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of the file on which to display             *
*     NW       Number of elements in the array KY (and KT)             *
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
*     IKEY     Serial number of the key to be displayed                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDISD, CDDISH, CDPRES                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =103 : Illegal data type                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
      CHARACTER       CHPRT*300, CFORM*5
      DIMENSION       KY(9), KT(9), ICON(9)
*
*     ------------------------------------------------------------------
*
      CHPRT = ' '
      IF (IKEY.GT.0) THEN
        WRITE (CHPRT(2:9), 1001) IKEY
      ENDIF
      I1    = 11
      DO 10 I = 1, NW
        IF (KT(I).EQ.1) THEN
*
*  **     Binary
*
          I1 = I1 + 1
          I2 = I1 + NUMCCC(I) - 3
          CFORM(1:2) = '(Z'
          CFORM(5:5) = ')'
          WRITE (CFORM(3:4), 1002) NUMCCC(I)-2
          WRITE (CHPRT(I1:I2), CFORM) KY(I)
          I1 = I2 + 2
*
        ELSE IF (KT(I).EQ.2) THEN
*
*  **   Integer
*
          I1 = I1 + 1
          I2 = I1 + NUMCCC(I) - 3
          CFORM(1:2) = '(I'
          CFORM(5:5) = ')'
          WRITE (CFORM(3:4), 1002) NUMCCC(I)-2
          WRITE (CHPRT(I1:I2), CFORM) KY(I)
          I1 = I2 + 2
*
        ELSE IF (KT(I).EQ.5) THEN
*
*  **     Hollerith
*
          IREP = ICON(I) + 1
          I1 = I1 + 1
          DO 6 J = 1, IREP
            I2 = I1 + 3
            IF (IREP.EQ.1) THEN
              I1 = I1 + 2
              I2 = I2 + 2
            ENDIF
            WRITE (CHPRT(I1:I2), 1003) KY(I+J-1)
            I1 = I2 + 1
    6     CONTINUE
          I1 = I1 + 1
*
        ELSE IF (KT(I).EQ.7) THEN
*
*  **     Packed Integer (upto seconds)
*
          CALL CDUPTS (IDATE, ITIME, KY(I), IRC)
          I1 = I1 + 1
          I2 = I1 + 5
          WRITE (CHPRT(I1:I2), 1004) IDATE
          I1 = I2 + 2
          I2 = I1 + 5
          WRITE (CHPRT(I1:I2), 1004) ITIME
          I1 = I2 + 2
*
        ELSE IF (KT(I).EQ.8) THEN
*
*  **     Packed Integer (upto minutes)
*
          CALL CDUPTM (IDATE, ITIME, KY(I), IRC)
          I1 = I1 + 1
          I2 = I1 + 5
          WRITE (CHPRT(I1:I2), 1004) IDATE
          I1 = I2 + 2
          I2 = I1 + 5
          WRITE (CHPRT(I1:I2), 1004) ITIME
          I1 = I2 + 2
*
        ELSE IF (KT(I).EQ.3.OR.KT(I).EQ.6.OR.KT(I).EQ.9) THEN
*
        ELSE
*
          IRC    = 103
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDDKYH : Illeg'//
     +       'al data type to be printed = '',I10,/)', KT(I), 1)
#endif
          GO TO 999
        ENDIF
*
   10 CONTINUE
      WRITE (LUN, 2001) CHPRT(1:MXDPCC)
      IRC    = 0
*
 1001 FORMAT (I8)
 1002 FORMAT (I2)
 1003 FORMAT (A4)
 1004 FORMAT (I6)
 2001 FORMAT (A)
*                                                             END CDDKYH
  999 END
