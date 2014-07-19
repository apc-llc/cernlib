*
* $Id: cddckh.F,v 1.1.1.1 1996/02/28 16:24:47 mclareni Exp $
*
* $Log: cddckh.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDCKH (CHPRT, KY, IKEY, IRC)
*     ========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDDCKH (CHPRT, KY*, IKEY*, IRC*)                        *
*                                                                      *
*   Fills a vector KY of elements NWKYCK according to its type IOTYCC  *
*   from the character string CHPRT in Horizontal Mode                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHPRT    Character string containing the keys to be decoded      *
*     KY       Vector to be filled with the decoded elements           *
*     IKEY     Serial number of the key to be filled                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Other variables :                                                  *
*                                                                      *
*     IOTYCC   Type of the vector elements                             *
*          1 - Binary                                                  *
*          2   Integer                                                 *
*          3   Floating                                                *
*          4   Double Precision (not yet implemented)                  *
*          5   Hollerith                                               *
*          6   Hollerith concatenetad to previous hollerith            *
*          7   Packed integer - time packed upto seconds               *
*          8   Packed integer - time packed upto minutes               *
*          9   Data which should not be displayed                      *
*     ICONCK   Number of keys concatenated to the present one          *
*     NWKYCK   Number of elements in the array KY (and IOTYCC)         *
*                                                                      *
*   Called by  CDEDAS, CDEDKY                                          *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
      CHARACTER       CHPRT*(*)
      DIMENSION       KY(9)
*
*     ------------------------------------------------------------------
*
      READ (CHPRT(2:9), '(I8)') IKEY
      CALL VZERO (KY(1), NWKYCK)
      ISTR = 12
      IRC  = 0
      DO 10 I = 1, NWKYCK
        KK    = IOTYCC(I)
        GO TO (1,2,10,10,5,10,7,8,10), KK
    1   CONTINUE
*
*  **   Binary
*
        IEND = ISTR + NUMCCC(I) - 3
        CALL CDCTOB (CHPRT(ISTR:IEND), KY(I), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        ISTR = IEND + 3
        GO TO 10
    2   CONTINUE
*
*  **   Integer
*
        IEND = ISTR + NUMCCC(I) - 3
        CALL CDCHTI (CHPRT(ISTR:IEND), KY(I), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        ISTR = IEND + 3
        GO TO 10
    5   CONTINUE
*
*  **   Hollerith
*
        IEND = ISTR + NUMCCC(I) - 3
        IREP = ICONCK(I) + 1
        IF (IREP.EQ.1) THEN
          DO 51 J = ISTR, IEND
            IF (CHPRT(J:J).EQ.' ') THEN
              GO TO 51
            ELSE IF (CHPRT(J:J).EQ.'''') THEN
              IST = J + 1
              GO TO 52
            ELSE
              IST = J
              GO TO 52
            ENDIF
   51     CONTINUE
          IST  = IEND
   52     ISTR = IST
        ENDIF
        CALL UCTOH (CHPRT(ISTR:IEND), KY(I), 4, 4*IREP)
        ISTR = IEND + 3
        GO TO 10
    7   CONTINUE
*
*  **   Packed Integer (upto seconds)
*
        IEND = ISTR + NUMCCC(I) - 3
        I1 = ISTR
        I2 = I1 + 5
        READ  (CHPRT(I1:I2), '(I6)') IDATE
        I1 = I2 + 2
        I2 = I1 + 5
        READ  (CHPRT(I1:I2), '(I6)') ITIME
        ISTR = IEND + 3
        CALL CDPKTS (IDATE, ITIME, KY(I), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        GO TO 10
    8   CONTINUE
*
*  **   Packed Integer (upto minutes)
*
        IEND = ISTR + NUMCCC(I) - 3
        I1 = ISTR
        I2 = I1 + 5
        READ  (CHPRT(I1:I2), '(I6)') IDATE
        I1 = I2 + 2
        I2 = I1 + 5
        READ  (CHPRT(I1:I2), '(I6)') ITIME
        ISTR = IEND + 3
        CALL CDPKTM (IDATE, ITIME, KY(I), IRC0)
        IF (IRC0.NE.0) IRC  = IRC0
        GO TO 10
*
   10 CONTINUE
      GO TO 999
*                                                             END CDDCKH
  999 END
