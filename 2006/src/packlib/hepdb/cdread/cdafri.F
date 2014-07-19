*
* $Id: cdafri.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdafri.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDAFRI (LUN, LBD, CHOPT, IRC)
*     ========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDAFRI (LUN, LBD, IRC*)                                 *
*                                                                      *
*   Decodes the content of the bank from packed bits to ASCII set and  *
*   writes it out into a file already opened                           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number of the file with ASCII data         *
*     LBD      Address of the data bank                                *
*     CHOPT    Character options                                       *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDHELP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*     IRC       =199 : Corrupted data                                  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      DIMENSION       LBD(9)
      CHARACTER       KLINE*80
      CHARACTER*(*)   CHOPT
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
      IRC       = 0
      IOPTC     = INDEX(CHOPT,'C')
      LREFCD(5) = LBD(1)
      IF (LUN.GT.0.AND.LREFCD(5).GT.0) THEN
        NDATA  = IQ(KOFUCD+LREFCD(5)-1)
        IF (NDATA.GT.0) THEN
#if !defined(CERNLIB__P3CHILD)
*
*  **     Display data if exists, if it does not, display dummy data
*
          NTOT   = 0
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          NREC   = 0
#endif
#if !defined(CERNLIB__P3CHILD)
   10     IF (NTOT.LT.NDATA) THEN
            MTOT=NTOT
            CALL CDLIND (IQ(KOFUCD+LREFCD(5)+1), NTOT, KLINE, LENG)
            IF (LENG.GT.0) THEN
              IF(IOPTC.EQ.0) THEN
                 WRITE (LUN, 1000) KLINE(1:LENG)
              ELSE
                 WRITE (LUN, 1001) KLINE(1:LENG)
              ENDIF
            ELSE IF(NTOT.GT.MTOT) THEN
              WRITE (LUN, *)
            ELSE
*             -- a case of corrupted data, force end of scan
              NTOT=NDATA
              IRC=199
              WRITE (LUN, *) ' ### Corrupted data ###'
              IF (IDEBCD.GT.-3)
     +      CALL CDPRNT (LPRTCD, '(/,'' CDAFRI : corrupted data '')',1)
            ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            NREC   = NREC + 1
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 10
          ENDIF
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.1) THEN
            CALL CDPRNT (LPRTCD, '(/,'' CDAFRI : '',I10,'' records '//
     +           'written for the current set '')', NREC, 1)
          ENDIF
#endif
#if defined(CERNLIB__P3CHILD)
          RNDBP3    = 'CDAFRI'
          NWDBP3    = 2
          IWDBP3(1) = LUN
          IWDBP3(2) = NDATA
          CALL CDCHLD
          IF (IQDBP3.EQ.0)
     +      CALL APFZUT (LODBP3, IDIVCD, LREFCD(5), 1, 'S', 0, 0, 0)
#endif
        ENDIF
      ENDIF
#if !defined(CERNLIB__P3CHILD)
*
 1000 FORMAT (A)
 1001 FORMAT (1X,A)
#endif
*                                                             END CDAFRI
  999 END
