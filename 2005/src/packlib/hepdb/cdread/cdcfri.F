*
* $Id: cdcfri.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdcfri.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDCFRI (CHTEXT, NTEXT, LTEXT, LBD, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCFRI (CHTEXT, *NTEXT*, LTEXT, LDB, IRC*)              *
*                                                                      *
*   Decodes the content of the bank from packed bits to ASCII set and  *
*   copies it to a character buffer                                    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHTEXT   Character buffer to which data is copied                *
*     *NTEXT*  On input, number of elements of CHTEXT                  *
*              On output, number of elements that have been filled     *
*     LTEXT    Maximum length of an element that can be accepted       *
*     LBD      Address of the data bank                                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCHAR                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  ? : data has been truncated                         *
*               =  ? : buffer overflow : see IQUEST(1)                 *
*     IQUEST(1) =  number of elements in data bank                     *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   CHTEXT(NTEXT)
#include "hepdb/cdcblk.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      DIMENSION       LBD(9)
      CHARACTER       KLINE*80
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
      IRC       = 0
      LREFCD(5) = LBD(1)
      IF (LREFCD(5).GT.0) THEN
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
             CALL CDLIND (IQ(KOFUCD+LREFCD(5)+1), NTOT, KLINE, LENG)
             NREC = NREC + 1
             IF(NREC.LE.NTEXT) THEN
                IF (LENG.GT.0) THEN
                  IF(LENG.GT.LTEXT) THEN
                     IRC  = 2
                     LENG = LTEXT
                  ENDIF
                  CHTEXT(NREC) = KLINE(1:LENG)
                ELSE
                  CHTEXT(NREC) = ' '
                ENDIF
                GO TO 10
             ENDIF
          ENDIF
          IQUEST(1) = NREC
          NTEXT     = MIN(NTEXT,NREC)
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
          IF (IDEBCD.GT.1) THEN
            CALL CDPRNT (LPRTCD, '(/,'' CDCFRI : '',I10,'' records '//
     +           'copied for the current set '')', NREC, 1)
          ENDIF
#endif
#if defined(CERNLIB__P3CHILD)
          RNDBP3    = 'CDCFRI'
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
#endif
*                                                             END CDCFRI
  999 END
