*
* $Id: cdpres.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdpres.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPRES (PATHI, PATHO, IOTI, IOTO, CHTI, CHTO, KEYI,
     +                   KEYO, ICONI, ICONO, NI, NO, NOB, CFNAM, IRC)
*     ===============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPRES (PATHI, PATHO, IOTI, IOTO, CHTI, CHTO, KEYI,     *
*                      KEYO, ICONI, ICONO, NI, NO, NOB, CFNAM, IRC*)   *
*                                                                      *
*   Routine to Present the "View"                                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Pathname of the First Table                             *
*     PATHO    Pathname of the Last  Table                             *
*     IOTI     Type of Keys used for Search                            *
*     IOTO     Type of Keys used for Presentation                      *
*     CHTI     Name of Keys used for Search                            *
*     CHTO     Name of Keys used for Presentation                      *
*     KEYI     Value of Keys used for Search                           *
*     KEYO     Value of Keys to be presented                           *
*     ICONI    Number of keys concatenated to this key                 *
*     ICONO    Number of keys concatenated to this key                 *
*     NI       Number of Keys used for Search                          *
*     NO       Number of Keys to be Presented                          *
*     NOB      Number of Objects to be presented                       *
*     CFNAM    File Name used for presentation                         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDVIEW                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       IDPFL(100)
      DIMENSION       IOTI(NI), IOTO(NO), KEYI(NI), KEYO(NO,NOB)
      DIMENSION       ICONI(NI), ICONO(NO)
      CHARACTER       PATHI*(*), PATHO*(*), CFNAM*(*)
      CHARACTER       CHTI(NI)*8, CHTO(NO)*8, CHEAD*300
*
*     ------------------------------------------------------------------
*
* *** Open file for Presentation
*
      LUN = LUKYCX
      CALL CDOPFL (LUN, CFNAM, 'UNKNOWN', ISTAT)
      IF (ISTAT.NE.0)  GO TO 999
*
* *** Header Line
*
      WRITE (LUN, 1001) PATHO
      IF (IOPHCC.NE.0) THEN
*
*  **   Check if display is wide enough
*
        NW = NO - 1
        IF (NW.GT.0) THEN
          DO 1 I = 1, NW
    1     IDPFL(I) = 1
          CALL CDRGCV (IDPFL, NW, NDISP, IOTO(2))
          IF (NDISP.GT.MXDPCC) THEN
            CALL CDPRNT (LPRTCD, '(/,'' CDPRES : Not enough space to '//
     +           'display.'',/,'' Enlarge display range by CDSETD or '//
     +           'use V-mode.'')', IARGCD, 0)
            WRITE (LUN, 1007)
            GO TO 100
          ENDIF
*
*  **     Display header
*
          CHEAD = ' '
          CHEAD(4:9) = 'Serial'
          CALL CDVHEA (IOTO(2), CHTO(2), NW, CHEAD)
          WRITE (LUN, '(A/)') CHEAD(1:MXDPCC)
*
*  **     Display the Objects if there is any
*
          WRITE (LUN, 1003)
          IF (NOB.GT.0) THEN
            DO 5 I = 1, NOB
              CALL CDDKYH (LUN, NW, KEYO(2,I), IOTO(2), ICONO(2),
     +                              KEYO(1,I), IRC)
    5       CONTINUE
          ENDIF
        ENDIF
      ELSE
        WRITE (LUN, '(/)')
*
*  **   Display the Objects if there is any
*
        IF (NOB.GT.0) THEN
          DO 10 I = 1, NOB
            WRITE (LUN, 1005) KEYO(1,I)
            CALL CDDKYV (LUN, NO-1, CHTO(2), KEYO(2,I), IOTO(2),
     +                   ICONO(2), IRC)
   10     CONTINUE
        ENDIF
      ENDIF
*
* *** Closing Phrase
*
      WRITE (LUN, 1004) PATHI
      IF (IOPHCC.NE.0) THEN
*
*   *   Check if display is wide enough
*
        IF (NI.GT.0) THEN
          NW = NI
          DO 11 I = 1, NW
   11     IDPFL(I) = 1
          CALL CDRGCV (IDPFL, NW, NDISP, IOTI(1))
          IF (NDISP.GT.MXDPCC) THEN
            CALL CDPRNT (LPRTCD, '(/,'' CDPRES : Not enough space to '//
     +           'display.'',/,'' Enlarge display range by CDSETD or '//
     +           'use V-mode.'')', IARGCD, 0)
            WRITE (LUN, 1007)
            GO TO 100
          ENDIF
*
*  **     Display header
*
          CHEAD = ' '
          CHEAD(4:9) = 'Serial'
          CALL CDVHEA (IOTI(1), CHTI(1), NW, CHEAD)
          WRITE (LUN, '(A/)') CHEAD(1:MXDPCC)
*
          WRITE (LUN, 1003)
          CALL CDDKYH (LUN, NW, KEYI(1), IOTI(1), ICONI(1), -1, IRC)
        ENDIF
      ELSE
        WRITE (LUN, '(/)')
        IF (NI.GT.0) THEN
          CALL CDDKYV (LUN, NI, CHTI, KEYI(1), IOTI(1), ICONI(1), IRC)
        ENDIF
      ENDIF
*
  100 CALL CDCLFL (LUN)
      CALL KUEDIT (CFNAM, IST)
*
 1001 FORMAT (/2X,' Objects in Directory ',A/)
 1002 FORMAT (/20(3X,A8,3X)/)
 1003 FORMAT (/)
 1004 FORMAT (////2X,' Satisfy the Search in Directory ',A//)
 1005 FORMAT (/2X,' ---- Object-Serial : ',I10,' ----'/)
 1007 FORMAT (/,' CDPRES : Not enough space to display.' /
     +          ' Enlarge display range by CDSETD or use V-mode.')
*                                                             END CDPRES
  999 END
