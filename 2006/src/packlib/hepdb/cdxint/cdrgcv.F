*
* $Id: cdrgcv.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdrgcv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRGCV (KDISP, NK, NTOT, IOKEY)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRGCV (KDISP, NK, NTOT*, *IOKEY*)                      *
*                                                                      *
*   Checks if the display Range is enough to display data              *
*      in Horizontal Mode (for CDVIEW action)                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KDISP    Key Flag = 0 for No Display, = 1 for Key to be displayed*
*     NK       Number of Keys                                          *
*     NTOT     Place occupied by displaying the keys                   *
*     IOKEY    Type of the Key                                         *
*          1 - Binary                                                  *
*          2   Integer                                                 *
*          3   Floating                                                *
*          4   Double Precision (not yet implemented)                  *
*          5   Hollerith                                               *
*          6   Hollerith concatenetad to previous hollerith            *
*          7   Packed integer - time packed upto seconds               *
*          8   Packed integer - time packed upto minutes               *
*          9   Data which should not be displayed                      *
*                                                                      *
*   Called by CDPRES                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       KDISP (NK), NUMBS(7), IOKEY(NK)
      DATA            NUMBS /12, 12, 15, 15, 12, 12, 15/
      DATA            NUMBI, NUMIN, NUMHO /12, 12, 6/
*
*     ------------------------------------------------------------------
*
      NTOT = 10
*
*  ** Input Keys (cannot be system keys)
*
      DO 10 I = 1,NK
        IF (KDISP(I).NE.0) THEN
          IF (IOKEY(I).EQ.6) THEN
            IOKEY(I) = 9
            NUMCCC(I) = 0
          ELSE IF (IOKEY(I).EQ.1) THEN
            NUMCCC(I) = NUMBI
            NTOT = NTOT + NUMBI
          ELSE IF (IOKEY(I).EQ.2) THEN
            NUMCCC(I) = NUMIN
            NTOT = NTOT + NUMIN
          ELSE IF (IOKEY(I).EQ.5) THEN
            NUMCCC(I) = NUMHO + 4*ICONCK(I)
            NUMCCC(I) = MAX0 (10, NUMCCC(I))
            NTOT = NTOT + NUMCCC(I)
          ENDIF
        ELSE
          IOKEY(I) = 9
          NUMCCC(I) = 0
        ENDIF
   10 CONTINUE
*                                                             END CDRGCV
      END
