*
* $Id: cdrgck.F,v 1.1.1.1 1996/02/28 16:24:51 mclareni Exp $
*
* $Log: cdrgck.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:51  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRGCK (KDISP, NK, NTOT, IOKEY)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRGCK (KDISP, NK, NTOT*, *IOKEY*)                      *
*                                                                      *
*   Checks if the display Range is enough to display data              *
*      in Horizontal Mode (except for CDVIEW action)                   *
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
*   Called by CDDISH                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       KDISP (NK), IOKEY(NK)
      DATA            NUMBI /12/, NUMIN /12/, NUMHO / 6/, NUMTS /15/
*
*     ------------------------------------------------------------------
*
      NTOT = 10
*
*  ** System Keys
*
      IF (IOPXCA.NE.0) THEN
        INDXT  = 7
      ELSE
        INDXT  = 2
      ENDIF
      DO 5 I = 1, 2*NPARCD
        IOKEY(NOF1CK+I) = INDXT
    5 CONTINUE
      IOKEY(IDHINS) = 8
*
*  ** All Keys
*
      DO 10 I = 1, NK
        IF (KDISP(I).NE.0) THEN
          IF (IOKEY(I).EQ.7.OR.IOKEY(I).EQ.8) THEN
            NUMCCC(I) = NUMTS
          ELSE IF (IOKEY(I).EQ.1) THEN
            NUMCCC(I) = NUMBI
          ELSE IF (IOKEY(I).EQ.6) THEN
            IOKEY(I)  = 9
            NUMCCC(I) = 0
          ELSE IF (IOKEY(I).EQ.5) THEN
            NUMCCC(I) = NUMHO + 4*ICONCK(I)
            NUMCCC(I) = MAX0 (10, NUMCCC(I))
          ELSE IF (IOKEY(I).EQ.9) THEN
            NUMCCC(I) = 0
          ELSE
            NUMCCC(I) = NUMIN
          ENDIF
          NTOT = NTOT + NUMCCC(I)
        ELSE
          IOKEY(I) = 9
          NUMCCC(I) = 0
        ENDIF
   10 CONTINUE
*                                                             END CDRGCK
      END
