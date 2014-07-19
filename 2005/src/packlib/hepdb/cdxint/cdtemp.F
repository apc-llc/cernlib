*
* $Id: cdtemp.F,v 1.1.1.1 1996/02/28 16:24:52 mclareni Exp $
*
* $Log: cdtemp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTEMP (IOKEY, NK, CHEAD)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTEMP (IOKEY, NK, *CHEAD)                              *
*                                                                      *
*   Writes a Template of the Display in Horizontal Mode into           *
*   the Character Variable CHEAD                                       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
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
*     NK       Number of Keys                                          *
*     CHEAD    Character variable containing the header                *
*                                                                      *
*   Called by CDDISH                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       IOKEY(9)
      CHARACTER       CHEAD*(*)
*
*     ------------------------------------------------------------------
*
      IPOS = 11
      DO 5 I = 1,NK
        IF (IOKEY(I).NE.9) THEN
          IW1 = IPOS
          IW2 = IPOS + NUMCCC(I) - 1
          CHEAD(IW1:IW1) = '|'
          CHEAD(IW2:IW2) = '|'
          IPOS = IPOS + NUMCCC(I)
        ENDIF
    5 CONTINUE
*                                                             END CDTEMP
      END
