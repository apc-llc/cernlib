*
* $Id: cdhead.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdhead.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDHEAD (IOKEY, NK, CHEAD)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDHEAD (IOKEY, NK, *CHEAD)                              *
*                                                                      *
*   Writes the Header of the Display in Horizontal Mode into the       *
*   Character Variable CHEAD                                           *
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
*   Called by CDDISD, CDDISH                                           *
*                                                                      *
************************************************************************
*
#include "hepdb/ckkeys.inc"
#include "hepdb/ccdisp.inc"
      DIMENSION       IOKEY(NK)
      CHARACTER       CHEAD*(*)
*
*     ------------------------------------------------------------------
*
      IPOS = 11
      DO 5 I = 1,NK
        IF (IOKEY(I).NE.9) THEN
          NSP = MAX0 ((NUMCCC(I)-8)/2,1)
          IW = IPOS + NSP
          CHEAD(IW:IW+7) = CTAGCK(I)
          IPOS = IPOS + NUMCCC(I)
        ENDIF
    5 CONTINUE
*                                                             END CDHEAD
      END
