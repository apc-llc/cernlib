*
* $Id: dboptm.F,v 1.1.1.1 1996/02/28 16:25:03 mclareni Exp $
*
* $Log: dboptm.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:03  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBOPTM (CHOPT)
*     =========================
*
************************************************************************
*                                                                      *
*        SUBR. DBOPTM (CHOPT*)                                         *
*                                                                      *
*   Encode the content of DXUSED into a character option               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHOPT    Character option to be supplied to HEPDB                *
*                                                                      *
*   Called by various routines in the patch DBCDFC                     *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "dxused.inc"
      DIMENSION       IOPTS(26)
      EQUIVALENCE     (IOPADX, IOPTS(1))
      CHARACTER       CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      NDOP   = 0
      CHOPT  = ' '
      DO 10 I = 1, 26
        IF (IOPTS(I).NE.0) THEN
          IF (NDOP.EQ.0) THEN
            CHOPT  = CALFCA(I)
          ELSE
            CHOPT  = CHOPT(1:NDOP)//CALFCA(I)
          ENDIF
          NDOP   = NDOP + 1
        ENDIF
   10 CONTINUE
*                                                             END DBOPTM
      END
