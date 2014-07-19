*
* $Id: cdfzlo.F,v 1.1.1.1 1996/02/28 16:24:17 mclareni Exp $
*
* $Log: cdfzlo.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:17  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFZLO (LUN, IFND)
*     =============================
*
************************************************************************
*                                                                      *
*        SUBR. CDFZLO (LUN, IFND*)                                     *
*                                                                      *
*   Sees if the FZ file is already opened                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*     IFND     Return flag (0 if not found; 1 if found)                *
*                                                                      *
*   Called by CDINIT                                                   *
*                                                                      *
************************************************************************
*
#include "zebra/mqsys.inc"
#include "zebra/eqlqf.inc"
*
*     ------------------------------------------------------------------
*
* *** Loop over control banks
*
      LQFF   = LQFS
   10 IF (LQFF.NE.0) THEN
        IF (IQ(KQSP+LQFF-5).EQ.LUN) THEN
          IFND   = 1
        ELSE
          LQFF   = LQ(KQSP+LQFF)
          GO TO 10
        ENDIF
      ELSE
        IFND   = 0
      ENDIF
*                                                             END CDFZLO
      END
