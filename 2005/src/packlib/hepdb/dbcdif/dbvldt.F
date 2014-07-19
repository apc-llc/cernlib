*
* $Id: dbvldt.F,v 1.1.1.1 1996/02/28 16:25:03 mclareni Exp $
*
* $Log: dbvldt.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:03  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBVLDT (ITMIN, ITMAX)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. DBVLDT (ITMIN*, ITMAX*)                                 *
*                                                                      *
*   Finds the overlapping validity range of all data base objects      *
*   used since the last call to DBTBCR.                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITMIN    Lower bound of the validity range                       *
*     ITMAX    Upper bound of the validity range                       *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      DIMENSION       IVECT(2*MXPACD)
*
*     ------------------------------------------------------------------
*
      CALL CDVALID (IVECT, IRC)
      ITMAX  = IVECT(2)
      ITMIN  = IVECT(1)
      IQUEST(1) = IRC
*                                                             END DBVLDT
      END
