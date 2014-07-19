*
* $Id: cdsave.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdsave.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB_OLD)
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSAVE (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDSAVE (IRC*)                                           *
*                                                                      *
*   Sends the spool file to the server for updating the data base.     *
*   Useful only for interactive programs running on IBM                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
*     ------------------------------------------------------------------
*
      IRC    = 0
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
      CALL CDSTSV (' ', 0, IRC)
#endif
*                                                             END CDSAVE
      END
#endif
