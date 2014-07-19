*
* $Id: dbsave.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbsave.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBSAVE
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. DBSAVE                                                  *
*                                                                      *
*   Sends the spool file to the server for updating the data base.     *
*   Useful only for interactive programs running on IBM                *
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
*     ------------------------------------------------------------------
*
      IRC = 0
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
      CALL CDSTSV (' ', 0, IRC)
#endif
*     CALL CDSAVE (IRC)
      IQUEST(1) = IRC
*                                                             END DBSAVE
      END
