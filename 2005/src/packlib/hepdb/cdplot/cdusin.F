*
* $Id: cdusin.F,v 1.1.1.1 1996/02/28 16:24:24 mclareni Exp $
*
* $Log: cdusin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:24  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUSIN
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDUSIN                                                  *
*                                                                      *
*   Default user routine for action needed in between 2 plots          *
*                                                                      *
*   Called by CDPLOB, CDPLOV                                           *
*                                                                      *
************************************************************************
*
      IWKST  = 1
      CALL IGSA (IWKST)
*                                                             END CDUSIN
      END
