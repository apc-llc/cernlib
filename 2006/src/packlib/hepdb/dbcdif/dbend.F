*
* $Id: dbend.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbend.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBEND
*     ================
*
************************************************************************
*                                                                      *
*        SUBR. DBEND                                                   *
*                                                                      *
*   Terminates the Data Base utility package                           *
*                                                                      *
*   Called by user,   DBMAIN, DBTERM, DBACTI                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
*
*     ------------------------------------------------------------------
*
      CALL CDEND ('*', 'A', IRC)
      IQUEST(1) = IRC
*                                                              END DBEND
  999 END
