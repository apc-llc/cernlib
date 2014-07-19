*
* $Id: dbinct.F,v 1.1.1.1 1996/02/28 16:24:58 mclareni Exp $
*
* $Log: dbinct.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:58  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBINCT (IDTMI, ISADD, IDTMO)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBINCT (IDTMI, ISADD, IDTMO*)                           *
*                                                                      *
*   Converts packed date and time (ala DBPKTS) to a similar packed     *
*   number after adding a fixed time in seconds                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDTMI    Packed date-time (ala DBPKTS) on input                  *
*     ISADD    Incremental time to IDTMI in seconds                    *
*     IDTMO    Packed date-time (ala DBPKTS) on output                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
************************************************************************
*
      CALL CDINCT (IDTMI, ISADD, IDTMO, IRC)
*                                                             END DBINCT
      END
