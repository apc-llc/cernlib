*
* $Id: dbprgd.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbprgd.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPRGD (PATHN, IKEEP, CHOPT)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPRGD (PATHN, IKEEP, CHOPT)                            *
*                                                                      *
*   Deletes partitions in a partitioned directory except the last      *
*   (first) few ones.                                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IKEEP    Number of partitions to be preserved                    *
*              (If -ve the last -IKEEP partitions deleted)             *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*                                                                      *
*   Called by user,   DBFZUP, DBAUXI                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 69 : Input directory is not partitioned              *
*               = 70 : Error in deleting a partition                   *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 75 : Cannot find the Top directory name in pathname  *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHN*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDPURP (PATHN, IKEEP, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBPRGD
  999 END
