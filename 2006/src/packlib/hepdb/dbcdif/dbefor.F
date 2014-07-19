*
* $Id: dbefor.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbefor.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBEFOR (TOPN, IDATE, ITIME)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBEFOR (TOPN, IDATE, ITIME)                             *
*                                                                      *
*   Set the maximum insertion time for retrieval of all subsequent     *
*   data objects for a given top directory                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory ('*' means all)               *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       TOPN*(*)
*     ------------------------------------------------------------------
*
      CALL CDBFOR (TOPN, IDATE, ITIME, IRC)
*                                                             END DBEFOR
      END
