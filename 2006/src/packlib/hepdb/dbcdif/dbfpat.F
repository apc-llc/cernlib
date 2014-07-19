*
* $Id: dbfpat.F,v 1.1.1.1 1996/02/28 16:24:56 mclareni Exp $
*
* $Log: dbfpat.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:56  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBFPAT (CPATH, LEVMX, LAD)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. DBFPAT (CPATH, LEVMX, LAD*)                             *
*                                                                      *
*   Store the information of the directory tree CPATH in a bank        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CPATH    The pathname of the directory tree about which the      *
*              information has to be provided                          *
*     LEVMX    Number of levels below CPATH about which information    *
*              has to be accumulated                                   *
*     LAD(*)   Address of the bank containing the information          *
*                                                                      *
*   Called by DBKEPT, DBACPL                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =161 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      DIMENSION       LAD(9)
      CHARACTER       CPATH*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDFPAT (CPATH, LEVMX, LAD, IRC)
      IQUEST(1) = IRC
*                                                             END DBFPAT
  999 END
