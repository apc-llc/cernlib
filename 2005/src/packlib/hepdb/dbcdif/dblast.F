*
* $Id: dblast.F,v 1.1.1.1 1996/02/28 16:25:00 mclareni Exp $
*
* $Log: dblast.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:00  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      LOGICAL FUNCTION DBLAST (LBK)
*     =============================
*
************************************************************************
*                                                                      *
*        FUNC. DBLAST (LBK)                                            *
*                                                                      *
*   Checks if the Key bank corresponds to the last inserted object     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LBK      Address of the key bank                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =131 : Illegal pathname in the Key bank                *
*               =132 : Illegal number of keys in the directory         *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      LOGICAL         CDLAST
      DIMENSION       LBK(9)
*     ------------------------------------------------------------------
*
      DBLAST = CDLAST (LBK, IRC)
      IQUEST(1) = IRC
*                                                             END DBLAST
  999 END
