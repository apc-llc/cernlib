*
* $Id: dbgnam.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbgnam.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBGNAM (PATH, CHTAG, IOBJ)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. DBGNAM (PATH, CHTAG, IOBJ*)                             *
*                                                                      *
*   Identifies the object element number from its name and the name    *
*   of the directory path name                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     CHTAG    Name of the data element                                *
*     IOBJ(*)  Object element number (0 if not found)                  *
*                                                                      *
*   Called by DBACPL                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =182 : Illegal path name                               *
*               =201 : DICTIONARY directory not found                  *
*               =202 : No description of data elements for the given   *
*                      path name exists in the data base               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHTAG*(*), PATH*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDGNAM (PATH, CHTAG, IOBJ, IRC)
      IQUEST(1) = IRC
*                                                             END DBGNAM
  999 END
