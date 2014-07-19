*
* $Id: cdgnode.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdgnode.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CD_G_NODE (NAME, LENGTH)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CD_G_NODE (NAME*, LENGTH*)                              *
*                                                                      *
*   Finds the name of the node where the program is running            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NAME     Name of the node                                        *
*     LENGTH   Length of the character string NAME                     *
*                                                                      *
*   Called by CD_CHK_CLIENT                                            *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   NAME
      INCLUDE '($LNMDEF)'
      STRUCTURE /ITMLST/
         UNION
            MAP
               INTEGER*2 BUFLEN
               INTEGER*2 CODE
               INTEGER*4 BUFADR
               INTEGER*4 RETLENADR
            END MAP
            MAP
               INTEGER*4 END_LIST
            END MAP
         END UNION
      END STRUCTURE
      RECORD /ITMLST/ TRN_LIST(2)
      INTEGER*4 STATUS,SYS$TRNLNM
*
*     ------------------------------------------------------------------
*
      TRN_LIST(1).BUFLEN    = 15
      TRN_LIST(1).CODE      = LNM$_STRING
      TRN_LIST(1).BUFADR    = %LOC(NAME)
      TRN_LIST(1).RETLENADR = %LOC(LENGTH)
      TRN_LIST(2).END_LIST  = 0
*
      STATUS = SYS$TRNLNM (,'LNM$SYSTEM_TABLE','SYS$NODE',,TRN_LIST)
      IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
*                                                          END CD_G_NODE
      END
