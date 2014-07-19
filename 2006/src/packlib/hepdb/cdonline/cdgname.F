*
* $Id: cdgname.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdgname.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CD_G_NAME (NAME, LENGTH)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CD_G_NAME (NAME*, LENGTH*)                              *
*                                                                      *
*   Finds the task name in the character variable NAME                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     NAME     Name of the task                                        *
*     LENGTH   Length of the character string NAME                     *
*                                                                      *
*   Called by CD_CHK_CLIENT                                            *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   NAME
      INCLUDE '($JPIDEF)'
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
      RECORD /ITMLST/ JPI_LIST(2)
      INTEGER*4 STATUS,SYS$GETJPIW
*
*     ------------------------------------------------------------------
*
      JPI_LIST(1).BUFLEN    = 15
      JPI_LIST(1).CODE      = JPI$_PRCNAM
      JPI_LIST(1).BUFADR    = %LOC(NAME)
      JPI_LIST(1).RETLENADR = %LOC(LENGTH)
      JPI_LIST(2).END_LIST  = 0
*
      STATUS = SYS$GETJPIW (,,,JPI_LIST,,,)
      IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
*
      STATUS = SYS$GETJPIW (,,,JPI_LIST,,,)
      IF (.NOT. STATUS) CALL LIB$SIGNAL (%VAL(STATUS))
*                                                          END CD_G_NAME
      END
