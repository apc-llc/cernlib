*
* $Id: dbfzop.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbfzop.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBFZOP (LUNFZ, TOPN, CHOPT)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBFZOP (LUNFZ, TOPN, CHOPT)                             *
*                                                                      *
*   Sets the output FZ file number as the sequential backup file for   *
*   a given top directory. One can have up to two backup files, one    *
*   for standard journaling, the other for special backups.            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNFZ    Logical unit number of the FZ file                      *
*     TOPN     Top directory name to which the FZ file refers          *
*     CHOPT    Character string with any of the following characters   *
*          B   Specify a special backup file                           *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 60 : Illegal top directory name                      *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       TOPN*(*), CHOPT*(*)
*
      CALL CDFZOP (LUNFZ, TOPN, CHOPT, IRC)
      IQUEST(1) = IRC
*
      END
