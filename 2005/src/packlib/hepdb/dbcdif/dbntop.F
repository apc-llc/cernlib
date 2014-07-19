*
* $Id: dbntop.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbntop.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBNTOP (PATHI, PATHO, MXKP0, NSAV, CHOPT)
*     ====================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBNTOP (PATHI, PATHO, MXKP0, NSAV, CHOPT)               *
*                                                                      *
*   Transforms the contents of a non-partitioned directory to a        *
*   partitioned directory.                                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Character string describing the input  pathname         *
*     PATHO    Character string describing the output pathname         *
*     MXKP0    Maximum number of objects in the partitioned directory  *
*     NSAV     Maximum number of objects to be entered in one go       *
*              (default is 20)                                         *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          F   Updates with a fully matched data object (in user keys) *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 68 : Input directory is partitioned                  *
*               = 71 : Illegal path name                               *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 75 : Cannot find the Top directory name in pathname  *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATHI*(*), PATHO*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDPART (PATHI, PATHO, MXKP0, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                             END DBNTOP
  999 END
