*
* $Id: dbfzup.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbfzup.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE DBFZUP (LUNFZ, CHOPT)
*     ================================
*
************************************************************************
*                                                                      *
*        SUBR. DBFZUP (LUNFZ, CHOPT)                                   *
*                                                                      *
*   Updates the data base from transcript file from unit LUNFZ         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNFZ    Logical unit number of the FZ file                      *
*     CHOPT    Character string with any of the following characters   *
*          F   Force continuation beyond errors                        *
*          O   Updating journal file is an external source             *
*          S   Updating journal file in a single record mode           *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               =141 : Read error on the FZ file                       *
*               =142 : Top directory name illegal in the transcript    *
*                      file                                            *
*               =143 : Illegal path name in the transcript file        *
*               = 78 : Illegal number of keys in journal file          *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHOPT*(*)
*
      CALL CDFZUP (LUNFZ, CHOPT, IRC)
      IQUEST(1) = IRC
*
      END
#endif
