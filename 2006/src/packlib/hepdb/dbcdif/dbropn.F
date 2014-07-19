*
* $Id: dbropn.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbropn.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBROPN (LUN, CHOPT, CFNAM, NRECL, IOERR)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBROPN (LUN, CHOPT, CFNAM, NRECL, IOERR*)               *
*                                                                      *
*   Opens a random access file                                         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number                                     *
*     CHOPT    Character string with any of the following characters   *
*          O   Output file (default is input)                          *
*          Z   File to be overwritten if exists                        *
*     CFNAM    File name                                               *
*     NRECL    Record length in bytes                                  *
*     IOERR    Error code (0 if no error)                              *
*                                                                      *
*   Called by DBEXAMn                                                  *
*                                                                      *
************************************************************************
*
      CHARACTER       CHOPT*(*), CFNAM*(*)
*
      CALL CDROPN (LUN, CHOPT, CFNAM, NRECL, IOERR)
*                                                             END DBROPN
      END
