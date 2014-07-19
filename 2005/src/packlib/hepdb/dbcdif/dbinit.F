*
* $Id: dbinit.F,v 1.1.1.1 1996/02/28 16:24:58 mclareni Exp $
*
* $Log: dbinit.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:58  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBINIT (IDIV, LUNRZ, TOPNM, LTOP, NRECS, CHOPT)
*     ==========================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBINIT (IDIV, LUNRZ, TOPNM, LTOP*, NRECS, CHOPT)        *
*                                                                      *
*   Initializes of the Data Base Package (for each file)               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDIV     User Division                                           *
*     LUNRZ    RZ file unit                                            *
*     TOPNM    Name of the Top Directory                               *
*     LTOP     Address of the top directory                            *
*     NRECS    Number of records for primary allocation (for RZMAKE)   *
*              If less than or equal to 0 use existing RZ file (RZFILE)*
*     CHOPT    Character Option : Same as in RZFILE / RZMAKE           *
*          Z   Reinitialize the random access file (use RZMAKE)        *
*              (also NRECS > 0 is required)                            *
*                                                                      *
*   Called by user,   DBACTI                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = -1 : Invalid top directory name                      *
*               = -2 : The file is already open with correct LUNRZ and *
*                      TOPNM                                           *
*               = -3 : The file is already open with wrong LUNRZ or    *
*                      TOPNM                                           *
*               = -4 : Already a file is opened with the same unique   *
*                      identifier as requested for this TOPNM          *
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(defined(CERNLIB__ONLINE))
*               = -5 : Invalid process name in Online context          *
*               = -6 : Error in IC_BOOK for booking the CACHE          *
*               = -7 : Error in CC_SETUP for reserving the CLUSCOM     *
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_IBMVM)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
*               = -8 : Cannot open journal file in server context      *
#endif
#if defined(CERNLIB__P3CHILD)
*               = -9 : Unable to open FZ communication channel         *
*               = -10: Host unable to open RZ file                     *
#endif
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      CHARACTER       CHOPT*(*), TOPNM*(*)
      DIMENSION       LTOP(9)
      LOGICAL         DONE
      SAVE            DONE
      DATA            DONE /.FALSE./
*
*     ------------------------------------------------------------------
*
      IF (.NOT.DONE) THEN
        DONE   = .TRUE.
        CALL MZLINK (IDIV, '/DXUSER/', LDATDX, LDATDX, LKEYDX)
        CALL VZERO  (MASKDX, MXKYDX)
        CALL VZERO  (IOKYDX, MXKYDX)
      ENDIF
      NTOP  = IQUEST(1)
      NPAIR = 1
      LUFZ  = 0
      CALL CDINIT (IDIV, LUNRZ,LUFZ, TOPNM,NPAIR,NRECS,NTOP, CHOPT, IRC)
      LTOP(1)   = 0
      IQUEST(1) = IRC
      IF (IQUEST(1).EQ.0) LTOP(1) = LBUPCD
*                                                             END DBINIT
      END
