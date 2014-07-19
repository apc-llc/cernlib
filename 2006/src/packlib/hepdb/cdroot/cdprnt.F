*
* $Id: cdprnt.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdprnt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDPRNT (LUN, CFORM, IARG, NARG)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPRNT (LUN, CFORM, IARG, NARG)                         *
*                                                                      *
*   Prints a vector IARG of NARG elements on logical unit LUN          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number of the output                       *
*     CFORM    Format specification                                    *
*     IARG     Array of numbers to be printed                          *
*     NARG     Number of elements in array IARG                        *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
************************************************************************
*
      CHARACTER       CFORM*(*)
      DIMENSION       IARG(9)
*
*     ------------------------------------------------------------------
*
#if !defined(CERNLIB__P3CHILD)
      IF(NARG.LE.0) THEN
         WRITE (LUN, CFORM, ERR=999)
      ELSE
         WRITE (LUN, CFORM, ERR=999) (IARG(I), I = 1, NARG)
      ENDIF
#endif
#if defined(CERNLIB__P3CHILD)
      CALL UTPRNT (LUN, CFORM, IARG, NARG)
#endif
*                                                             END CDPRNT
  999 END
