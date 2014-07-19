*
* $Id: dbplti.F,v 1.1.1.1 1996/02/28 16:25:05 mclareni Exp $
*
* $Log: dbplti.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:05  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPLTI (PATHN, KEYS, CHOPT)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPLTI (PATHN, KEYS, CHOPT)                             *
*                                                                      *
*   Plots the validity time of data objects in a given directory       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHOPT    Character string with any of the following characters   *
*          3   selects objects with start validity time > KEYS(3)      *
*          4   selects objects with start validity time < KEYS(4)      *
*          5   specific Program version number required                *
*          7   selects objects with insertion      time < KEYS(7)      *
*          n   consider user key n (where 7 < n < 30)                  *
*                                                                      *
*   Called by user,   DBACPL                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =161 : Illegal path name                               *
*               =162 : No keys or data in the directory                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), CHOPF*28
      DIMENSION       KEYS(9)
*
*     ------------------------------------------------------------------
*
* *** Reformat CHOPT
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPXDX = 1
      CALL DBOPTM (CHOPF)
*
* *** Reformat the keys
*
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
*
      CALL CDPLTI (PATHN, MASKDX, KEYSDX, CHOPF, IRC)
      IQUEST(1) = IRC
*                                                             END DBPLTI
  999 END
