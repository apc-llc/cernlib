*
* $Id: dbplov.F,v 1.1.1.1 1996/02/28 16:25:05 mclareni Exp $
*
* $Log: dbplov.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:05  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPLOV (PATHN, KEYS, NOBJ, KOBJ, NST, CHOPT)
*     =======================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPLOV (PATHN, KEYS, NOBJ, KOBJ, NST, CHOPT)            *
*                                                                      *
*   Plots data elemnet(s) versus data element(s) for a given path name *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     NOBJ     Number of data objects to be plotted                    *
*     KOBJ     Vector specifying the element numbers to be plotted     *
*     NST      Step size for selection of object number                *
*     CHOPT    Character string with any of the following characters   *
*          L   line to be drawn through the points                     *
*          P   a symbol to be drawn at each point                      *
*              (If L or P not chosen, a default marker to be drawn     *
*               at each point)                                         *
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
*               =163 : Illegal number of objects                       *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), CHOPF*28
      DIMENSION       KEYS(9), KOBJ(2,9)
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
      CALL CDPLOV (PATHN, MASKDX, KEYSDX, NOBJ, KOBJ, NST, CHOPF, IRC)
      IQUEST(1) = IRC
*                                                             END DBPLOV
  999 END
