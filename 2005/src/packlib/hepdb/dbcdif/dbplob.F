*
* $Id: dbplob.F,v 1.1.1.1 1996/02/28 16:25:05 mclareni Exp $
*
* $Log: dbplob.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:05  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPLOB (PATHN, KEYS, NOBJ, KOBJ, KEX, NST, CHOPT)
*     ============================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPLOB (PATHN, KEYS, NOBJ, KOBJ, KEX, NST, CHOPT)       *
*                                                                      *
*   Plots data elemnet(s) versus a key element for a given path name   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     NOBJ     Number of data objects to be plotted                    *
*     KOBJ     Vector specifying the element numbers to be plotted     *
*     KEX      Key index for the abcissa                               *
*     NST      Step size for selection of object number                *
*     CHOPT    Character string with any of the following characters   *
*          L   a line to be drawn through the points                   *
*              (needed only when symbol and line both to be drawn)     *
*          P   a symbol to be drawn at each point                      *
*              (Default is a line to be drawn through the points)      *
*          S   all elements shown on the same plot                     *
*              (Default is a seperate plot for each variable)          *
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
      DIMENSION       KEYS(9), KOBJ(9)
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
      IF (KEX.EQ.1) THEN
        KEYXT = IDHKSN
      ELSE IF (KEX.EQ.2) THEN
        KEYXT = IDHPTR
      ELSE IF (KEX.EQ.3) THEN
        KEYXT = NOF1CK + 1
      ELSE IF (KEX.EQ.4) THEN
        KEYXT = NOF1CK + 2
      ELSE IF (KEX.EQ.5) THEN
        KEYXT = IDHUSI
      ELSE IF (KEX.EQ.6) THEN
        KEYXT = IDHFLG
      ELSE IF (KEX.EQ.7) THEN
        KEYXT = IDHINS
      ELSE
        KEYXT = KEX - NSYSDX + NOF2CK + 2
      ENDIF
*
      CALL CDPLOB (PATHN, MASKDX,KEYSDX, NOBJ,KOBJ,KEYXT,NST, CHOPF,IRC)
      IQUEST(1) = IRC
*                                                             END DBPLOB
  999 END
