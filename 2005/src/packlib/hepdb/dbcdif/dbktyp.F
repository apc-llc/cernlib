*
* $Id: dbktyp.F,v 1.1.1.1 1996/02/28 16:24:59 mclareni Exp $
*
* $Log: dbktyp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:59  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBKTYP (PATHN, NKEYS, IOTY, CTAG)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. DBKTYP (PATHN, *NKEYS*, IOTY*, CTAG*)                   *
*                                                                      *
*   Gets the key types and their tags for a given directory            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     NKEYS    On input it carries the maximum dimension of IOTY and   *
*              CTAG; on output it carries number of key elements (if   *
*              no error; -1 if error)                                  *
*     IOTY     Type of each key element                                *
*     CTAG     Tag for each element                                    *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =101 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      CHARACTER*(*)   CTAG(*)
      CHARACTER       PATHN*(*), PATH*80
      DIMENSION       IOTY(9)
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)  THEN
        IQUEST(1) = IRC
        NKEYS     = -1
        GO TO 999
      ENDIF
      CALL CDKYTG
*
* *** Fill up the user vectors
*
      NKMAX  = MIN0 (NKEYS, NWKYCK)
      DO 10 I = 1, NKMAX
        IOTY(I) = IOTYCK(I)
        CTAG(I) = CTAGCK(I)
   10 CONTINUE
      NKEYS  = NWKYCK
*                                                             END DBKTYP
  999 END
