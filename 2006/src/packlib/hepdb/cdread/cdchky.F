*
* $Id: cdchky.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdchky.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCHKY (ITIME, KEYS)
*     ===============================
*
************************************************************************
*                                                                      *
*        SUBR. CDCHKY (ITIME, KEYS)                                    *
*                                                                      *
*   Checks if the current key bank is still valid                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Time for which the valid data are required              *
*     KEYS     Vector of keys                                          *
*                                                                      *
*   Called by CDCHCK                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctkxin.inc"
      DIMENSION       ITIME(9), KEYS(9)
*
*     ------------------------------------------------------------------
*
* *** Loop over the keys
*
      DO 10 IK = IMINCT, NKEYCK
        CALL CDKEYR (IK, NWKYCK, KEYVCK)
        CALL CDKSEL (ITIME, KEYS, KEYVCK, 0, ISEL, INBR)
        IF (ISEL.EQ.0.AND.MNKYCT.LE.KEYVCK(IDHKSN))  THEN
          MNKYCT = KEYVCK(IDHKSN)
          KEY6CT = KEYVCK(IDHFLG)
          IUSECT = ICURCT
        ENDIF
   10 CONTINUE
*                                                             END CDCHKY
  999 END
