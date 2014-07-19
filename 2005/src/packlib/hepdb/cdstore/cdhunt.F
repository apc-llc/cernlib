*
* $Id: cdhunt.F,v 1.1.1.1 1996/02/28 16:24:33 mclareni Exp $
*
* $Log: cdhunt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDHUNT (KEYV)
*     ========================
*
************************************************************************
*                                                                      *
*        SUBR. CDHUNT (KEYV*)                                          *
*                                                                      *
*   Returns the key elements of an object with matching user keys as   *
*   in KEYVCK and with Key(2) = 0 (not updated)                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYV(*)  Key vector found (if none found, set KEYV(1) = 0)       *
*                                                                      *
*   Called by CDDONT                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       KEYV(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Look at all the keys in the subdirectory
*
      DO 20 IK1 = 1, NKEYCK
        IK     = NKEYCK - IK1 + 1
        CALL CDKEYR (IK, NWKYCK, KEYV)
*
        IF (KEYV(IDHPTR).NE.0)              GO TO 20
        IF (KEYV(IDHUSI).NE.KEYVCK(IDHUSI)) GO TO 20
        DO 10 I = NSYSCK+1, NWKYCK
          IF (KEYV(I).NE.KEYVCK(I))         GO TO 20
   10   CONTINUE
        IF (JBIT(KEYV(IDHFLG),JRZUCD).EQ.0) GO TO 20
        GO TO 999
   20 CONTINUE
      KEYV(1) = 0
*                                                             END CDHUNT
  999 END
