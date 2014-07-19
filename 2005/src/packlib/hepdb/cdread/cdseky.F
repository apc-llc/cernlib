*
* $Id: cdseky.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdseky.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSEKY (ITIME, KEY)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDSEKY (ITIME, KEY)                                     *
*                                                                      *
*   Select on the values of key elements                               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Time for which data are required to be valid            *
*     KEY      Vector of keys (on the basis of which selection is to   *
*              be made)                                                *
*                                                                      *
*   Called by CDKXIN                                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctkxin.inc"
      DIMENSION       ITIME(9), KEY(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Look at all the keys in the subdirectory
*
      IF (IOPSCA.NE.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        ISTP   = NWKYCK + 1
        KPNT   = IUHUNT (KEY(IDHKSN), IQ(IPNT+IDHKSN), NKEYCK*ISTP,ISTP)
        IF (KPNT.GT.0) THEN
          IK     = (KPNT - IDHKSN)/ ISTP + 1
        ELSE
          GO TO 999
        ENDIF
        CALL CDKEYR (IK, NWKYCK, KEYNCK)
        NTIMCT = NTIMCT + 1
        KEY1CK(NTIMCT) = KEYNCK(IDHKSN)
        IF (MNKYCT.LE.KEYNCK(IDHKSN))  THEN
          IKYLCT = IK
          MNKYCT = KEYNCK(IDHKSN)
          KEY6CT = KEYNCK(IDHFLG)
        ENDIF
*
      ELSE
*
        DO 20 IK = 1, NKEYCK
          CALL CDKEYR (IK, NWKYCK, KEYNCK)
          CALL CDKSEL (ITIME, KEY, KEYNCK, 1, ISEL, INBR)
          IF (ISEL.EQ.0) THEN
            NTIMCT = NTIMCT + 1
            KEY1CK(NTIMCT) = KEYNCK(IDHKSN)
            IF (MNKYCT.LE.KEYNCK(IDHKSN))  THEN
              IKYLCT = IK
              MNKYCT = KEYNCK(IDHKSN)
              KEY6CT = KEYNCK(IDHFLG)
              IF (JBIT(KEY6CT,JIGNCD).EQ.0) KY6NCT = IK
            ENDIF
          ELSE IF (INBR.EQ.0) THEN
            IDNRCT = IK
            IUSECT = ICURCT
          ENDIF
   20   CONTINUE
      ENDIF
*                                                             END CDSEKY
  999 END
