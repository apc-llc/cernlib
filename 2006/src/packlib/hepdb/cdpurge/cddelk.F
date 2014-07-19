*
* $Id: cddelk.F,v 1.1.1.1 1996/02/28 16:24:24 mclareni Exp $
*
* $Log: cddelk.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:24  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE CDDELK (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDDELK (IRC*)                                           *
*                                                                      *
*   Deletes objects in a given directory taking care the objects with  *
*   IPURCK(n) set to 2 and those on which these objects depend are     *
*   preserved                                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDPURG, CDPURK                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
*
*     ------------------------------------------------------------------
*
      IRC    = 0
*
* *** Label by '1' those objects one should not delete
*
      IF (IOPSCD.NE.0) CALL RZLOCK ('CDDELK')
      IPNT   = KOFSCD + LCDRCD + IKDRCD
      ISTP   = NWKYCK + 1
      I1     = 1
      NDEL   = 0
   10 IF (I1.LE.NKEYCK)  THEN
        NN     = NKEYCK - I1 + 1
        II     = IUCOMP (2, IPURCK(I1), NN)
        IF (II.GT.0)  THEN
          II     = II + I1 - 1
          I1     = II + 1
   15     IP     = IPNT + (II - 1) * ISTP
          NO1    = IQ(IP+IDHPTR)
          II     = IUCOMP (NO1, KEY1CK, NKEYCK)
          IF (II.GT.0.AND.NO1.NE.0) THEN
            IF (IPURCK(II).NE.2) IPURCK(II) = 1
            GO TO 15
          ENDIF
          GO TO 10
        ENDIF
      ENDIF
*
      DO 20 IK1 = 1, NKEYCK
        IK     = NKEYCK + 1 - IK1
        IF (IPURCK(IK).LE.0) THEN
          CALL CDKEYR (IK, NWKYCK, KEYNCK)
          CALL RZDELK (KEYNCK, ICDUM, 'C')
          NDEL   = NDEL + 1
        ENDIF
   20 CONTINUE
      IF (IOPSCD.NE.0) CALL RZFREE ('CDDELK')
      IQUEST(2) = NDEL
*                                                             END CDDELK
  999 END
#endif
