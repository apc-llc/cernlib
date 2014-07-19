*
* $Id: dbrky1.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbrky1.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBRKY1 (PATHN, KEY1S, NKEY1)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBRKY1 (PATHN, KEY1S*, *NKEY1*)                         *
*                                                                      *
*   Retrieves all the Key 1 values for the directory PATHN             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEY1S    Vector containing the Key 1 values                      *
*     NKEY1    On input contains the maximum number of elements to be  *
*              stored in KEY1S; on return it will contain the true     *
*              number of objects                                       *
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
#include "hepdb/ctpath.inc"
      DIMENSION       KEY1S(9)
      CHARACTER       PATHN*(*), PATHY*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      NKMAX  = NKEY1
      NKEY1  = 0
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)    GO TO 991
      PATHY  = PAT1CT
      NCHR   = LENOCC (PATHY)
      ISTP   = NWKYCK + 1
      CALL CDKEYT
      IF (NKEYCK.EQ.0) GO TO 999
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
*  ** Find all key 1 values
*
      IF (IOPTP.EQ.0) THEN
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        DO 10 IK = 1, NKEYCK
          NKEY1  = NKEY1 + 1
          IF (NKEY1.LE.NKMAX) KEY1S(NKEY1) = IQ(IPNT+IDHKSN)
          IPNT   = IPNT + ISTP
   10   CONTINUE
      ELSE
        NKEYS  = NKEYCK
        DO 20 IKK = 1, NKEYS
          CALL CDPATH (TOP1CT, IKK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 101
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBRKY1 : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 991
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          DO 15 IK = 1, NKEYCK
            NKEY1  = NKEY1 + 1
            IF (NKEY1.LE.NKMAX) KEY1S(NKEY1) = IQ(IPNT+IDHKSN)
            IPNT   = IPNT + ISTP
   15     CONTINUE
   20   CONTINUE
      ENDIF
*
  991 IQUEST(1) = IRC
*                                                             END DBRKY1
  999 END
