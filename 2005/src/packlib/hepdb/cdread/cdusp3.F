*
* $Id: cdusp3.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdusp3.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
      SUBROUTINE CDUSP3 (CROOT, ITIME, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUSP3 (CROOT, ITIME, *IRC*)                            *
*                                                                      *
*   Retrieves several objects in memory in one transaction from host   *
*   to child in P3 context                                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CROOT    Name of the calling routine                             *
*     ITIME    Event data acquisition time                             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDGETDB,CDUSEDB,CDUSEM                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  5 : Error in CDCHLD in P3 communication             *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/clinks.inc"
#include "hepdb/p3dbl3.inc"
#include "zebra/mzbits.inc"
*
      DIMENSION       ITIME(9)
      CHARACTER       CROOT*(*)
*
*     ------------------------------------------------------------------
*
      IQ1    = IRC
      IQ2    = IQUEST(2)
      IQ3    = IQUEST(3)
      IF (IPASP3.NE.1)          GO TO 30
*
* *** Request the host to load the data objects from the RZ file
*
      IF (NBKYP3.EQ.0) THEN
        IQ2    = 0
        IQ3    = 0
        GO TO 30
      ENDIF
*
      RNDBP3 = 'CDRZIN  '
      PAT1CT = CROOT
      CALL UCTOH  (PAT1CT, IWDBP3, 4, 8)
      CALL CDCHLD
      IF (IQDBP3.NE.0) THEN
        IQ1    = 5
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSP3 : Error '//
     +  'code'',I6,'' from CDCHLD'')', IQDBP3, 1)
#endif
        GO TO 30
      ENDIF
*
* *** Relocate the objects
*
      IPASP3 = 2
      LNK6P3 = LQ(KOFUCD+LNK3P3-1)
      LNK7P3 = LQ(KOFUCD+LNK3P3-2)
*
      DO 20 JK = 1, NKBP3
        IP3NEW = IQ(KOFUCD+LNK2P3+JK)
        I      =      IP3NEW/1000000
        IP3NEW = MOD (IP3NEW,1000000)
        IF (JK.GT.1) THEN
          IF (IP3NEW.EQ.IP3OLD) GO TO 20
          DO 10 IP3 = IP3OLD, IP3NEW-1
            LNK4P3 = LQ(KOFUCD+LNK6P3)
            LNK5P3 = LQ(KOFUCD+LNK7P3)
            CALL MZDROP (IXDBP3, LNK6P3, 'BV..')
            CALL MZDROP (IXDBP3, LNK7P3, 'BV..')
            LNK6P3 = LNK4P3
            LNK7P3 = LNK5P3
   10     CONTINUE
        ELSE
          LNK4P3 = LNK6P3
          LNK5P3 = LNK7P3
        ENDIF
        IP3OLD = IP3NEW
*
        LBKYCD = LQ(KOFUCD+LNK2P3-JK)
        LBDACD = LQ(KOFUCD+LBKYCD-KLDACD)
        NDK    = IQ(KOFUCD+LBKYCD-1)
        IF (LBDACD.NE.0) CALL MZDROP (IDIVCD, LBDACD, 'L...')
*
        NWKEY  = IQ(KOFUCD+LBKYCD-1)
        CALL UCOPY (IQ(KOFUCD+LBKYCD+1), KEYVCK, NWKEY)
        CALL CDKXIN (ITIME, IDIVCD, LAUXCL(9), LBKYCD, -KLDACD, NWKEY,
     +               KEYVCK, IPREC, IRC)
        LAUXCL(9) = 0
*
        IF (IRC.EQ.0) IQ(KOFUCD+LBKYCD+NDK+MKYPRE) = IPREC
        IF (IRC.NE.0.AND.IQ1.EQ.0) IQ1 = IRC
   20 CONTINUE
*
   30 IRC       = IQ1
      IQUEST(2) = IQ2
      IQUEST(3) = IQ3
*                                                             END CDUSP3
      END
#endif
