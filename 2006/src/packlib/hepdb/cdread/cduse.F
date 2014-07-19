*
* $Id: cduse.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cduse.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDUSE (PATHN, LBK, ISEL, CHOPT, IRC)
*     ================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUSE (PATHN, *LBK*, ISEL, CHOPT, IRC*)                *
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and set of Keys, unless already done.                     *
*   Returns the address in memory for the corresponding Key bank after *
*   checking its validity.                                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address(es) of Keys bank(s) KYCD   (INPUT or OUTPUT)    *
*              Address of the databank can be obtained from the actual *
*              key-address as LBD = LQ(LBK-1)                          *
*     ISEL     Instant of validity, e.g. data acquisition time         *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address(es) if non-zero                       *
*          D   drop data structure at LBK before retrieving new data   *
*              structure                                               *
*          F   force retrieval of new data structure                   *
*          K   read only the keys (no data is required)                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  2 : Illegal path name                               *
*               =  3 : Data base structure in memory clobbered         *
*               =  4 : Illegal key option                              *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information whether data part has  *
*     been actually read from the disk or not                          *
*     IQUEST(2) =  0 : No disk i/o has been performed                  *
*               =  1 : Data have been refreshed from the disk          *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
*     (Arbitary dimension 9 to force transmission by address for scalar)
      PARAMETER       (NZ=0)
      DIMENSION       ISEL(9), KEYS(9), LBK(9)
#if defined(CERNLIB__P3CHILD)
      LOGICAL         LCOND
#endif
      CHARACTER       CHOPT*(*), PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
C ACP_data_retrieval_start
      LREFCD(1) = LBK(1)
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)        GO TO 999
      IOPVCA = 1
*
* *** Create (or complete) database skeleton in memory
*                       (banks NOCD and KYCD)
*
      IF (IOPACA.EQ.0. OR. LREFCD(1).EQ.0) THEN
*
        CALL CDNODE (PATHN, IRC)
        IF (IRC.NE.0)      GO TO 999
*
        LBKYCD =LQ(KOFUCD+LBNOCD-KLKYCD)
        IF (LBKYCD.EQ.0) THEN
          ND     = IQ(KOFUCD+LBNOCD+MNDNWD)
          CALL UCOPY  (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
          CALL CDBANK (IDIVCD, LBKYCD, LBNOCD, -KLKYCD, 'KYCD', NLKYCD,
     +                 NSKYCD, ND, IOKYCD, NZ, IRC)
          IF (IRC.NE.0)    GO TO 999
          LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
          LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
        ENDIF
        LBK(1)    = LBKYCD
        LREFCD(1) = LBK(1)
*
      ELSE
*
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0)     GO TO 999
        CALL RZCDIR (PAT1CT, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
*
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          NF     = IQ(KOFUCD+LBNOCD+MNDNCH)
          CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT3CT, NF)
          PAT3CT = PAT3CT(1:NF)
          N      = LENOCC (PAT1CT) + 1
*
   50     N      = N -1
          IF (PAT1CT(N:N).NE.PAT3CT(NF:NF))  THEN
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSE : Da'//
     +      'ta-base structure in memory clobbered'')', IARGCD, 0)
            IRC        = 3
            IQUEST(11) = N
            GO TO 999
          ELSE IF (N.NE.1)  THEN
            NF     = NF -1
            GO TO 50
          ENDIF
        ENDIF
#endif
      ENDIF
*
* *** Create (or update) Data bank(s)
*
#if defined(CERNLIB__P3CHILD)
      LCOND  =  .FALSE.
      CALL CDSTP3 (1, LCOND, NBKP3, 0)
#endif
      IQUEST(2) = 0
      LBDACD = LQ(KOFUCD+LREFCD(1)-KLDACD)
      CALL CDCHCK (LREFCD(1), ISEL, KEYS, LBDACD, IRC)
*
  999 CONTINUE
#if defined(CERNLIB__P3CHILD)
      IF (LNK3P3.NE.0) CALL MZDROP (IXDBP3, LNK3P3, '....')
      LNK3P3 = 0
      LNK4P3 = 0
      LNK5P3 = 0
      NBKYP3 = 0
      NDIRP3 = 0
      IPASP3 = 0
#endif
*                                                              END CDUSE
C ACP_data_retrieval_end
      END
