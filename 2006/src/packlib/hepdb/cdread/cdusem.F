*
* $Id: cdusem.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdusem.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDUSEM (PATHN, LBK, ITIME, MASK, KEYS, CHOPT, IRC)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUSEM (PATHN, *LBK*, ITIME, MASK, KEYS, CHOPT, IRC*)   *
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and set of Keys, unless already done.                     *
*   Returns the addresses in memory for the corresponding Key banks    *
*   after checking their validity for the given time and keys.         *
*   The address of the databank can only be obtained from the actual   *
*   key-address of the linear structure as LBD = LQ(LBK-1)             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address of the first Key bank KYCD of a linear structure*
*              (INPUT or OUTPUT)                                       *
*     ITIME    Event data acquisition time                             *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity smaller than *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with end validity larger than those in   *
*              KEYS. If MASK corresponding to time of insertion is set,*
*              objects inserted earlier than KEYS(IDHINS) are selected *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*              When option 'M' is declared, KEYS vector is ignored.    *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address(es) if non-zero                       *
*          D   drop data structure at LBK before retrieving new data   *
*              structure                                               *
*          F   force retrieval of new data structure                   *
*          K   read only the keys (no data is required)                *
*          M   expect multiple Key banks to be returned (only up to    *
*              a maximum of 5 user keys)                               *
*          N   If data do not exist for ITIME, take the nearest data   *
*              object in time                                          *
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys (Options S and M are mutually exclusive) *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  1 : Illegal character option                        *
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
*
      DIMENSION       ITIME(9), MASK(9), KEYS(9), LBK(9)
#if defined(CERNLIB__P3CHILD)
      LOGICAL         LCOND
#endif
      CHARACTER       CHOPT*(*), PATHN*(*), PATHY*80
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
C ACP_data_retrieval_start
      LREFCD(1) = LBK(1)
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                 GO TO 999
      IF ((IOPMCA.NE.0.AND.IOPSCA.NE.0) .OR.
     +    (IOPMCA.NE.0.AND.LREFCD(1).EQ.0))  THEN
        IRC    = 1
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          PATHY  = CHOPT
          NCH    = LENOCC (PATHY)
          CALL CDPRNT (LPRTCD, '(/,'' CDUSEM : Illegal Character opti'//
     +         'on '//PATHY(1:NCH)//' '')', IARGCD, 0)
        ENDIF
#endif
        GO TO 999
      ENDIF
      IOPVCA = 1
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Create (or complete) database skeleton in memory
*
      IF (IOPMCA.EQ.0 .AND. (IOPACA.EQ.0 .OR. LREFCD(1).EQ.0))  THEN
*
        CALL CDNODE (PATHN, IRC)
        IF (IRC.NE.0)               GO TO 999
        PATHY  = PAT1CT
*
      ELSE
*
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0)               GO TO 999
        PATHY  = PAT1CT
        CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
#if defined(CERNLIB__DEBUG)
*
        IF (IDEBCD.GT.0) THEN
          NF     = IQ(KOFUCD+LBNOCD+MNDNCH)
          CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT3CT, NF)
          PAT3CT = PAT3CT(1:NF)
          N      = LENOCC (PATHY) + 1
*
   50     N      = N -1
          IF (PATHY(N:N).NE.PAT3CT(NF:NF))  THEN
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSEM : Da'//
     +      'ta-base structure in memory clobbered'')', IARGCD, 0)
            IRC        = 3
            IQUEST(11) = N
            GO TO 999
          ELSE IF (N.NE.1) THEN
            NF     = NF -1
            GO TO 50
          ENDIF
        ENDIF
#endif
      ENDIF
*
      IF (IOPMCA.EQ.0 .AND. (IOPACA.EQ.0 .OR. LREFCD(1).EQ.0))  THEN
*
        CALL CDKEYB (KEYS, LBK, ITIME, IRC)
        IF (IRC.NE.0)               GO TO 999
        LREFCD(1) = LBK(1)
*
      ELSE IF (IOPSCA.NE.0) THEN
        CALL CDBKKS (KEYS, LBK, ITIME, IRC)
        IF (IRC.NE.0)               GO TO 999
      ENDIF
*
* *** Get number of Data banks needed
*
      LBKYCD = LREFCD(1)
      IF (IOPSCA.NE.0.OR.IOPMCA.NE.0)  THEN
        NKYMX  = NZBANK (IDIVCD, LBKYCD)
      ELSE
        NKYMX  = 1
      ENDIF
*
* *** Create (or update) Data bank(s)
*
      IQUEST(2) = 0
#if defined(CERNLIB__P3CHILD)
      LCOND  =  (NKYMX.GT.1)
      CALL CDSTP3 (1, LCOND, NBKP3, 0)
#endif
      I      = 0
  100 I      = I + 1
      LBDACD = LQ(KOFUCD+LBKYCD-KLDACD)
#if defined(CERNLIB__P3CHILD)
      NBKP3  = NBKYP3
#endif
      CALL CDCHCK (LBKYCD, ITIME, KEYS, LBDACD, IRC)
#if defined(CERNLIB__P3CHILD)
      IF (IPASP3.EQ.1.AND.IRC.EQ.0) CALL CDSTP3 (2, LCOND, NBKP3, I)
#endif
#if defined(CERNLIB__DEBUG)
      IF (IRC.EQ.99) THEN
         IF (IDEBCD.GT.0) THEN
           IARGCD(1) = I
           IARGCD(2) = NKYMX
           CALL CDPRNT (LPRTCD, '(/,'' CDUSEM : Fatal error - No mor'//
     +          'e space available to lift bank for'',/,''  '//PATHY//
     +          ''',2I10)', IARGCD, 2)
        ENDIF
      ENDIF
#endif
      IF (IOPSCA.NE.0.AND.IRC.NE.0) GO TO 999
      IF (IOPSCA.NE.0.OR.IOPMCA.NE.0) THEN
        LBKYCD = LQ(KOFUCD+LBKYCD)
        IF (LBKYCD.NE.0)            GO TO 100
      ENDIF
#if defined(CERNLIB__P3CHILD)
*
      CALL CDUSP3 ('CDUSEM', ITIME, IRC)
#endif
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
*                                                             END CDUSEM
C ACP_data_retrieval_end
      END
