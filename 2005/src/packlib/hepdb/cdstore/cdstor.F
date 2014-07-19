*
* $Id: cdstor.F,v 1.1.1.1 1996/02/28 16:24:34 mclareni Exp $
*
* $Log: cdstor.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:34  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSTOR (PATHN, LADDR, LBK, IUDIV, KEY, CHOPT, IRC)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSTOR (PATHN, LADDR, *LBK*, IUDIV, KEY, CHOPT, IRC*)   *
*                                                                      *
*   Stores data from memory to disk and also enters in the memory ala  *
*   DBUSE in NODE/KEY structure                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LADDR    Address of bank in memory where data reside             *
*     LBK      Address of Keys bank KYCD   (INPUT or OUTPUT)           *
*     IUDIV    Division index of the user data bank                    *
*     KEY      Vector of keys                                          *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          C   Create Node/Key data structure ala CDUSE                *
*          D   Store only the differences from an existing object      *
*          F   Updates with a fully matched data object (in user keys) *
*          K   Store data only inside the keys (not yet installed)     *
*          H   Insertion time as supplied by user to be honoured       *
*          P   Store data compressed (bit packing)                     *
*          T   Special text type of data (to be used with R)           *
*              (was for DBAIWR - no longer needed?)                    *
*          Y   Store with full RZ option (No compression to be made)   *
*          Z   Store only nonzero elements. An element is considered   *
*              to be zero if its absolute value is less than DELTA     *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user,   CDSTOM                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 63 : Data base structure in memory clobbered         *
*               = 64 : Error in MZCOPY while copying Data bank         *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
      DIMENSION       KEY(9), LADDR(9), LBK(9)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOP*2, PATHY*80
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      LREFCD(1) = LBK(1)
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)     GO TO 999
*
      IF (IOPFCA.NE.0) IOPDCA = 1
      IF (IOPTCA.NE.0) IOPYCA = 1
      IF (IOPYCA.NE.0) THEN
        IOPPCA = 0
        IOPZCA = 0
      ENDIF
      IF (IOPPCA.NE.0) IOPZCA = 0
*
* *** Write out the data
*
      CALL CDKOUT (PATHN, LADDR, IUDIV, KEY, KEYVCK, IRC)
      IF (IRC.NE.0)     GO TO 999
      CALL RZCDIR (PATHY, 'R')
*
* *** Create data bank in memory ala DBUSE
*
      IF (IOPCCA.NE.0)  THEN
*
*  **   Create database skeleton in memory (banks NOCD and KYCD)
*
        IF (IOPACA.EQ.0. OR. (IOPACA.NE.0. AND. LREFCD(1).EQ.0) )  THEN
*
          CALL CDBOOK (PATHY, LBK, 1, CHOPT, IRC)
          IF (IRC.NE.0) GO TO 999
          LREFCD(1) = LBK(1)
          CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LREFCD(1)+1), NWKYCK)
          NDK    = IQ(KOFUCD+LREFCD(1)-1)
          IQ(KOFUCD+LREFCD(1)+NDK+MKYFRI) = 0
          DO 20 I = 1, NPARCD
            IQ(KOFUCD+LREFCD(1)+NWKYCK+I) = KEYVCK(NOF1CK+2*I)
   20     CONTINUE
*
        ELSE
#if defined(CERNLIB__DEBUG)
*
          IF (IDEBCD.GT.0) THEN
            LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
            NF     = IQ(KOFUCD+LBNOCD+MNDNCH)
            CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT2CT, NF)
            PAT2CT = PAT2CT (1:NF)
            N      = LENOCC (PATHY) + 1
*
   25       N      = N -1
            IF (PATHY(N:N).NE.PAT2CT(NF:NF))  THEN
              IRC        = 63
              IQUEST(11) = N
              CALL CDPRNT (LPRTCD, '(/,'' CDSTOR : Database structure'//
     +             ' in memory clobbered'')', IARGCD, 0)
              GO TO 999
            ELSE IF (N.NE.1)  THEN
              NF         = NF -1
              GO TO 25
            ENDIF
          ENDIF
#endif
*
          CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LREFCD(1)+1), NWKYCK)
          NDK    = IQ(KOFUCD+LREFCD(1)-1)
          IQ(KOFUCD+LREFCD(1)+NDK+MKYFRI) = 0
          DO 30 I = 1, NPARCD
            IQ(KOFUCD+LREFCD(1)+NWKYCK+I) = KEYVCK(NOF1CK+2*I)
   30     CONTINUE
        ENDIF
*
*  **   Now copy the data banks appended at LADDR to the key bank
*
        IF (IOPYCA.EQ.0)  THEN
          CHOP   = 'NP'
        ELSE
          CHOP   = 'LP'
        ENDIF
*
        LBD    = LQ(KOFUCD+LREFCD(1)-KLDACD)
        IF (LBD.NE.0) CALL MZDROP (IDIVCD, LBD, 'L')
        IF (IOPKCA.EQ.0 .AND. LADDR(1).NE.0) THEN
          CALL MZCOPY (IUDIV, LADDR(1), IDIVCD, LREFCD(1), -KLDACD,
     +                 CHOP)
*
          IF (IQUEST(1).NE.0)  THEN
            IQUEST(11) = IQUEST(1)
            IRC        = 64
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSTOR : Err'//
     +      'or '',I6,'' while copying the Data bank in the Node/Key '//
     +      'structure'')', IQUEST(11), 1)
#endif
          ENDIF
        ENDIF
*
      ENDIF
*                                                             END CDSTOR
  999 END
