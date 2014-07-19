*
* $Id: cdusedb.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdusedb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDUSEDB (PATHN, LBK, ITIME, MASK, KEYS, CHOPT, IRC)
*     ==============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUSEDB (PATHN, *LBK*, ITIME, MASK, KEYS, CHOPT, IRC*)  *
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and set of Keys, unless already done.                     *
*   Returns (optionally) the addresses in memory for the corresponding *
*   Key banks and Data banks after checking their validity for the     *
*   given time and keys.                                               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address(es) of Keys bank(s) KYCD   (INPUT or OUTPUT)    *
*              For option 'S' it is the support address of the linear  *
*              structure                                               *
*              For option 'M' with selection on user keys 8 and 9,     *
*              LBK(k) is the address corresponding to the ith Key-8    *
*              and the jth Key-9 value, where k = KEYS(8) * (j-1) + i  *
*              Address(es) of Data bank(s) DADB can be obtained from   *
*              the actual key-address as LBD = LQ(LBK-1)               *
*     ITIME    Event data acquisition time (or 0, if Data not wanted)  *
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
*              When option 'M' is declared KEYS(n) (when user Key n    *
*              is selected should contain the number of data objects   *
*              to be retrieved according to the KEYS(n) values and     *
*              the values of the key elements for Key-n to be matched  *
*              should be stored in successive KEYS(i) elements, with   *
*              i starting from NWKEY+1 (NWKEY is the number of key     *
*              elements for this directory)                            *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address(es) if non-zero                       *
*          K   read only the keys (no data is required)                *
*          M   expect multiple Key banks to be returned (only up to    *
*              a maximum of 5 user keys)                               *
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys (Options S and M are mutually exclusive) *
*          V   declare the Data as being different in size to what is  *
*              already resident in memory                              *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  1 : Illegal character option                        *
*               =  3 : Data base structure in memory clobbered         *
*               =  4 : Illegal key option                              *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information whether data part has  *
*     been actually read from the disk or not                          *
*     IQUEST(2) =  0 : No disk i/o has been performed                  *
*               =  1 : Data have been refreshed from the disk          *
*     and IQUEST(3) gives the total number of data banks               *
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
      DIMENSION       ITIME(9), KEYS(9), MASK(9), LBK(9)
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
      IF (IRC.NE.0) THEN
        IQUEST(3) = 0
        GO TO 999
      ELSE IF (IOPMCA.NE.0 .AND. IOPSCA.NE.0) THEN
        IRC    = 1
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSEDB : Illega'//
     +  'l Character option - S/M options are mutually exclusive'')',
     +  IARGCD, 0)
#endif
        IQUEST(3) = 0
        GO TO 999
      ENDIF
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Create (or complete) database skeleton in memory
*                       (banks NOCD and KYCD)
*
      IF (ITIME(1).EQ.0 .OR. IOPACA.EQ.0 .OR.
     +   (IOPACA.NE.0.AND.LREFCD(1).EQ.0) )  THEN
*
        CALL CDNODE (PATHN, IRC)
        IF (IRC.NE.0) THEN
          IQUEST(3) = 0
          GO TO 999
        ENDIF
        PATHY  = PAT1CT
*
        CALL CDKYDB (KEYS, LBK, ITIME, IRC)
        IF (IRC.NE.0) THEN
          IQUEST(3) = 0
          GO TO 999
        ENDIF
        LREFCD(1) = LBK(1)
*
*  *    That's it, when only initialisation required
*
        IF (ITIME(1).EQ.0) THEN
          IQUEST(2) = 0
          IQUEST(3) = 0
          GO TO 999
        ENDIF
*
      ELSE
*
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0) THEN
          IQUEST(3) = 0
          GO TO 999
        ENDIF
        PATHY  = PAT1CT
        CALL RZCDIR (PATHY, ' ')
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        LBNOCD = LQ(KOFUCD+LREFCD(1)-KLNOCD)
*
        IF (IOPSCA.NE.0)  THEN
          CALL CDBKKS (KEYS, LBK, ITIME, IRC)
          IF (IRC.NE.0) THEN
            IQUEST(3) = 0
            GO TO 999
          ENDIF
          LREFCD(1) = LBK(1)
        ENDIF
*
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          NF     = IQ(KOFUCD+LBNOCD+MNDNCH)
          CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT3CT, NF)
          PAT3CT = PAT3CT(1:NF)
          N      = LENOCC (PATHY) + 1
*
   50     N      = N -1
          IF (PATHY(N:N).NE.PAT3CT(NF:NF))  THEN
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSEDB : Da'//
     +      'ta-base structure in memory clobbered'')', IARGCD, 0)
            IRC        = 3
            IQUEST(3)  = 0
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
* *** Get number of Data banks needed
*
      NKYMX  = 1
      IF (IOPMCA.NE.0)  THEN
        IF (NWKYCK.GT.NSYSCK)  THEN
          DO 60 I = NSYSCK+1, NWKYCK
            IF (IOKYCA(I).NE.0) THEN
              IF (KEYS(I).LE.0)  THEN
*
*  **           Illegal key option
*
                IRC        = 4
                IQUEST(3)  = 0
                IQUEST(11) = KEYS(I)
                IQUEST(12) = I
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUSEDB '//
     +          ': Illegal key option '',I3,'' for key '',I3,'' with '//
     +          'option M'')', IQUEST(11), 2)
#endif
                GO TO 999
              ENDIF
*
              NKYMX  = NKYMX * KEYS(I)
            ENDIF
   60     CONTINUE
        ENDIF
      ENDIF
*
* *** Number of Key banks in S mode
*
      IF (IOPSCA.NE.0)  THEN
        LBKYCD = LREFCD(1)
        NKYMX  = NZBANK (IDIVCD, LBKYCD)
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
      IF (IOPSCA.EQ.0)  THEN
        LBKYCD = LBK(I)
      ENDIF
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
           CALL CDPRNT (LPRTCD, '(/,'' CDUSEDB : Fatal error - No mor'//
     +          'e space available to lift bank for'',/,''  '//PATHY//
     +          ''',2I10)', IARGCD, 2)
        ENDIF
      ENDIF
#endif
      IF (IOPSCA.EQ.0)  THEN
        IF (I.LT.NKYMX)    GO TO 100
      ELSE
        IF (IRC.NE.0) THEN
          IQUEST(3) = NKYMX
          GO TO 999
        ENDIF
        LBKYCD = LQ(KOFUCD+LBKYCD)
        IF (LBKYCD.NE.0)   GO TO 100
      ENDIF
      IQUEST(3) = NKYMX
#if defined(CERNLIB__P3CHILD)
*
      CALL CDUSP3 ('CDUSEDB', ITIME, IRC)
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
*                                                            END CDUSEDB
C ACP_data_retrieval_end
      END
