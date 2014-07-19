*
* $Id: dbuse.F,v 1.1.1.1 1996/02/28 16:25:03 mclareni Exp $
*
* $Log: dbuse.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:03  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBUSE (PATHN, LBK, LBD, ITIME, KEYS, CHOPT)
*     ======================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBUSE (PATHN, *LBK*, LBD*, ITIME, KEYS, CHOPT)          *
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
*     LBD      Address(es) of Data bank(s) DADB                        *
*              For option 'S' the address of the databank can only be  *
*              obtained from the actual key-address: LK  of the linear *
*              structure as LBD = LQ(LBK-1)                            *
*     ITIME    Event data acquisition time (or 0, if Data not wanted)  *
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
*          3   selects objects with start validity time < KEYS(3)      *
*              (with option S)                                         *
*          4   selects objects with end validity time > KEYS(4)        *
*              (with option S)                                         *
*          5   specific Program version number required                *
*          7   select objects with insertion time < KEYS(7)            *
*          n   consider user key n (where 7 < n < 29 )                 *
*                                                                      *
*   Called by user,   DBJOIN, DBVWPR                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =  1 : Illegal character option                        *
*               =  2 : Illegal path name                               *
*               =  3 : Data base structure in memory clobbered         *
*               =  4 : Illegal key option                              *
*                                                                      *
*     If IQUEST(1) =0, IQUEST(2) carries information whether data      *
*     part had been actually read from the disk or not                 *
*     IQUEST(2) =  0 : No disk i/o has been performed                  *
*               =  1 : Data have been refreshed from the disk          *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       KEYS(9), LBD(9), LBK(9), ITIME(9)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*28
*
*     ------------------------------------------------------------------
*
* *** Reformat CHOPT
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      CALL DBOPTM (CHOPF)
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
*
* *** Set up a callto CDUSEDB
*
      CALL CDUSEDB (PATHN, LBK, ITIME, MASKDX, KEYSDX, CHOPF, IRC)
      IQUEST(1) = IRC
      IF (IOPSDX.NE.0) THEN
        IF (LBK(1).NE.0) LBD(1) = LQ(KOFUCD+LBK(1)-KLDACD)
      ELSE
        NKYMX  = IQUEST(3)
        DO 10 I = 1, NKYMX
          IF (LBK(I).NE.0) LBD(I) = LQ(KOFUCD+LBK(I)-KLDACD)
   10   CONTINUE
      ENDIF
*                                                              END DBUSE
  999 END
