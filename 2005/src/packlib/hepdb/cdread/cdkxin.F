*
* $Id: cdkxin.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdkxin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDKXIN (ITIME, IUDIV, LU, LSUP, JBIAS, NWKEY, KEY,
     +                   IPREC, IRC)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKXIN (ITIME, IUDIV, LU*, LSUP, JBIAS, NWKEY*, *KEY*,  *
*                      IPREC*, IRC*)                                   *
*                                                                      *
*   Fetches from disk to a ZEBRA bank data valid for a given time      *
*                                                                      *
*   Restrictions : This routine is called internally by other routines *
*                  in HEPDB. One should be very careful in using this  *
*                  routine. For example, the character option for      *
*                  special selection scheme like options S or N in     *
*                  CDUSEM is not present in this routine and they can  *
*                  be switched on or off only by a prior call to CDUSEM*
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ITIME    Time for which data are required to be valid            *
*     IUDIV    Division index where bank is expected                   *
*     LU       Address of the bank in memory                           *
*     LSUP     Supporting link of the bank (see MZBOOK)                *
*     JBIAS    Link bias for creating the data bank (see MZBOOK)       *
*     NWKEY    Number of key elements                                  *
*     KEY      Vector of keys (On input if any key beyond element 8 or *
*              element 5 is nonzero,data are retrieved according to its*
*              content; on output it contains the key elements for the *
*              current data)                                           *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCHCK, CDGETDB,CDGNAM, CDPART, CDPRDT, CDPURK, CDRHLP,  *
*             CDRNAM, CDRTFZ, CDUSP3                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 31 : Illegal Pathname                                *
*               = 33 : No valid data for the given set of keys and     *
*                      program version number                          *
*               = 35 : Wrong reference to data objects in update mode  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctkxin.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      DIMENSION       ITIME(9), KEY(9), LSUP(9), LU(9)
      CHARACTER       PATHN*80
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
#if defined(CERNLIB__P3CHILD)
      IF (IPASP3.GT.1) THEN
        IKYLCT = KEY(IDHKSN)
        IPRVCT = IKYLCT
        NWKYCK = NWKEY
        CALL UCOPY (KEY, KEYNCK, NWKEY)
        GO TO 5
      ENDIF
#endif
      IPRVCT = KEY(IDHUSI)
      NWKEY  = NWKYCK
      NKEYS  = NKEYCK
      CALL RZCDIR (PATHN, 'R')
      MAXL   = LENOCC (PATHN)
*
* *** Check the validity limits from the Keys
*
    5 NTIMCT = 0
      MNKYCT = -999
      DO 10 I = 1, NPARCD
        KYENCD(I) = IBIGCD
        INRSCT(I) = 100000
   10 CONTINUE
      IDNRCT = 0
      KEY6CT = 0
      KY6NCT = 0
      ICURCT = 1
      ISTP   = NWKYCK + 1
#if defined(CERNLIB__P3CHILD)
      IF (IPASP3.GT.1)                                      GO TO 40
#endif
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
* *** Select by the key number
*
      IF (IOKYCA(IDHKSN).NE.0)  THEN
        IKYLCT = KEY(IDHKSN)
        IPRVCT = IKYLCT
*
        IF (IOPTP.EQ.0)  THEN
*
          IF (IKYLCT.GT.0 .AND. IKYLCT.LE.NKEYCK)  THEN
            GO TO 35
          ELSE
            GO TO 993
          ENDIF
*
        ELSE
*
          DO 15 JK = 1, NKEYCK
            IK     = NKEYCK + 1 - JK
            KPNT   = IUHUNT (IK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                       NKEYCK*ISTP, ISTP)
            IF (KPNT.NE.0) THEN
              IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
            ELSE
              IPNT   = KOFSCD + LCDRCD + IKDRCD + (IK - 1) * ISTP
            ENDIF
            KEY5   = IQ(IPNT+MOBJCD)
            IF (IKYLCT.GT.KEY5)                             GO TO 20
   15     CONTINUE
          GO TO 993
   20     KOFF   = KEY5
          CALL CDPATH (TOP2CT, IK)
          CALL RZCDIR (TOP2CT, ' ')
          IF (IQUEST(1).NE.0)                               GO TO 991
          IKYLCT = KEY(IDHKSN) - KOFF
*
*     Check not valid in case of deleted keys
*
*         IF (IKYLCT.GT.IQUEST(7)) THEN
*           GO TO 993
*         ELSE
            NKEYCK = IQUEST(7)
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            CALL CDKEYT
            ISTP   = NWKYCK + 1
            GO TO 35
*         ENDIF
        ENDIF
*
      ENDIF
*
* *** Select by the Key values
*
      IF (IOPTP.EQ.0)  THEN
*
        CALL CDSEKY (ITIME, KEY)
*
      ELSE
*
        DO 25 JK = 1, NKEYS
          ICURCT = NKEYS + 1 - JK
          CALL CDPATH (TOP2CT, ICURCT)
          IF (JK.NE.1) THEN
            CALL RZCDIR (PATHN, ' ')
            IF (IQUEST(1).NE.0)                             GO TO 991
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
          ENDIF
          KPNT   = IUHUNT (ICURCT, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*ISTP, ISTP)
          IF (KPNT.NE.0) THEN
            IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (ICURCT - 1) * ISTP
          ENDIF
          CALL CDPSEL (ITIME, KEY, IQ(IPNT+1), 0, ISEL)
          IF (ISEL.NE.0)                                    GO TO 25
*
          CALL RZCDIR (TOP2CT, ' ')
          IF (IQUEST(1).NE.0)                               GO TO 991
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL CDSEKY (ITIME, KEY)
          IF (NTIMCT.NE.0 .AND. JBIT(KEY6CT,JIGNCD).EQ.0)   GO TO 30
*
   25   CONTINUE
*
      ENDIF
*
* *** Check if valid data exist
*
   30 ISTP   = NWKYCK + 1
      IF (NTIMCT.EQ.0 .OR. JBIT(KEY6CT,JIGNCD).NE.0)  THEN
*
        IF (KY6NCT.NE.0)  THEN
          IKYLCT = KY6NCT
*
        ELSE IF (IOPNCA.NE.0 .AND. IDNRCT.NE.0)  THEN
*
*  **     Take the nearest neighbour
*
          NTIMCT = -1
          IKYLCT = IDNRCT
          IF (IOPTP.NE.0 .AND. IUSECT.NE.ICURCT)  THEN
            CALL CDPATH (TOP2CT, IUSECT)
            PAT2CT = PATHN(1:MAXL)//'/'//TOP2CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)                             GO TO 991
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
          ENDIF
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.1)  THEN
            IPNT   = KOFSCD + LCDRCD + IKDRCD + (IDNRCT - 1) * ISTP
            IARGCD(1) = ITIME(1)
            IARGCD(2) = IQ(IPNT+NOF1CK+1)
            IARGCD(3) = IQ(IPNT+NOF1CK+2)
            CALL CDPRNT (LPRTCD, '(/,'' CDKXIN : Warning --> Data val'//
     +           'id at time '',I10,'' does not exist. Data valid bet'//
     +           'ween '',2I10,'' are returned'')', IARGCD, 3)
          ENDIF
#endif
*
        ELSE
*
*  **     None exists; return with error message
*
          GO TO 993
*
        ENDIF
*
      ENDIF
*
* *** Read in the data
*
   35 ICYCL  = 9999
      CALL CDKEYR (IKYLCT, NWKYCK, KEYNCK)
   40 KEY6CT = KEYNCK(IDHFLG)
      IF (JBIT(KEY6CT,JIGNCD).NE.0) GO TO 993
      KEY(IDHKSN) = IKYLCT
      LSTRCL(2) = 0
      IF (NTIMCT.EQ.-1) THEN
        DO 45 I = 1, NPARCD-1
          KYENCD(I) = KEYNCK(NOF1CK+2*I-1)
   45   CONTINUE
        KYENCD(NPARCD) = KEYNCK(NOF1CK+2*NPARCD-1) + 1
      ENDIF
*
      IF (IOPKCA.NE.0)  THEN
*
*  **   Only the keys are needed
*
        CALL UCOPY (KEYNCK, KEY, NWKYCK)
*
      ELSE IF (JBIT(KEY6CT,JRZUCD) .NE. 0)  THEN
*
*  **   Data are stored in RZ way
*
        LSTRCL(2) = LSUP(1)
        CALL CDRZIN (IUDIV, LSTRCL(2), JBIAS, KEY, ICYCL, PATHN, IRC)
        LSUP(1) = LSTRCL(2)
        LSTRCL(2) = 0
        IF (JBIAS.GT.0)  THEN
          LU(1)  = LSUP(1)
        ELSE
          LU(1)  = LQ(KOFUCD+LSUP(1)+JBIAS)
        ENDIF
        IF (IRC.NE.0)                                       GO TO 998
*
        CALL UCOPY (KEYNCK, KEY, NWKYCK)
*
      ELSE
*
*  **   Read in standard DB format
*
        CALL CDRZIN (IDISCD, LSTRCL(2), 2, KEY, ICYCL, PATHN, IRC)
        IF (IRC.NE.0)                                       GO TO 998
        CALL UCOPY (KEYNCK, KEY, NWKYCK)
*
*  **   If the data is update - uncompress and proceed updating
*
        IF (KEY(IDHPTR).NE.0)  THEN
#if defined(CERNLIB__P3CHILD)
          IF (IPASP3.EQ.2) THEN
            IK   = 1
            GO TO 55
          ENDIF
#endif
          DO 50 I = 1, NKEYCK
            IPNT   = KOFSCD + LCDRCD + IKDRCD + ISTP * (I-1)
            IF (KEY(IDHPTR).EQ.IQ(IPNT+IDHKSN))  THEN
              IK     = I
#if defined(CERNLIB__P3CHILD)
              IF (IPASP3.EQ.1) THEN
                IRC    = 0
                CALL CDRZIN (IDISCD, LREFCD(5), 2, IK, ' ', IRC)
                GO TO 998
              ENDIF
#endif
              GO TO 55
            ENDIF
   50     CONTINUE
          GO TO 995
*
   55     CALL CDUNCP (LSTRCL(2), LREFCL(3), IK, IRC)
        ELSE
#if defined(CERNLIB__P3CHILD)
          IF (IPASP3.EQ.1) THEN
            IRC    = 0
            GO TO 998
          ENDIF
#endif
          CALL CDUNCP (LSTRCL(2), LREFCL(3), 0, IRC)
        ENDIF
*
*  **   Drop the compressed data
*
        CALL MZDROP (IDISCD, LSTRCL(2), ' ')
        IF (IRC.NE.0)                                       GO TO 998
*
*  **   Copy from internal Data Base system to user format
*
        CALL CDTOUS (LREFCL(3), IUDIV, LU(1), LSUP(1),JBIAS, IPREC, IRC)
*
        CALL MZDROP (IDISCD, LREFCL(3), ' ')
        IF (IRC.NE.0)                                       GO TO 998
*
      ENDIF
      GO TO 998
*
* *** Error messages
*
  991 IRC    = 31
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKXIN : Illegal '//
     +   'path name '//PATHN//TOP2CT(1:8)//''')', IARGCD, 0)
#endif
      GO TO 999
*
  993 IRC    = 33
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) THEN
        IARGCD(1) = IPRVCT
        CALL UCOPY (ITIME, IARGCD(2), NPARCD)
        NARG      = MIN (4, NPARCD+1)
        CALL CDPRNT (LPRTCD, '(/,'' CDKXIN : No valid data can be fou'//
     +       'nd for Program Version'',I6,'' and ISEL'',3I10)', IARGCD,
     +       NARG)
      ENDIF
#endif
      GO TO 998
*
  995 IRC    = 35
      IQUEST(11)= KEY(IDHPTR)
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKXIN : Key 2 ref'//
     +   'erence '',I4,'' cannot be found among the Key 1 values'')',
     +   IQUEST(11), 1)
#endif
      GO TO 998
*
  998 IF (IOPTP.NE.0)  THEN
        CALL RZCDIR (PATHN, ' ')
        IF (IQUEST(1).NE.0) THEN
          IF (IRC.EQ.0)                                     GO TO 991
        ELSE
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
        ENDIF
      ENDIF
*                                                             END CDKXIN
  999 END
