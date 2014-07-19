*
* $Id: cdlmod.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdlmod.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLMOD (PATHN, IDATE, ITIME, CHOPT, IRC)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLMOD (PATHN, IDATE*, ITIME*, CHOPT, IRC*)             *
*                                                                      *
*   Finds the modification date and time for the specified directory   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IDATE    Date (YYMMDD) of the insertion of the last element      *
*     ITIME    Time (HHMM)   "   "      "      "  "    "     "         *
*     CHOPT    ' ' - HEPDB date and time                               *
*              'R' - RZ    date and time (not yet implemented)         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =131 : Illegal pathname                                *
*               =132 : Illegal number of keys in the directory         *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), PATHY*80, PATHX*16, CHOPT*(*)
      CHARACTER*255   CHTEMP
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
*
* *** Save current directory
*
      CALL RZCDIR(CHTEMP,'R')
*
* *** Crack options
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0) GO TO 999
*
* *** Set the current directory path name
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0) GO TO 999
*
      PATHY  = PAT1CT
      IF (IOPRCA.NE.0) THEN
        IDATE  = IQUEST(16)
        ITIME  = IQUEST(17)
        GO TO 999
      ENDIF
      IF (NKEYCK.LE.0) THEN
        IRC     = 132
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDLMOD : Illegal'//
     +  ' number of keys for '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      IPNT   = KOFSCD + LCDRCD + IKDRCD
      ISTP   = NWKYCK + 1
      IOPTP  = JBIT (IQ(IPNT+IDHFLG), JPRTCD)
*
* *** Take different action for partitioned and non-partitioned cases
*
      IF (IOPTP.EQ.0) THEN
        KPNT   = IPNT + (NKEYCK-1) * ISTP
        KMAX   = IQ(KPNT+IDHINS)
        JMAX   = NKEYCK
        DO 10 I = 2, NKEYCK
          KPNT   = KPNT - ISTP
          IF (IQ(KPNT+IDHINS).GT.KMAX) THEN
            KMAX = IQ(KPNT+IDHINS)
            JMAX = I - 1
          ENDIF
   10   CONTINUE
        KEY7   = IQ(IPNT+(JMAX-1)*ISTP+IDHINS)
      ELSE
*
        NCH    = LENOCC (PATHY)
        DO 20 IK = 1, NKEYCK
          JK     = NKEYCK + 1 - IK
          CALL CDPATH (PATHX, JK)
          PAT1CT = PATHY(1:NCH)//'/'//PATHX
          CALL RZCDIR (PAT1CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 131
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDLMOD : Ill'//
     +      'egal path name '//PAT1CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYS  = IQUEST(7)
          NWKYCK = IQUEST(8)
          IF (NKEYS.GT.0) THEN
            IPNT   = KOFSCD + IQUEST(11) + IQUEST(13)
            KPNT   = IPNT + (NKEYS-1) * ISTP
            KMAX   = IQ(KPNT+IDHINS)
            JMAX   = NKEYS
            DO 15 I = 2, NKEYS
              KPNT   = KPNT - ISTP
              IF (IQ(KPNT+IDHKSN).GT.KMAX) THEN
                KMAX = IQ(KPNT+IDHINS)
                JMAX = I - 1
              ENDIF
   15       CONTINUE
            KEY7   = IQ(IPNT+(JMAX-1)*ISTP+IDHINS)
            GO TO 30
          ENDIF
   20   CONTINUE
      ENDIF
*
* *** and get the date
*
   30 IRC    = 0
      CALL CDUPTM (IDATE, ITIME, KEY7, IRC)
*                                                             END CDLMOD
*     Reset current directory
*
  999 CALL RZCDIR(CHTEMP,' ')
      END
