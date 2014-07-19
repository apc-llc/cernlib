*
* $Id: cdprin.F,v 1.1.1.1 1996/02/28 16:24:27 mclareni Exp $
*
* $Log: cdprin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:27  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPRIN (PATHN, CHOPT, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPRIN (PATHN, CHOPT, IRC*)                             *
*                                                                      *
*   Prints all objects attached to the directory PATHN                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     CHOPT    Character string with any of the following characters   *
*          I   Print the insertion time as well                        *
*          K   Print only the keys                                     *
*          D   Print all data as well as the keys                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 31 : Illegal path name                               *
*               = 32 : No key or data for the path name                *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80, CFMT*130
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)       GO TO 999
*
* *** Suppress blanks from the path name
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)       GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      CALL CDKYTG
*
* *** Check the number of keys
*
      IF (NKEYCK.EQ.0) THEN
        IRC    = 32
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPRIN : No key '//
     +     'or data for Path Name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
*  ** Print keys
*
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      IF (IOPKCA.NE.0) IOPDCA = 0
      IF (IOPKCA.NE.0 .OR. IOPDCA.NE.0)  THEN
*
        IF (IOPKCA.NE.0)  THEN
          CALL CDPRNT (LPRTCD, '(/,''  Keys with Insertion Time in Di'//
     +         'rectory '//PATHY//''')', IARGCD, 0)
        ELSE IF (IOPDCA.NE.0)  THEN
          CALL CDPRNT (LPRTCD, '(/,''  Keys and Data with Insertion T'//
     +         'ime in Directory '//PATHY//''')', IARGCD, 0)
        ENDIF
        CFMT(1:5)     = '(7X,'''
        CFMT(129:130) = ''')'
        DO 10 I1 = 1, NWKYCK, 10
          I2     = I1 + 9
          IF (I2.GT.NWKYCK) I2 = NWKYCK
          CFMT(6:128) = ' '
          IFLD   = 6
          DO 5 I = I1, I2
            CFMT(IFLD:IFLD+7) = CTAGCK(I)
            IFLD   = IFLD + 12
    5     CONTINUE
          CALL CDPRNT (LPRTCD, CFMT, IARGCD, 0)
   10   CONTINUE
        CALL CDPRNT (LPRTCD, '(//)', IARGCD, 0)
*
        IF (IOPTP.EQ.0)  THEN
          CALL CDPRDT (IRC)
          IF (IRC.NE.0)   GO TO 999
        ELSE
*
*  **     Loop over subdirectories
*
          NKEYS  = NKEYCK
          DO 20 IK = 1, NKEYS
            CALL CDPATH (TOP1CT, IK)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0) THEN
              IRC    = 31
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPRIN : '//
     +           'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            CALL CDKEYT
            CALL CDPRDT (IRC)
            IF (IRC.NE.0) GO TO 999
   20     CONTINUE
        ENDIF
*
      ENDIF
*                                                             END CDPRIN
  999 END
