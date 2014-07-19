*
* $Id: cdextr.F,v 1.1.1.1 1996/02/28 16:24:12 mclareni Exp $
*
* $Log: cdextr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:12  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEXTR (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT,IRC)
*     =========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEXTR (PATHI, LUNFZ, K1MIN, K1MAX, CHOPT)              *
*                                                                      *
*   Copies a part of the directory tree from the current level         *
*   downwards (in the range K1MIN-K1MAX or with insertion time > K1MIN *
*   for specified CHOPT) into a sequential file in the format of the   *
*   journal file                                                       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Path name describing the directory tree                 *
*     LUNFZ    Logical unit number of the output FZ file               *
*     K1MIN    Minumum serial number (Key 1 value) to be copied, or    *
*              Minimum date (YYMMDD) for insertion time to be copied   *
*              (Depends on CHOPT supplied)                             *
*     K1MAX    Maximum serial number (Key 1 value) to be copied, or    *
*              Minimum time (HHMM) for insertion time to be copied     *
*              (Depends on CHOPT supplied)                             *
*     CHOPT    Character string with any of the following characters   *
*          F   Updates with a fully matched data object (in user keys) *
*          I   Copies objects with insertion time > (K1MIN/K1MAX)      *
*          R   Copied objects with RZ insertion time > (K1MIN/K1MAX)   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 75 : Dictionary is not loaded                        *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information on number of data      *
*     objects transfered to the FZ file                                *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*80, PATHY*80, PATHF*80, PROOT*80
      CHARACTER       PATHI*80, CHOPT*(*)
*
*     ------------------------------------------------------------------
*
* *** Load the current directory
*
      PATHN  = PATHI
      CALL CDOPTS (' ', IRC)
      CALL CDLDUP (PATHN, 1, IRC)
      IF (IRC.NE.0)                                       GO TO 999
      PATHN  = PAT1CT
      NCHR   = LENOCC (PATHN)
      PATHF  = '//'//TOPNCD(1:NCHRCD)//'/DICTIONARY'
      IF (PATHN.EQ.PATHF)                                 GO TO 999
      PATHF  = '//'//TOPNCD(1:NCHRCD)//'/HELP'
      IF (PATHN.EQ.PATHF)                                 GO TO 999
      IF (NCHRCD.GE.NCHR-2) THEN
        PROOT  = ' '
        NROOT  = 0
      ELSE
        PROOT  = PATHN(NCHRCD+3:NCHR)
        NROOT  = NCHR - NCHRCD - 2
      ENDIF
*
* *** Find the dictionary record
*
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LFIXCD.EQ.0) THEN
        IRC    = 75
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDEXTR : DICTION'//
     +  'ARY not found for '//TOPNCD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Loop over all items
*
      NITEM  = IQ(KOFUCD+LFIXCD+MDCNTM)
      NTOTL  = 0
      DO 20 I = 1, NITEM
        IPNT   = KOFUCD + LFIXCD + (I - 1) * NWITCD + 1
        NCHF   = IQ(IPNT+MDCNCH)
        IF (IQ(IPNT+MDCITM).GT.0) THEN
          CALL UHTOC (IQ(IPNT+MDCNAM), 4, PATHF, NCHF)
          PATHF  = PATHF(1:NCHF)
          IF (PATHF.EQ.'/HELP'.OR.PATHF.EQ.'/DICTIONARY') GO TO 20
          IF (NROOT.GT.0) THEN
            IF (PATHF(1:NROOT).NE.PROOT(1:NROOT))         GO TO 20
            IF (PATHF(NROOT+1:NROOT+1).NE.' '.AND.
     +          PATHF(NROOT+1:NROOT+1).NE.'/')            GO TO 20
          ENDIF
          PATHY  = '//'//TOPNCD(1:NCHRCD)//PATHF
          CALL CDRTFZ (PATHY, LUNFZ, K1MIN, K1MAX, CHOPT, IRC)
          IF (IRC.NE.0)                                   GO TO 999
          NTOTL  = NTOTL + IQUEST(2)
        ENDIF
   20 CONTINUE
      IQUEST(2) = NTOTL
*                                                             END CDEXTR
  999 END
