*
* $Id: cdgali.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdgali.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDGALI (PATHN, ALIAS, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDGALI (PATHN, ALIAS*, IRC*)                            *
*                                                                      *
*   Get the alias name of a given directory                            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string specifying the directory path name     *
*     ALIAS    Character string specifying the alias name              *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDALIA                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =146 : Illegal path name                               *
*               =147 : Dictionary directory not found in memory        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      CHARACTER       PATHN*(*), ALIAS*(*), PATHY*80, PATHL*80
*
*     ------------------------------------------------------------------
*
* *** Load the top directory information
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)       GO TO 999
      PATHL  = ' '
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
*
* *** Find the unique directory identifier from the pathname
*
      CALL CDGPID (PATHY, IDN)
      IF (IDN.LE.0) THEN
        IRC    = 146
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGALI : Illegal'//
     +  ' pathname '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Modify in memory
*
      LUFZCF = LUFZCD
      LFIXCD = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LFIXCD.EQ.0) THEN
        IRC    = 147
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGALI : DICTION'//
     +  'ARY not found for '//TOPNCD//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      IPNT   = KOFUCD + LFIXCD + (IDN - 1) * NWITCD + 1
      ALIAS  = ' '
      CALL UHTOC (IQ(IPNT+MDCALI), 4, ALIAS, 8)
*                                                             END CDGALI
  999 END
