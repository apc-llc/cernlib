*
* $Id: cdfzop.F,v 1.1.1.1 1996/02/28 16:24:11 mclareni Exp $
*
* $Log: cdfzop.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:11  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFZOP (LUNFZ, TOPN, CHOPT, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFZOP (LUNFZ, TOPN, CHOPT, IRC*)                       *
*                                                                      *
*   Sets the output FZ file number as the sequential backup file for   *
*   a given top directory. One can have up to two backup files, one    *
*   for standard journaling, the other for special backups.            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNFZ    Logical unit number of the FZ file                      *
*     TOPN     Top directory name to which the FZ file refers          *
*     CHOPT    Character string with any of the following characters   *
*          B   Specify a special backup file                           *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =135 : Illegal top directory name                      *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       TOPN*(*), CHOPT*(*)
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0) GO TO 999
      CALL CDSBLC (TOPN, TOPNCD, NCHRCD)
*
* *** Check if the topname matches with any of the existing topnames
*
      LBUPCD = LTOPCD
   10 IF (LBUPCD.GT.0) THEN
        NCHR   = IQ(KOFUCD+LBUPCD+MUPNCH)
        CALL UHTOC (IQ(KOFUCD+LBUPCD+MUPNAM), 4, TOP1CT, NCHR)
        TOP1CT = TOP1CT(1:NCHR)
        IF (TOPNCD.NE.TOP1CT) THEN
          LBUPCD = LQ(KOFUCD+LBUPCD)
          GO TO 10
        ELSE
          IF (IOPBCA.EQ.0) THEN
            IOFF   = MUPJFL
          ELSE
            IOFF   = MUPBAK
          ENDIF
          LUNOL  = IQ(KOFUCD+LBUPCD+IOFF)
          IQ(KOFUCD+LBUPCD+IOFF) = LUNFZ
          IF (LUNOL.LE.0.AND.LUNFZ.GT.0) THEN
            INC    = 1
          ELSE IF (LUNOL.GT.0.AND.LUNFZ.LE.0) THEN
            INC    =-1
          ELSE
            INC    = 0
          ENDIF
          NUFZCF = NUFZCF + INC
        ENDIF
      ELSE
        IRC    = 135
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZOP : Illegal'//
     +  ' top directory name '//TOPNCD//''')', IARGCD, 0)
#endif
      ENDIF
*                                                             END CDFZOP
  999 END
