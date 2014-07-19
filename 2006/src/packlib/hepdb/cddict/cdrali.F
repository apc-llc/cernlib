*
* $Id: cdrali.F,v 1.1.1.1 1996/02/28 16:24:10 mclareni Exp $
*
* $Log: cdrali.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:10  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRALI (ALIAS, PATH, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRALI (ALIAS, PATH*, IRC*)                             *
*                                                                      *
*   Retrieve the complete path name from the alias name                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     ALIAS    Character string specifying the alias name              *
*     PATH     Character string specifying the directory path name     *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDALIA                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =154 : Not a valid alias name                          *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       PATH*(*), ALIAS*(*), CALI*8, PATHN*80, TOPN*16
*
*     ------------------------------------------------------------------
*
      PATH   = ' '
      IRC    = 0
*
* *** Loop over all the top directories
*
      LTOP   = LTOPCD
   10 IF (LTOP.NE.0) THEN
        LDIC   = LQ(KOFUCD+LTOP-KLDICD)
        IF (LDIC.NE.0) THEN
          NITEM  = IQ(KOFUCD+LDIC+MDCNTM)
          DO 20 I = 1, NITEM
            IPNT   = KOFUCD + LDIC + (I - 1) * NWITCD + 1
            IF (IQ(IPNT+MDCITM).GT.0) THEN
              CALL UHTOC (IQ(IPNT+MDCALI), 4, CALI, 8)
              IF (ALIAS.EQ.CALI) THEN
                NCHF   = IQ(IPNT+MDCNCH)
                CALL UHTOC (IQ(IPNT+MDCNAM), 4, PATHN, NCHF)
                NCHT   = IQ(KOFUCD+LTOP+MUPNCH)
                CALL UHTOC (IQ(KOFUCD+LTOP+MUPNAM), 4, TOPN, NCHT)
                PATH   = '//'//TOPN(1:NCHT)//PATHN(1:NCHF)
                GO TO 999
              ENDIF
            ENDIF
   20     CONTINUE
        ENDIF
        LTOP   = LQ(KOFUCD+LTOP)
        GO TO 10
      ELSE
        IRC    = 154
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          CALI   = ALIAS
          CALL CDPRNT (LPRTCD, '(/,'' CDRALI : Alias name '//CALI//
     +         ' not found'')', IARGCD, 0)
        ENDIF
#endif
      ENDIF
*                                                             END CDRALI
  999 END
