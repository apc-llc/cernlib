*
* $Id: cdldic.F,v 1.1.1.1 1996/02/28 16:24:29 mclareni Exp $
*
* $Log: cdldic.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLDIC (PATHN, IFLG, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLDIC (*PATHN*, IFLG, IRC*)                            *
*                                                                      *
*   Gets information of the specified pathname from the dictionary     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*              If flag < 0, current directory name is set to PATHN     *
*     IFLG     Flag (< 0 if PATHN to be obtained from the name of the  *
*              current set directory)                                  *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDDONT, CDENFZ, CDENTB, CDKOUT, CDPART, CDRTFZ           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 95 : Description not found in the dictionary         *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Remove the top directory part from the current directory
*
      IF (IFLG.LT.0) CALL RZCDIR (PATHN, 'R')
      CALL CDWTOP (PATHN, PAT2CT, NCHR)
*
* *** Load the UPDB bank content for this top directory
*
      LFIXCD  = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LBUPCD.NE.0.AND.LFIXCD.NE.0) THEN
        NITEM  = IQ(KOFUCD+LFIXCD+MDCNTM)
        DO 10 I = 1, NITEM
          IPNT   = KOFUCD + LFIXCD + (I - 1) * NWITCD + 1
          NCHF   = IQ(IPNT+MDCNCH)
          IF (IQ(IPNT+MDCITM).GT.0.AND.NCHR.EQ.NCHF) THEN
            CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT3CT, NCHF)
            IF (PAT2CT(1:NCHR).EQ.PAT3CT(1:NCHF)) THEN
              ITNUCD = IQ(IPNT+MDCITM)
              LUPDCD = IQ(IPNT+MDCLUP)
              ICMPCD = IQ(IPNT+MDCCMP)
              IPRECD = IQ(IPNT+MDCPRC)
              DELTCD =  Q(IPNT+MDCDEL)
              CALL UHTOC (IQ(IPNT+MDCALI), 4, CALICD, 8)
              IRC    = 0
              GO TO 999
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
*
      IRC    = 95
#if defined(CERNLIB__DEBUG)
      IF (IDEBCD.GT.0) THEN
        CALL CDPRNT (LPRTCD, '(/,'' CDLDIC : Cannot find the entry '//
     +       PAT2CT(1:NCHR)//' in the dictionary '')', IARGCD, 0)
      ENDIF
#endif
*                                                             END CDLDIC
  999 END
