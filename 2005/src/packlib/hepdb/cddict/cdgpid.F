*
* $Id: cdgpid.F,v 1.1.1.1 1996/02/28 16:24:09 mclareni Exp $
*
* $Log: cdgpid.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:09  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDGPID (PATHN, IDN)
*     ==============================
*
************************************************************************
*                                                                      *
*        SUBR. CDGPID (PATHN, IDN*)                                    *
*                                                                      *
*   Finds the unique directory identifier from the path name           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IDN      Unique directory identifier                             *
*                                                                      *
*   Called by CDDINF, CDEALI, CDEHLP, CDENAM, CDRHLP, CDRNAM, CDGNAM   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IDN       >  0 : No error                                        *
*               =  0 : No dictionary for directory identifier          *
*               <  0 : Illegal path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Extract the top directory and the trailing part from the path name
*
      IDN    = 0
      CALL CDWTOP (PATHN, PAT2CT, NCHR)
      IF (NCHR.LE.0)       GO TO 999
*
* *** Find the appropriate dictionary for the directory path name
*
      IF (LBUPCD.LE.0)     GO TO 999
      LDIC   = LQ(KOFUCD+LBUPCD-KLDICD)
      IF (LDIC.LE.0)       GO TO 999
      NITEM  = IQ(KOFUCD+LDIC+MDCNTM)
*
* *** Look up the current path in the dictionary
*
      DO 10 ITEM = 1, NITEM
        IPNT   = KOFUCD + LDIC + (ITEM - 1) * NWITCD + 1
        IF (IQ(IPNT+MDCITM).GT.0) THEN
          IF (NCHR.EQ.IQ(IPNT+MDCNCH)) THEN
            CALL UHTOC (IQ(IPNT+MDCNAM), 4, PAT3CT, NCHR)
            IF (PAT2CT(1:NCHR).EQ.PAT3CT(1:NCHR)) THEN
              IDN    = IQ(IPNT+MDCITM)
              GO TO 999
            ENDIF
          ENDIF
        ENDIF
   10 CONTINUE
*                                                             END CDGPID
  999 END
