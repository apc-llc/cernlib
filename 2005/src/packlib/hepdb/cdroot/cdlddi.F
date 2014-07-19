*
* $Id: cdlddi.F,v 1.1.1.1 1996/02/28 16:24:32 mclareni Exp $
*
* $Log: cdlddi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLDDI (PATHI, PATHO, NCHAR)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLDDI (PATHI, PATHO*, NCHAR*)                          *
*                                                                      *
*   Checks the directory name, if alias returns the complete pathname; *
*   otherewise returns the original name in upper case                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHI    Input pathname (could be alias name with '%' prefix)    *
*     PATHO    Output pathname (in upper case)                         *
*     NCHAR    Number of characters in PATHO                           *
*                                                                      *
*   Called by CDKEEP, CDLDIR, CDLDUP, CDLIST                           *
*                                                                      *
************************************************************************
*
#include "hepdb/ctpath.inc"
      CHARACTER       PATHI*(*), PATHO*(*), CSALI*1
      DATA            CSALI /'%'/
*
*     ------------------------------------------------------------------
*
* *** Remove blanks in the path name and set current directory
*
      CALL CDSBLC (PATHI, PATHO, NCHAR)
      IF (PATHO(1:1).EQ.CSALI) THEN
        PAT2CT = PATHO(2:NCHAR)
        CALL CDRALI (PAT2CT, PAT3CT, IRC)
        IF (IRC.EQ.0) THEN
          PATHO  = PAT3CT
          NCHAR  = LENOCC (PATHO)
        ENDIF
      ENDIF
      CALL CLTOU (PATHO(1:NCHAR))
*                                                             END CDLDDI
      END
