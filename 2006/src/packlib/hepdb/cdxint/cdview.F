*
* $Id: cdview.F,v 1.1.1.1 1996/02/28 16:24:53 mclareni Exp $
*
* $Log: cdview.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:53  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDVIEW (CFNAM, PATHI, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDVIEW (CFNAM, PATHI, IRC*)                             *
*                                                                      *
*   Routine to Create and Present "View" a la ORACLE                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CFNAM    File-Name for Presentation                              *
*     PATHI    Pathname of the Table                                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAUXI                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       KEYI(100), IOTI(100), IOTO(100), IOTJ(100)
      DIMENSION       ICONI(100), ICONO(100), ICONJ(100), IDATM(MXPACD)
      CHARACTER       PATHJ*80, PATHO*80, PATHY*80, CHPRO*32
      CHARACTER       CHTI(100)*8, CHTO(100)*8, CHTJ(100)*8, CHMOD*1
      CHARACTER       PATHI*(*), CFNAM*(*)
*
*     ------------------------------------------------------------------
*
* *** Define the Mode to Present (Horizontal or Vertical)
*
      CALL KUPROC ('Which Mode ? (H/V) ', CHMOD, LCMOD)
      IF (CHMOD.NE.'H') THEN
        CHMOD = 'V'
        IOPHCC = 0
      ELSE
        IOPHCC = 1
      ENDIF
*
* *** Set the current directory
*
      CALL CDLDUP (PATHI, 0, IRC)
      IF (IRC.NE.0)       GO TO 999
      PATHY  = PAT1CT
*
* *** Get the date and time (or Run# or last version)
*
      DO 5 I = 1, NPARCD
        IF (IOPXCA.NE.0) THEN
          IDATE = 0
          CALL KUPROI ('Date in YYMMDD ', IDATE)
          ITIME = 0
          CALL KUPROI ('Time in HHMMSS ', ITIME)
          CALL CDPKTS (IDATE, ITIME, IDATM(I), IRC)
        ELSE
          NRUN = 0
          WRITE (CHPRO, 1001) I
          CALL KUPROI (CHPRO, NRUN)
          IDATM(I) = NRUN
        ENDIF
    5 CONTINUE
*
* *** Create the first view
*
      CALL CDVWPR (PATHY, IDATM, NI, CHTI(1), IOTI(1), ICONI(1),
     +             KEYI(1), NO, CHTO(1), IOTO(1), ICONO(1), NOBO, IRC)
      IF (IRC.NE.0)       GO TO 999
*
* *** Present the View or Join New Tables before Presenting
*
      PATHO = PATHY
   10 CONTINUE
      CALL KUPROC ('Present view (|) or join Table (Pathname :) ',
     +             PATHJ, LPJ)
      IF (PATHJ.EQ.'|') THEN
        IF (NO*NOBO.GT.0) THEN
          CALL CDPRES (PATHY, PATHO, IOTI(1), IOTO(1), CHTI(1),
     +                 CHTO(1), KEYI(1), IQ(KOFUCD+LVIWCX+1), ICONI(1),
     +                 ICONO(1), NI, NO, NOBO, CFNAM, IRC)
          CALL MZDROP (IDIVCD, LVIWCX, ' ')
        ENDIF
      ELSE
        IF (NO*NOBO.GT.0) THEN
          CALL CDJOIN (PATHJ, IDATM, NO, NOBO, CHTO(1), IOTO(1),
     +                 ICONO(1), IQ(KOFUCD+LVIWCX+1), NJ, NOBJ, CHTJ(1),
     +                 IOTJ(1), ICONJ(1), IRC)
          CALL MZDROP (IDIVCD, LVIWCX, ' ')
          IF (IRC.NE.0)   GO TO 999
          LVIWCX = LJOICX
          NO = NJ
          NOBO = NOBJ
          DO 15  I = 1, NJ
            IOTO(I)  = IOTJ(I)
            CHTO(I)  = CHTJ(I)
            ICONO(I) = ICONJ(I)
   15     CONTINUE
          PATHO = PATHJ
          GO TO 10
        ENDIF
      ENDIF
*
 1001 FORMAT ('Selection on Validity zone ',I2)
*                                                             END CDVIEW
  999 END
