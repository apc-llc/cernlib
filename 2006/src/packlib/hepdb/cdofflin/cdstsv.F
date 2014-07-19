*
* $Id: cdstsv.F,v 1.1.1.1 1996/02/28 16:24:17 mclareni Exp $
*
* $Log: cdstsv.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:17  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSTSV (TOPN, IFL, IRC)
*     ==================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSTSV (TOPN, IFL, IRC*)                                *
*                                                                      *
*   Sends the journal file to the offline server if required           *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory for the current update        *
*     IFL      Flag (0 if journal file to be reopened; -1 if not)      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDDDIR, CDDONT, CDEALI, CDEND,  CDENFZ, CDENTB, CDKOUT,  *
*             CDPART, CDPURP, CDRENK, CDSAVE, CDSDIR, CDSNAM, CDSPUR   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =225 : Error in sending the spool file to the server   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "zebra/fzstat.inc"
      CHARACTER       TOPN*(*)
      LOGICAL         IOPEN
*
*     ------------------------------------------------------------------
*
      LUFZCF = LUFMCD
      IF (TOPN.NE.TOPLCI.AND.TOPLCI(1:1).NE.' ') THEN
        CALL FZINFO (LUFZCF)
        CALL FZENDO (LUFZCF, 'QTE')
*
*     Send journal file only if some updates have been made
*
        IF(LUFZCF.EQ.INFLUN.AND.INFOFZ(2).EQ.0) THEN
           CLOSE(LUFZCF,STATUS='DELETE')
        ELSE
           CALL CDFSND (LUFZCF, TOPLCI(3:4), IRC1)
        ENDIF
 
        IF (IFL.EQ.0) THEN
          CALL CDFOPN (LUFZCF, IRC2)
          IF (IRC1.EQ.0) IRC1 = IRC2
          CALL FZFILE (LUFZCF, 0, 'AOQ')
        ENDIF
 
        IF (IRC1.NE.0) THEN
          IRC        = 225
          IQUEST(11) = IRC1
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '('' CDSTSV : Error'//
     +    ' '',I6,'' in sending file to server for '//TOPLCI//''')',
     +    IQUEST(11), 1)
#endif
          TOPLCI = ' '
          GO TO 999
        ENDIF
        IRC    = 0
      ELSE
        IRC    = 0
        IF (IFL.EQ.-1) THEN
          INQUIRE(LUFZCF,OPENED=IOPEN)
          IF(IOPEN.AND.LUFZCF.GT.0) CLOSE (LUFZCF, STATUS='DELETE')
        ENDIF
      ENDIF
      IF (IFL.EQ.0) THEN
        TOPLCI = TOPN
      ELSE
        TOPLCI = ' '
      ENDIF
*                                                             END CDSTSV
  999 END
