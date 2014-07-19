*
* $Id: cdsave.F,v 1.1.1.1 1996/02/28 16:24:18 mclareni Exp $
*
* $Log: cdsave.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:18  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSAVE (TOPN,CHOPT,IRC)
*     ==================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSAVE (TOPN,CHOPT,IRC*)                                *
*                                                                      *
*   Sends the journal file to the server for updating the data base.   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CHOPT                                                            *
*           C  Journal file to be closed, else reopened                *
*           P  Purge pending updates                                   *
*           S  Send signal to server                                   *
*           W  Wait for response from server (not yet implemented)     *
*                                                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =225 : Error in sending the file to the server         *
*                       - or - error deleting journal file (IOPTP)     *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "zebra/fzstat.inc"
      CHARACTER       TOPN*(*)
      LOGICAL         IOPEN
#include "hepdb/hdbopts.inc"
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
         IF(IOPTP.NE.0.OR.(LUFZCF.EQ.INFLUN.AND.INFOFZ(2).EQ.0)) THEN
            CLOSE(LUFZCF,STATUS='DELETE',IOSTAT=IRC1)
         ELSE
            CALL CDFSND (LUFZCF, TOPLCI(3:4), IRC1)
#if defined(CERNLIB_UNIX)
*
*     Send signal
*
            IF(IOPTS.NE.0) THEN
               CALL CDGETP(LUFZCF,IPID,IRC)
               IF(IRC.EQ.0) CALL CDALARM(IPID,IRC)
            ENDIF
#endif
         ENDIF
 
         IF (IOPTC.EQ.0) THEN
            CALL CDFOPN (LUFZCF, IRC2)
            IF (IRC1.EQ.0) IRC1 = IRC2
            CALL FZFILE (LUFZCF, 0, 'AOQ')
         ENDIF
 
         IF (IRC1.NE.0) THEN
            IRC = 225
            IQUEST(11) = IRC1
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '('' CDSTSV : Error'/
     +      / ' '',I6,'' in sending file to server for '//TOPLCI//''')'
     +      , IQUEST(11), 1)
#endif
            TOPLCI = ' '
            GO TO 999
         ENDIF
         IRC = 0
      ELSE
         IRC = 0
         IF (IOPTC.NE.0) THEN
            INQUIRE(LUFZCF,OPENED=IOPEN)
            IF(IOPEN.AND.LUFZCF.GT.0) CLOSE (LUFZCF, STATUS='DELETE')
         ENDIF
      ENDIF
      IF (IOPTC.EQ.0) THEN
         TOPLCI = TOPN
      ELSE
         TOPLCI = ' '
      ENDIF
*                                                             END CDSAVE
  999 END
