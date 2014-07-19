*
* $Id: pfloop.F,v 1.2 1996/05/06 13:30:59 dinofm Exp $
*
* $Log: pfloop.F,v $
* Revision 1.2  1996/05/06 13:30:59  dinofm
* Code modified to take care of empty histograms detection on slave(s).
*
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :          10/11/95  16.10.02  by  Timo Hakulinen
*-- Author :    Alfred Nathaniel   07/04/93
      SUBROUTINE PFLOOP(ISTAT)
*
* listen for requests from Piaf server
*
      LOGICAL   HEXIST
      CHARACTER CHMAIL*80, CHMESS*6, CHLINE*74
      INTEGER EMASK
*
* loop over server replies
*
 1    CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 999
      CALL PFLOG(8,'PFLOOP',CHMAIL)

      CHMESS=CHMAIL(:6)
      CHLINE=CHMAIL(7:)

      IF(CHMESS(:1).EQ.'I') THEN
*
* remote HIGZ
*
         CALL PFHIGZ(CHMESS,CHLINE,0,ISTAT)

      ELSEIF(CHMESS(:2).EQ.'HP') THEN
*
* remote HPLOT
*
         CALL PFPLOT(CHMESS,CHLINE,0,0,ISTAT,EMASK)

      ELSEIF(CHMESS(:1).EQ.'P') THEN
*
* remote PAW
*
         CALL PFPAW(CHMESS,CHLINE,0,0,ISTAT,EMASK)

      ELSEIF(CHMESS.EQ.'HEXIST') THEN
*
* Does histogram exist?
*
        READ(CHLINE,'(I12)') IDH
        IF(HEXIST(IDH)) THEN
           CALL CZPUTA('YES',ISTAT)
        ELSE
           CALL CZPUTA('NO',ISTAT)
        ENDIF

      ELSEIF(CHMESS.EQ.'HLOGPF') THEN
*
* import log file
*
         CALL HLOGPF(CHMAIL,ISTAT)
         IF(ISTAT.NE.0) GOTO 999
         GOTO 9

      ELSEIF(CHMESS.EQ.'HINPF') THEN
*
* import histogram
*
         READ(CHLINE,'(I12)') IDH
         CALL HINPF(IDH,0)

      ELSEIF(CHMESS.EQ.'HOUTPF') THEN
*
* export histogram
*
         READ(CHLINE,'(I12)') IDH
         CALL HOUTPF(IDH)

      ELSEIF(CHMESS.EQ.'KISNDV') THEN
*
* export a vector if it exists
*
         CALL KISNDV(CHLINE,ISTAT)

      ELSEIF(CHMESS.EQ.'STAGEW') THEN
*
* request to wait for the end of a staging request
* (if NOWAIT flag was specified, we don't come here - in
*  that case the response goes straight through log file)
*
         PRINT '(A)',CHLINE

      ELSEIF(CHMESS.EQ.'BREAK*') THEN
*
* server had an exception
*
         CALL CZGETA(CHMAIL,ISTAT)
         PRINT '(A)',CHLINE
         CALL HLOGPF(CHMAIL,ISTAT)
         ISTAT=1
         GOTO 999

      ENDIF
      GOTO 1

 9    CONTINUE
 999  CONTINUE
      END
#endif
