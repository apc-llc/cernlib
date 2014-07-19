*
* $Id: cddtrn.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cddtrn.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDTRN(CHRAN,IDATE,ITIME,JDATE,JTIME,IC)
*     Convert character date & time range into integer format
*      yymmdd = yymmdd until now
*     -yymmdd = beginning of time until yymmdd
*     .hhmm   = hhmm on first day of Universe/today depending on position
*
      CHARACTER*(*) CHRAN
 
      IC    = 0
      IDATE = 0
      ITIME = 0
      CALL DATIME(JDATE,JTIME)
 
      LCHRAN = LENOCC(CHRAN)
 
      IF(LCHRAN.EQ.0) THEN
         IC = -1
         RETURN
      ENDIF
*
*     Start date and time?
*
      IMINUS = INDEX(CHRAN(1:LCHRAN),'-')
 
      IF(IMINUS.EQ.1) GOTO 10
 
      IF(IMINUS.EQ.0) THEN
         IDOT   = INDEX(CHRAN(1:LCHRAN),'.')
      ELSE
         IDOT   = INDEX(CHRAN(1:IMINUS-1),'.')
      ENDIF
 
      IDATE  = ICDECI(CHRAN,1,LCHRAN)
 
      IF(IDOT.NE.0) ITIME = ICDECI(CHRAN,IDOT+1,LCHRAN)
      IF((IMINUS.EQ.0).OR.(IMINUS.EQ.LCHRAN)) RETURN
 
10    CONTINUE
 
      ISTART = IMINUS + 1
      JDOT   = INDEX(CHRAN(ISTART:LCHRAN),'.')
      IF(JDOT.NE.1) JDATE  = ICDECI(CHRAN,ISTART,LCHRAN)
      IF(JDOT.NE.0) THEN
         JTIME = ICDECI(CHRAN,ISTART+JDOT,LCHRAN)
      ELSE
         JTIME = 0
      ENDIF
 
      END
