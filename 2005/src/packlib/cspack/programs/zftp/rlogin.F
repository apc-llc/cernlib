*
* $Id: rlogin.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: rlogin.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE RLOGIN
*
*           To start a communication with a remote computer
*           Current implementation based on TCP/IP
*
#include "cspack/czsock.inc"
      CHARACTER*255 HOST,HNAME
*     CHARACTER*32 HOST,HNAME
      CHARACTER*4  CHOPT
      SAVE HNAME
      DATA HNAME/' '/
*
*=========================================================
*
      CALL KUGETC(HOST,NCH)
      IF(NCH.LE.0)GO TO 99
      CALL CUTOL(HOST)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         LCHOPT = 1
         CHOPT  = ' '
      ENDIF
 
      IF(HOST.EQ.HNAME.AND.ISKIN.GE.0)THEN
         PRINT *,' Communication already established'
         GO TO 99
      ENDIF
*
      IF(ISKIN.GE.0)THEN
         CALL CZCLOS(ISTAT)
         HNAME=' '
      ENDIF
 
 
      IPROT = 0
#if defined(CERNLIB_VAXVMS)
      IF(INDEX(CHOPT(1:LCHOPT),'D').NE.0) IPROT = 1
#endif
 
      IF((INDEX(CHOPT(1:LCHOPT),'V').NE.0).AND.
     +   (INDEX(HOST(1:NCH),'(vm').EQ.0)) THEN
         CALL CZOPEN('zserv',HOST(1:NCH)//'(vm',ISTAT)
      ELSEIF((INDEX(CHOPT(1:LCHOPT),'M').NE.0).AND.
     +   (INDEX(HOST(1:NCH),'(mvs').EQ.0)) THEN
         CALL CZOPEN('zserv',HOST(1:NCH)//'(mvs',ISTAT)
      ELSE
         CALL CZOPEN('zserv',HOST,ISTAT)
      ENDIF
*
      IF(ISTAT.NE.0)THEN
         PRINT *,' Cannot open communication with ',HOST
#if defined(CERNLIB_BATCH)
         CALL CZSTOP(ISTAT)
#endif
         GO TO 99
      ENDIF
      HNAME=HOST
      CUNODE = HOST(1:LENOCC(HOST))
*
  99  END
 
