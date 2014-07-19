*
* $Id: cslog.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cslog.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.12/05 20/03/92  16.04.18  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSLOG(NAME)
***---------------------------------
#include "comis/cslun.inc"
         CHARACTER *(*) NAME
#if defined(CERNLIB_IBM)
         CHARACTER *80 VMIBM
#endif
      IF(ISTLOG.NE.0)THEN
         CLOSE(LUNLOG)
         ISTLOG=0
      ENDIF
#if defined(CERNLIB_VAX)
      OPEN(LUNLOG,FILE=NAME,RECORDTYPE='VARIABLE',STATUS='NEW'
     1,ORGANIZATION='SEQUENTIAL',CARRIAGECONTROL='LIST',ERR=1)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      L=LEN(NAME)
      L=MNBLAN(MJSCHA(NAME),L)
      VMIBM='FILEDEF      DISK '//NAME(1:L)
      WRITE(VMIBM(9:12),77)LUNLOG
  77  FORMAT(1X,I2)
      CALL VMCMS(VMIBM,IER)
      IF(IER.NE.0)GO TO 1
*      OPEN(LUNLOG,FILE=NAME,STATUS='NEW', ERR=1)
#endif
#if defined(CERNLIB_IBMMVS)
*--       OPEN UNIT DONE VIA JCL OR TSO CONTROL CARDS
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      OPEN(LUNLOG,FILE=NAME,STATUS='UNKNOWN',ERR=1)
#endif
      ISTLOG=1
      RETURN
#if !defined(CERNLIB_IBMMVS)
  1    CALL CSSOUT('LOG-FILE WAS NOT OPEN')
        CALL CSSOUT(NAME)
#endif
      END
