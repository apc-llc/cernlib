*
* $Id: zsmodu.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsmodu.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE ZSMODU(CHCPU,CHMOD)
*
*         to map /PAWC/ of an OS9 process to /PAWM/
*
      TEMPLATE PAWM
      COMMON/PAWM/NM(10000)
      INTEGER F_LINK
#include "cspack/czdir.inc"
#if defined(CERNLIB_OS9)
#include "cspack/hos9c.inc"
#endif
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHCPU,CHMOD
      character*32 chdir
*
      IF(CHCPU(1:1).NE.'1')GO TO 50
*
*          Mapping a module on the server machine
*
      CPU='1'
      MODULE=CHMOD
      ISTAT = F_LINK(MODULE,@PAWM,JJ)
      IF(ISTAT.NE.0)THEN
         CALL CZPUTA('3 Cannot map module ',ISTAT)
         GO TO 99
      ENDIF
*
      CHRZ(1)=MODULE
      LUNRZ(1)=0
      FLNAME(1)=MODULE
      IDIR=1
      IF(NCHRZ.EQ.0)  NCHRZ=1
      CALL CZPUTA('1 Mapping module '//MODULE,ISTAT)
      GO TO 99
*
*           Setting a connection to a module of another CPU
*
  50  CALL Net_HTAB(CHCPU,CHMOD,ISTAT)
      IF(ISTAT.NE.0)THEN
         CALL CZPUTA('3 Cannot map module ',ISTAT)
         GO TO 99
      ENDIF
      CHRZ(1)=CHMOD
      LUNRZ(1)=-1
      IDIR=1
      IF(NCHRZ.EQ.0)  NCHRZ=1
      CPU=CHCPU
      MODULE=CHMOD
      lcpu=lenocc(cpu)
      chdir=cpu(1:lcpu)//'/'//module
      FLNAME(1)=CHDIR
      CALL CZPUTA('1 Connecting module '//CHDIR,ISTAT)
*
  99  END
#endif
