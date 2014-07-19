*
* $Id: kibres.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kibres.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/00 03/11/92  14.24.36  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KIBRES
*
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen2.inc"
*
#if defined(CERNLIB_VMS)
      LOGICAL OPENED
      INQUIRE(5,OPENED=OPENED)
      IF (.NOT.OPENED) THEN
        OPEN(5,FILE='TT:',STATUS='OLD')
      ENDIF
      INQUIRE(6,OPENED=OPENED)
      IF (.NOT.OPENED) THEN
        OPEN(6,FILE='TT:',STATUS='OLD')
      ENDIF

#endif
#if defined(CERNLIB_APOLLO)
      CALL FIO_$RESET
#endif
*
* Call user terminal routine defined by the user with KUTERM
*
      CALL KUALFA
*
999   END



