*
* $Id: idawk.F,v 1.1.1.1 1996/02/14 13:10:55 mclareni Exp $
*
* $Log: idawk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 11/12/91  17.59.57  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IDAWK(IWKID)
      ENTRY      GDAWK(IWKID)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      DO 10 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID)THEN
            ACWKFL(I)=.FALSE.
            NODRFL(I)=.TRUE.
            GOTO 20
         ENDIF
 10   CONTINUE
      CALL IGERR('Workstation is not active','IDAWK')
      RETURN
 20   CONTINUE
#if defined(CERNLIB_PSCRIPT)
      IF(IGIWTY(IWKID).LT.0)THEN
         PFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
      END
