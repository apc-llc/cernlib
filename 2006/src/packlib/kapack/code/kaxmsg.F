*
* $Id: kaxmsg.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxmsg.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXMSG(LUN,TEXT)
*
*.....PROCESS MESSAGES
*
#include "kapack/kax000.inc"
#include "kapack/kax010.inc"
#include "kapack/kax020.inc"
*
      CHARACTER JIDMSG*60, TEXT*(*)
*
*-----------------------------------------------------------------------
*
      IF ( LUN .EQ. 0 )  RETURN
*
*.....WRITE A TITLE IF NECESSARY
      IF ( .NOT. TITLED(LUN) )  THEN
         CALL KAT(LUN)
         WRITE(JIDMSG,100) JOBID
         CALL KAXED(LUN,JIDMSG)
         TITLED(LUN) = .TRUE.
      ENDIF
*
      CALL KAXED(LUN,TEXT)
*
 100  FORMAT('CRNKA360 KAXMSG: THE KEY FOR THIS RUN IS ',I11)
*
      END
