*
* $Id: pzconn.F,v 1.1.1.1 1996/03/08 15:44:22 mclareni Exp $
*
* $Log: pzconn.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:22  mclareni
* Cspack
*
*
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
      SUBROUTINE PZCONN
#include "pzunit.inc"
*
*     Action routine for the CONNECT command
*
      CHARACTER*20 CHGRP,CHPASS
 
      CALL KUGETS(CHGRP,LGRP)
      CALL KUGETP(CHPASS,LPASS)
 
      CALL PZNTRC(LUNPZN,CHGRP(1:LGRP),CHPASS(1:LPASS),'U',IRC)
 
      IF(IRC.NE.0) THEN
         PRINT *,'PZCONN. error ',IRC,' updating .netrc file'
         RETURN
      ENDIF
*
*     Try opening connection to PIAF
*
      CALL CZOPEN('ZSERV','piaf1',IRC)
 
      IF(IRC.NE.0) THEN
         PRINT *,'PZCONN. cannot open connection to PIAF'
         CALL PZNTRC(LUNPZN,' ',' ','R',IRC)
         RETURN
      ENDIF
 
      END
