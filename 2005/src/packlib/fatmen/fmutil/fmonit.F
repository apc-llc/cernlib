*
* $Id: fmonit.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmonit.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_IBMVM)
      SUBROUTINE FMONIT(ACTION)
#include "fatmen/fatsys.inc"
      CHARACTER*(*) ACTION
      INTEGER FMUSER,FMHOST
      CHARACTER*8   CHUSER,CHHOST,CHTYPE,CHSYS
 
      IC = FMUSER(CHUSER)
      IF(CHUSER(1:LENOCC(CHUSER)).EQ.SERNAM(1:LSN)) RETURN
 
      IC = FMHOST(CHHOST,CHTYPE,CHSYS)
      IF(CHHOST(1:LENOCC(CHHOST)).NE.'CERNVM') RETURN
*
*     Routine to log action via SMSG to service machine
*     Action will appear in normal console log
*
      CALL VMCMS('EXEC FATLOG '
     +//SERNAM(1:LENOCC(SERNAM))//' '//ACTION,IRC)
      END
 
#endif
