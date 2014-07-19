*
* $Id: rshell.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: rshell.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE RSHELL
*
*           To send a message to a remote machine
*     Method:
*         The message is sent
*         The ackowledgement is received in CHMESS
*         If CHMESS(1:1)='0' Return
*         If CHMESS(1:1)='1' Print CHMESS and Return
*         If CHMESS(1:1)='2' Print CHMESS and read next CHMESS
*         If CHMESS(1:1)='9' Close connection with remote machine
*
#include "cspack/czsock.inc"
      CHARACTER*80 CHMESS
      DIMENSION IHMESS(22),ICONTR(2)
 
*
#include "cspack/czopen.inc"
      CALL KUGETE(CHMESS,NCH)
      IF(NCH.LE.0)RETURN
*
      CALL JUMPST(IADTCP)
      ICONTR(1)=3
      ICONTR(2)=20
      CALL VBLANK(IHMESS,22)
      CALL UCTOH('MESS :'//CHMESS,IHMESS,4,NCH+6)
      CALL JUMPX2(IHMESS,ICONTR)
      IF(ICONTR(1).NE.0)GO TO 99
*
  10  ICONTR(1)=2
      ICONTR(2)=20
      CALL VBLANK(IHMESS,22)
      CALL JUMPX2(IHMESS,ICONTR)
      IF(ICONTR(1).NE.0)GO TO 99
      CALL UHTOC(IHMESS,4,CHMESS,80)
      IF(CHMESS(1:1).EQ.'0')GO TO 99
      IF(CHMESS(1:1).EQ.'9')THEN
         CALL CZCLOS(ISKIN)
         PRINT *,' Communication closed'
         ISKIN=-1
         GO TO 99
      ENDIF
      NCH=LENOCC(CHMESS)
      IF(NCH.GT.1)THEN
         PRINT *,CHMESS(2:NCH)
      ENDIF
      IF(CHMESS(1:1).EQ.'2')GO TO 10
*
  99  END
 
