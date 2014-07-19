#include "paw/pilot.h"
*
*-- Author :    Dino Ferrero Merlino 19/04/96
*
*   Catching the HPLFR3 Hplot call on the PIAF side
*
      SUBROUTINE HPLFR3 (X1I,X2I,Y1I,Y2I,Z1I,Z2I,THETA,PHI,CHOPT)
#include "paw/pchost.inc"

      CHARACTER*(*) CHOPT
      CHARACTER*80 CHMAIL

*---- Old PAW client can't cope with HPLFR3 command
      IF (ISPROTO.GE.2) THEN
         CHMAIL='HPLFR3'//CHOPT
         CALL CZPUTA(CHMAIL,ISTAT)
         WRITE(CHMAIL,'(4E16.8)') X1I,X2I,Y1I,Y2I
         CALL CZPUTA(CHMAIL,ISTAT)
         WRITE(CHMAIL,'(4E16.8)') Z1I,Z2I,THETA,PHI 
         CALL CZPUTA(CHMAIL,ISTAT)
      ELSE
         CALL PANT3D(X1I,X2I,Y1I,Y2I,Z1I,Z2I,0,0,' ',' ',' ',CHOPT)
      ENDIF

      END
