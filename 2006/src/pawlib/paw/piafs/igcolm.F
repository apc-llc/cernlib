#include "paw/pilot.h"
*-- Author :    Dino Ferrero Merlino 19/4/96
      SUBROUTINE IGCOLM(X1,X2,Y1,Y2,IC1,IC2,ZMIN,ZMAX,CHOPT)
      CHARACTER*(*) CHOPT
      DIMENSION IC2(*)

      CHARACTER CHMAIL*80

#include "paw/pchost.inc"

*---- Old PAW client can't cope with IGCOLM command
      IF (ISPROTO.GE.2) THEN
         WRITE(CHMAIL,'(A,I12,A)') 'IGCOLM',IC2(1),CHOPT
         CALL CZPUTA(CHMAIL,ISTAT)
         WRITE(CHMAIL,'(3E16.8)') X1,X2,Y1
         CALL CZPUTA(CHMAIL,ISTAT)
         WRITE(CHMAIL,'(3E16.8)') Y2,ZMIN,ZMAX 
         CALL CZPUTA(CHMAIL,ISTAT)
      ENDIF
      END
