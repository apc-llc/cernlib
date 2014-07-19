*
* $Id: dzdase.F,v 1.1.1.1 1996/03/04 16:13:02 mclareni Exp $
*
* $Log: dzdase.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:02  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDASE
      CHARACTER*80 CHFILE, CMD
#include "dzdzbrinc.inc"
      INTEGER ISTAT
      CALL KSVPAR(CHFILE, CMD)
*      WRITE(*,*)'DZDASE: File: ',CHFILE(1:LENOCC(CHFILE)),
*     &' Command: ',CMD(1:LENOCC(CMD))
 
      IF      (INDEX(CHFILE,'.scratch').NE.0)THEN
         CALL KUOPEN(LUPTMP,CHFILE,'UNKNOWN',ISTAT)
         IF(ISTAT.EQ.0)CALL KICLOS(LUPTMP,'DELETE',ISTAT)
      ELSE IF (INDEX(CHFILE,'.dzdoc')  .NE.0)THEN
         CALL DZDUDD(CHFILE)
      ELSE
         WRITE(*,*)'DZDASE: Unknown file:',CHFILE
      ENDIF
      END
*********************************************************************
