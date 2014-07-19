*
* $Id: dzdcrz.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdcrz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCRZ
*      IMPLICIT NONE
#include "dzdzbrinc.inc"
      INTEGER NCH, IFC, INDEXC
      CHARACTER*20 CHPATH
 
      CALL KUGETC(CHPATH,NCH)
      IF(NCH.LE.0)GOTO 999
      IFC=INDEX(CHPATH,'R_')
      IF(IFC.LE.0)IFC=INDEXC(CHPATH,'/')-2
      CALL RZCLOS(CHPATH(IFC+2:),' ')
      IF(INDEX(CHPATH(IFC+2:),'RZDOC').NE.0)CTDDOC=' '
*      CALL RZCLOS('//'//CHPATH(IFC+2:),' ')
999   END
***********************************************************************
