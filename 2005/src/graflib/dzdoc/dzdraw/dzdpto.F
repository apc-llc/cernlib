*
* $Id: dzdpto.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdpto.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPTO(IWTYP)
      CHARACTER*4 ANYCHA
      IF(IWTYP.EQ.7878 .OR.
     &   IWTYP.EQ.   0 .OR.
     &   IWTYP.EQ.7800 .OR.
     &   IWTYP.EQ. 221 .OR.
     &   IWTYP.EQ. 101 .OR.
     &   IWTYP.EQ. 103 .OR.
     &   IWTYP.EQ. 121 .OR.
     &   IWTYP.EQ. 123 .OR.
     &   IWTYP.EQ. 125 .OR.
     &   IWTYP.EQ.1020 .OR.
     &   IWTYP.EQ.1021 .OR.
     &   IWTYP.EQ.1030)THEN
 
         CALL KUPROC(' Return to continue',ANYCHA,NCH)
      ENDIF
      END
***********************************************************************
