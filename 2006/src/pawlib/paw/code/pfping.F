*
* $Id: pfping.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfping.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.04/12 15/02/94  17.15.19  by  Fons Rademakers
*-- Author :    Fons Rademakers   15/02/94
      SUBROUTINE PFPING(ISLAV, IACT, ISTAT)
*
* Check if a Piaf server (ISLAV=0 is master) is alive. If IACT=1 then
* close connection in case server does not respond.
*
#include "hbook/hcpiaf.inc"
*
      ISTAT = 0
*
      IF (CONNPF) THEN
         CALL PFSOCK(ISLAV)
         CALL CZPUTA('PING :', ISTAT)
         IF (IACT.EQ.1 .AND. ISTAT.NE.0 .AND. ISLAV.EQ.0) THEN
            CALL KUEXEC('disconnect')
         ENDIF
         IF (ISLAV .NE. 0) CALL PFSOCK(0)
      ENDIF
*
      END
#endif
