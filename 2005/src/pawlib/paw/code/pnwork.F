*
* $Id: pnwork.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pnwork.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 24/08/93  17.21.22  by  Fons Rademakers
*-- Author :    Fons Rademakers   11/05/93
      SUBROUTINE PNWORK(IMODE)
************************************************************************
*                                                                      *
*  Routine takes care of printing progress report of Ntuple chain      *
*  processing. Handles Motif case via jump table.                      *
*  IMODE=1, initialize or popup, IMODE=2, report progress, IMODE=3,    *
*  final statistics or popdown of work dialog.                         *
*                                                                      *
************************************************************************
*
#include "paw/pcntwk.inc"
#include "paw/pcaddr.inc"
*
      IF (IMODE .EQ. 1) THEN
*
*-- status message or popup Motif Work dialog
*
         IEDONE = 0
         NTCONT = 0
         IF (JNTWINI .EQ. 0) THEN

         ELSE
            CALL JUMPST(JNTWINI)
            CALL JUMPX0
            CALL JUMPST(JNTWCHK)
            CALL JUMPX0
         ENDIF
*
      ELSEIF (IMODE .EQ. 2) THEN
*
         IF (JNTWCHK .EQ. 0) THEN

         ELSE
            CALL JUMPST(JNTWCHK)
            CALL JUMPX0
         ENDIF
*
      ELSEIF (IMODE .EQ. 3) THEN
*
         IF (JNTWCHK .EQ. 0) THEN

         ELSE
            NTCONT = 1
            CALL JUMPST(JNTWCHK)
            CALL JUMPX0
         ENDIF
*
      ENDIF
*
      END
