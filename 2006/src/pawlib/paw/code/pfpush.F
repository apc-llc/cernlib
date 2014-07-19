*
* $Id: pfpush.F,v 1.4 1996/09/09 12:36:54 couet Exp $
*
* $Log: pfpush.F,v $
* Revision 1.4  1996/09/09 12:36:54  couet
* - OLD cut routine removed
*
* Revision 1.3  1996/03/29 16:02:36  cernlib
* Replace TAB by blanks; cpp replaces a TAB by a single blank!
*
* Revision 1.2  1996/03/15 10:29:34  dinofm
* RLOGIN subroutine has been modified so that once the connection to PIAF is up,
* the PIAF master is queried about his version using the PIAF MESSAGE command
* back door.
*
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.05/15 14/07/94  16.17.54  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   13/04/93
      SUBROUTINE PFPUSH(ISTAT)
*
* reset the memory of the Piaf server and push objects which will be needed
*
#include "paw/pccsel.inc"
#include "hbook/hcpiaf.inc"
*
*
      CALL PFKUIP('histo/delete 0; vector/delete *; nt/cuts $0 -',
     +            ISTAT)
      IF(ISTAT.NE.0) GOTO 999
*
*-- send CSELECT status
*
      IF (IOPTCN .NE. 0) THEN
         WRITE(CHSMPF,'(A,1X,A8,F10.6)') 'nt/cselect',CSOPT,CSIZE
         CALL PFKUIP(CHSMPF,ISTAT)
         IF(ISTAT.NE.0) GOTO 999
      ENDIF
*
*-- send the cut definitions
*
      IMAX = MAXCUTS()
      DO 10 I=1,IMAX
         ICUTY =  ICUTTYPE(I)
         IF (ICUTY. EQ. 1) THEN
            CALL PACKCUT (I,CHLMPF,ISTAT)
            IF(ISTAT.NE.0) GOTO 999
            CALL PFKUIP(CHLMPF,ISTAT)
            IF(ISTAT.NE.0) GOTO 999
         ELSEIF (ICUTY .EQ. 2) THEN
*
*-- send the graphical cut definition
*
            CALL PFCUTS(I, 0, ISTAT)
            IF (ISTAT .NE. 0) GOTO 999
         ENDIF
 10   CONTINUE
*
 999  CONTINUE
      END
#endif
