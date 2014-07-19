*
* $Id: iacwk.F,v 1.1.1.1 1996/02/14 13:10:27 mclareni Exp $
*
* $Log: iacwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:27  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.02.57  by  O.Couet
*-- Author :
      SUBROUTINE IACWK(WKID)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Select a display device
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JDEVON (DEVICE)
*.
*.   Parameters:
*.   -----------
*.     DEVICE                     [ INTEGER; Input ]
*.
*.      - Display device to be selected.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/himeta.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(WKID.LT.0)THEN
         CALL IGERR('Workstation identifier less than 0','IACWK')
         RETURN
      ENDIF
*
*           Select a display device
*
      IF (DEVICES(WKID).EQ.1) THEN
         IF (SEGOPN) THEN
            CALL JCLOSE
            SEGOPN =.FALSE.
         ENDIF
         CALL JDEVON (WKID)
         DEVICES(WKID) = 2
         INOPWK=INOPWK+1
         IF (WKID.EQ.0) THEN
            METACT = .TRUE.
         ELSE
            TEKACT = .TRUE.
         ENDIF
      ELSE IF (DEVICES(WKID).EQ.0) THEN
         CALL IGERR('Workstation not initialized','IACWK')
      ENDIF
*
      END
#endif
