*
* $Id: idawk.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: idawk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/06 09/03/89  15.02.58  by  O.Couet
*-- Author :
      SUBROUTINE IDAWK(WKID)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Deselect a display device.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JDEVOF (DEVICE)
*.
*.   Parameters:
*.   -----------
*.     DEVICE                     [ INTEGER; Input ]
*.
*.      - Display device to be deselected.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/himeta.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(IGIWTY(WKID).LT.0)THEN
         PFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
C
C     ***** Deselect DEVICE, disabling output to
C     ***** and input from DEVICE
C
      IF (SEGOPN) THEN
         CALL JUPDAT
         CALL JCLOSE
         SEGOPN = .FALSE.
      ENDIF
      IF (DEVICES(WKID).EQ.2) THEN
         CALL JFRAME
         CALL JDEVOF(WKID)
         DEVICES(WKID) = 1
         INOPWK=INOPWK-1
         IF (WKID.EQ.0) THEN
            METACT = .FALSE.
         ELSE
            TEKACT = .FALSE.
            DO 10 I = 1 , MAXDEV
   10       TEKACT = TEKACT .OR. (DEVICES(I).NE.0)
         ENDIF
      ENDIF
*
      END
#endif
