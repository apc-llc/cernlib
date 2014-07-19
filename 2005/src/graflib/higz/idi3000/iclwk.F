*
* $Id: iclwk.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: iclwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.16/08 11/12/92  10.06.37  by  O.Couet
*-- Author :
      SUBROUTINE ICLWK(WKID)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Terminate a display device
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JDEND (DEVICE)
*.
*.   Parameters:
*.   -----------
*.     DEVICE                     [ INTEGER; Input ]
*.
*.      - Display device to be terminated.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/himeta.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
      INTEGER WKID
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#include "higz/di3seg.inc"
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
      IF(IGIWTY(WKID).LT.0)THEN
         CALL IPEND
         GOTO 1
      ENDIF
#endif
*
      CALL IGSG (0)
 1    IF (SEGOPN) THEN
         CALL JUPDAT
         CALL JCLOSE
         SEGOPN = .FALSE.
      ENDIF
      IF (DEVICES(WKID).NE.0) THEN
         IF (DEVICES(WKID).EQ.2) THEN
            CALL JFRAME
            CALL JDEVOF(WKID)
            INOPWK=INOPWK-1
         ENDIF
         CALL JDEND(WKID)
         DEVICES(WKID) = 0
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
