*
* $Id: iopwk.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: iopwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.12/16 14/08/91  12.10.00  by  O.Couet
*-- Author :
      SUBROUTINE IOPWK(WKID,CONID,WTYPE)
*.===========>
*.
*.   Purpose:
*.   --------
*.   Initialize a display device
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      INTEGER WKID,CONID,WTYPE
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(WKID.LT.0)THEN
         CALL IGERR('Workstation identifier less than 0','IOPWK')
         RETURN
      ENDIF
*
*              PostScript Metafile
*
      IF(WTYPE.LT.0)THEN
         CALL IPINIT(WKID,ABS(CONID),WTYPE)
         RETURN
      ENDIF
*
      IF (DEVICES(WKID).EQ.0) THEN
         INOPWK=INOPWK+1
         DEVICES(WKID)=1
         ICONID=CONID
*
         IF (SEGOPN) THEN
            CALL JCLOSE
            SEGOPN =.FALSE.
         ENDIF
         CALL JDINIT (WKID)
         IF (WKID.EQ.0) THEN
            METACT = .TRUE.
         ELSE
            TEKACT = .TRUE.
         ENDIF
      ENDIF
*
      END
#endif
