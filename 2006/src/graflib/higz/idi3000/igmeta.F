*
* $Id: igmeta.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: igmeta.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 06/06/89  09.20.39  by  O.Couet
*-- Author :
      SUBROUTINE IGMETA (IUNIT,METAFL)
*.===========>
*.
*.   To OPEN, activate, deactivate and close metafile or screen
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
      INTEGER WKID
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
*._____________________________
*
*
      WKID = METAFL
      IF (IUNIT.EQ.0) THEN
         CALL IDAWK(WKID)
         IF (WKID.NE.IDID) CALL ICLWK(WKID)
      ELSE IF (IUNIT.EQ.1) THEN
         CALL IOPWK(WKID,0,WKID)
         CALL IACWK(WKID)
      ELSE
         CALL IGERR('Invalid ON/OFF flag (0/1)','IGMETA')
      ENDIF
*
 999  END

#endif
