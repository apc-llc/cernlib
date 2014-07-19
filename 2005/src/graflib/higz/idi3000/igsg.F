*
* $Id: igsg.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: igsg.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.08/00 10/10/89  09.27.39  by  O.Couet
*-- Author :
      SUBROUTINE IGSG(IIWK)
*.===========>
*.
*.   Purpose:
*.   --------
*.
*.   IGSG switches a raster terminal from alpha to graphics mode .
*.   The terminal must be an activated HIGZ workstation .
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hilun.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/di3dev.inc"
*
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
*.______________________________________
*
#if defined(CERNLIB_VAX)
      LOGICAL START  /.TRUE./
      CHARACTER*6  LOGNAM
      CHARACTER*80 STRING
      INTEGER LENGTH
      CHARACTER*3 DRIVER(MAXDEV)
      SAVE START,DRIVER

      IF (START) THEN
         START=.FALSE.

*        Translate YD%DRV logical names to obtain devices.

         DO 100 I=1,MAXDEV
            WRITE(LOGNAM,'(A2,I1,A3)') 'YD',I,'DRV'
            STATUS = LIB$SYS_TRNLOG(LOGNAM,LENGTH,STRING)
            DRIVER(I) = STRING(LENGTH-6:LENGTH-4)
  100    CONTINUE
      END IF
#endif
      IF(.NOT.GRFLAG)THEN
         GRFLAG=.TRUE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=IDID
         IF (DEVICES(IWK).NE.0) THEN
#if defined(CERNLIB_VAX)
            IF (DRIVER(IWK).EQ.'COD') THEN
*              Do nothing.
            ELSEIF (
     +         (DRIVER(IWK).EQ.'240').OR.(DRIVER(IWK).EQ.'24Y').OR.
     +         (DRIVER(IWK).EQ.'241').OR.(DRIVER(IWK).EQ.'24Z').OR.
     +         (DRIVER(IWK).EQ.'330').OR.(DRIVER(IWK).EQ.'340')
     +         ) THEN
*              Do nothing.
            ELSEIF (DRIVER(IWK).EQ.'HDS') THEN
               WRITE(LUNOUT,'(1X,A6,$)') CHAR(27)//'[?38h'
            ELSEIF (DRIVER(IWK).EQ.'MAC') THEN
               WRITE(LUNOUT,'(1X,A8,$)') CHAR(27)//'%!0'//
     +            CHAR(27)//'LV0'
            ENDIF
#endif
#if defined(CERNLIB_IBM)
*           Insert proper code here!
#endif
         ENDIF
      ENDIF
*
      END

#endif
