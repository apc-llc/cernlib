*
* $Id: igsa.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: igsa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.08/00 10/10/89  09.27.38  by  O.Couet
*-- Author :
      SUBROUTINE IGSA(IIWK)
*.===========>
*.
*.   Purpose:
*.   --------
*.
*.   IGSA switches a raster terminal from graphics to alpha mode.
*. The terminal must be an activated HIGZ workstation.This is done by
*. prompting the user who should respond by typing RETURN or any
*. character string followed by return.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hilun.inc"
#include "higz/hiatt.inc"
#include "higz/hiques.inc"
#include "higz/hiflag.inc"
#include "higz/di3dev.inc"
*
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
*.______________________________________
*
#if defined(CERNLIB_IBM)
      INTEGER IDDRV
#endif
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
      IF(GRFLAG)THEN
         IF (ENTRY_FLAG) CALL JUPDAT
         GRFLAG=.FALSE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=IDID
         IF(IQUEST(100).EQ.-1)THEN
            IQUEST(100)=0
         ENDIF
         IF (DEVICES(IWK).NE.0) THEN
#if defined(CERNLIB_VAX)
            IF (DRIVER(IWK).EQ.'COD') THEN
               WRITE(LUNOUT,'(1X,A1)') CHAR(24)
            ELSEIF (
     +         (DRIVER(IWK).EQ.'240').OR.(DRIVER(IWK).EQ.'24Y').OR.
     +         (DRIVER(IWK).EQ.'241').OR.(DRIVER(IWK).EQ.'24Z').OR.
     +         (DRIVER(IWK).EQ.'330').OR.(DRIVER(IWK).EQ.'340')
     +         ) THEN
               WRITE(LUNOUT,'(1X,A2)') CHAR(27)//'\'
            ELSEIF (DRIVER(IWK).EQ.'HDS') THEN
               WRITE(LUNOUT,'(1X,A15)') CHAR(27)//'[?38l'//
     +            CHAR(27)//'='//CHAR(27)//'<'//CHAR(27)//'[?1h'
            ELSEIF (DRIVER(IWK).EQ.'MAC') THEN
               WRITE(LUNOUT,'(1X,A4)') CHAR(27)//'%!2'
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
