*
* $Id: igmeta.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igmeta.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if !defined(CERNLIB_DI3000)
*CMZ :  1.18/01 04/05/93  11.22.25  by  O.Couet
*-- Author :
      SUBROUTINE IGMETA (IIUNIT,METAFL)
*.==========>
*.
*.   To open, activate, deactivate and close metafile or screen
*.
*..=========>
#include "higz/himeta.inc"
#include "higz/himet2.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ATCGKS)
      CHARACTER*24 METAFILE
#endif
*._____________________________
*
      IUNIT = IIUNIT
      IF(IIUNIT.EQ.999)IUNIT = 0
*
*              Output to screen and metafile
*
      IWKID=1
      IF (IUNIT.GT.0) THEN
         GFLAG=.TRUE.
         IF (.NOT.TEKACT) THEN
            CALL IACWK(IWKID)
            TEKACT=.TRUE.
         ENDIF
         IF (.NOT. METACT) THEN
            IF (.NOT. FILOPN) THEN
               IF(METAFL.NE.0)THEN
                  IWT=METAFL
               ELSE
                  IWT=METDEF
               ENDIF
#if defined(CERNLIB_ATCGKS)
            IF(CHMETA.EQ.' ') THEN
               IF(IWT.EQ.10100)THEN
                  METAFILE='cgmb.met'
               ELSEIF(IWT.EQ.10110)THEN
                  METAFILE='cgmc.met'
               ELSEIF(IWT.EQ.10120)THEN
                  METAFILE='cgmt.met'
               ELSEIF(IWT.GE.1500.AND.IWT.LE.1505)THEN
                  METAFILE='talaris.higz'
               ELSEIF(IWT.GE.1900.AND.IWT.LE.1905)THEN
                  METAFILE='postscr.higz'
               ELSEIF(IWT.GE.5300.AND.IWT.LE.5350)THEN
                  METAFILE='x11.higz'
               ELSEIF(IWT.GE.6300.AND.IWT.LE.6307)THEN
                  METAFILE='imagen.higz'
               ELSE
                  METAFILE='printer.higz'
               ENDIF
            ELSE
               METAFILE=CHMETA
            ENDIF
            CALL GUESC050(IUNIT,METAFILE)
#endif
               CALL IOPWK (IDMETA,IUNIT,IWT)
#if !defined(CERNLIB_MSDOS)
               FILOPN = .TRUE.
#endif
#if defined(CERNLIB_MSDOS)
               IF(IWT.NE.METDEF)FILOPN = .TRUE.
#endif
            ENDIF
            CALL IACWK(IDMETA)
            METACT = .TRUE.
         ENDIF
         RETURN
      ENDIF
*
*              Output to screen only
*
      IF(IUNIT.EQ.0)THEN
         CALL IGQWK(IWKID,'OPEN',RVAL)
         IF(RVAL.NE.0.)THEN
            GFLAG=.TRUE.
            IF (.NOT.TEKACT) THEN
               CALL IACWK(IWKID)
               TEKACT = .TRUE.
            ENDIF
         ENDIF
         IF (METACT) THEN
            CALL IDAWK(IDMETA)
            METACT = .FALSE.
         ENDIF
         IF(IIUNIT.EQ.999)THEN
            CALL ICLWK(IDMETA)
            FILOPN=.FALSE.
         ENDIF
         RETURN
      ENDIF
*
*              Output to metafile file only
*
      IF (TEKACT) THEN
         CALL IDAWK(IWKID)
         TEKACT = .FALSE.
      ENDIF
      IF (.NOT. METACT) THEN
         IF (.NOT. FILOPN) THEN
            IF(METAFL.NE.0)THEN
               IWT=METAFL
            ELSE
               IWT=METDEF
            ENDIF
#if defined(CERNLIB_ATCGKS)
            IF(CHMETA.EQ.' ') THEN
               IF(IWT.EQ.10100)THEN
                  METAFILE='cgmb.met'
               ELSEIF(IWT.EQ.10110)THEN
                  METAFILE='cgmc.met'
               ELSEIF(IWT.EQ.10120)THEN
                  METAFILE='cgmt.met'
               ELSEIF(IWT.GE.1500.AND.IWT.LE.1505)THEN
                  METAFILE='talaris.higz'
               ELSEIF(IWT.GE.1900.AND.IWT.LE.1905)THEN
                  METAFILE='postscr.higz'
               ELSEIF(IWT.GE.5300.AND.IWT.LE.5350)THEN
                  METAFILE='x11.higz'
               ELSEIF(IWT.GE.6300.AND.IWT.LE.6307)THEN
                  METAFILE='imagen.higz'
               ELSE
                  METAFILE='printer.higz'
               ENDIF
            ELSE
                  METAFILE=CHMETA
            ENDIF
            CALL GUESC050(-IUNIT,METAFILE)
#endif
            CALL IOPWK (IDMETA,-IUNIT,IWT)
#if !defined(CERNLIB_MSDOS)
            FILOPN = .TRUE.
#endif
#if defined(CERNLIB_MSDOS)
            IF(IWT.NE.METDEF)FILOPN = .TRUE.
#endif
         ENDIF
         CALL IACWK(IDMETA)
         METACT = .TRUE.
         IF(IWT.LT.0)GFLAG=.FALSE.
      ENDIF
*
      END
#endif
