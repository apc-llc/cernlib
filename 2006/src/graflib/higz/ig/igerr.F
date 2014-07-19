*
* $Id: igerr.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igerr.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/07 04/11/91  16.28.50  by  O.Couet
*-- Author :
      SUBROUTINE IGERR(ERRTEX,RNAME)
*.===========>
*.
*.   This prints on the screen the error message (ERRTEX) and
*. the routine name (RNAME). This routine sets also IQUEST(1)
*. to 1. If RNAME equal ' ' only a warning is printed and
*. IQUEST(1) is sets to 0.
*.
*. _Input parameters:
*.
*.  INTEGER ERRTEX : Error text .
*.  CHARACTER RNAME : Routine name .
*.
*..==========> (O.Couet)
#include "higz/hilun.inc"
#include "higz/hiques.inc"
      CHARACTER*(*) RNAME,ERRTEX
      CHARACTER*80 CHTEXT
      LOGICAL OPND
      SAVE IFIRST
      DATA IFIRST /1/
*.___________________________________________
*
      IF(IFIRST.NE.0.AND.LUNERR.NE.LUNOUT)THEN
         INQUIRE(UNIT=LUNERR,OPENED=OPND)
         IF(.NOT.OPND)THEN
#if defined(CERNLIB_IBM)
            OPEN(LUNERR,FILE='/HIGZ ERR',
     +           FORM='FORMATTED',STATUS='UNKNOWN')
#endif
#if !defined(CERNLIB_IBM)
            OPEN(LUNERR,FILE='higz.err',
     +           FORM='FORMATTED',STATUS='UNKNOWN')
#endif
         ENDIF
      ENDIF
      IFIRST=0
      CALL IGSA(0)
      NR=LENOCC(RNAME)
      NE=LENOCC(ERRTEX)
      IF(NR.GT.1)THEN
         CHTEXT=' ***** ERROR in '//RNAME(1:NR)//' : '//ERRTEX(1:NE)
         NC=NE+NR+20
         IQUEST(1)=1
      ELSE
         CHTEXT=' *** '//ERRTEX(1:NE)
         NC=NE+6
         IQUEST(1)=0
      ENDIF
*
      WRITE(LUNERR,'(A)')CHTEXT(1:NC)
*
      END
