*
* $Id: igrsiz.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igrsiz.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_X11)
*CMZ :  1.15/00 13/07/92  17.19.17  by  O.Couet
*-- Author :    O.Couet   14/06/91
      SUBROUTINE IGRSIZ(IWKID)
*.===========>
*.
*.  Resize the X11 window and display the current
*.  picture if it exists.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiques.inc"
      CHARACTER*(32) CHTEMP
*.______________________________________
*
      IW=IWINID(IGIWIN(IWKID))
      CALL IXRSIZ(IW)
      CALL IZPICT(CHTEMP,'R')
      IF(CHTEMP.NE.' ')THEN
         DO 10 I=1,INOPWK
            CALL IGQWK(ILOPWK(I),'ACTI',RQUEST(40+I))
            IF(ILOPWK(I).NE.IWKID)THEN
               CALL IDAWK(ILOPWK(I))
            ELSE
               CALL IACWK(IWKID)
            ENDIF
   10    CONTINUE
         CALL IZPICT(CHTEMP,'D')
         DO 20 I=1,INOPWK
            IF(RQUEST(40+I).EQ.0.)THEN
               CALL IDAWK(ILOPWK(I))
            ELSE
               CALL IACWK(ILOPWK(I))
            ENDIF
   20    CONTINUE
         CALL IXSAVWI
         CALL IXSELWI(IW)
         CALL IXUPDWI(1)
         CALL IXSETWI
      ENDIF
*
      END
#endif
