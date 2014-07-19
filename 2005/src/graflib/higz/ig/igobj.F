*
* $Id: igobj.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: igobj.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_X11)
*CMZ :  1.23/03 04/09/95  11.56.42  by  O.Couet
*-- Author :    O.Couet   26/08/92
      SUBROUTINE IGOBJ(IX,IY,NBLEV,IPID,CHPID)
*.===========>
*.
*.   This routine return to KUIP/Motif the object picked
*. on the HIGZ/X11 graphics window.
*.
*. _Input parameters:
*.
*.  INTEGER IX,IY : Cursor position
*.  INTEGER NBLEV : Window number
*.
*. _Output parameters:
*.
*.  INTEGER NBLEV : Number of level in the pick path
*.  INTEGER IPID(NBLEV) : Integer ID.
*.  CHARACTER CHPID(NBLEV) : Character ID. CHPID(1)=Class name
*.
*..==========>
      CHARACTER*(*) CHPID(*)
      DIMENSION IPID(*)
*.______________________________________
*
      X     = FLOAT(IX)
      Y     = FLOAT(IY)
      IWIN  = NBLEV
      NBLEV = 0
      CALL IZPICT(' ','QO')
      CALL IRQLC(1,99,ISTAT,NT,X,Y)
      CALL IGPICK(NT,X,Y,NBLEV,CHPID,IPID,' ')
*
      IF(NBLEV.EQ.0)THEN
         NT2 = 0
   10    IF (NT2.NE.NT) THEN
            CALL ISELNT(NT2)
            X = FLOAT(IX)
            Y = FLOAT(IY)
            CALL IRQLC(1,99,ISTAT,NTT,X,Y)
            IF (NTT.EQ.NT2) THEN
               CALL IGPICK(NT2,X,Y,NBLEV,CHPID,IPID,' ')
               IF (NBLEV.NE.0) RETURN
            ENDIF
         ENDIF
         NT2 = IZGNNT(NT2)
         IF (NT2.GE.0) GOTO 10
         NBLEV    = 1
         CHPID(1) = 'win'
         IPID(1)  = IWIN
      ENDIF
*
      END
#endif
