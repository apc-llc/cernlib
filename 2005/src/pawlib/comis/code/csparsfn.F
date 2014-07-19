*
* $Id: csparsfn.F,v 1.2 1996/05/13 08:05:00 berejnoi Exp $
*
* $Log: csparsfn.F,v $
* Revision 1.2  1996/05/13 08:05:00  berejnoi
* Mods for SHL version on IBMRT
*
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/01 29/03/94  15.59.20  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   29/03/94
      SUBROUTINE CSPARSFN(FILENAME,PATH,NAME,FEXT)
      CHARACTER  *(*)     FILENAME,PATH,NAME,FEXT
      LN=LENOCC(FILENAME)
      IF(LN.GT.0)THEN
        I=1
 10     I1=INDEX(FILENAME(I:LN),'/')
        IF(I1.GT.0)THEN
          I=I+I1
          GO TO 10
        ENDIF
        IF(I.GT.1)THEN
          PATH=FILENAME(:I-1)
         ELSE
          PATH=' '
        ENDIF
        IB=I
 20     I1=INDEX(FILENAME(I:LN),'.')
        IF(I1.GT.0)THEN
          I=I+I1
          GO TO 20
        ENDIF
         IF(I.GT.IB .AND. I.LE.LN)THEN
          FEXT=FILENAME(I-1:LN)
          LN=I-2
        ELSE
          FEXT=' '
        ENDIF
        NAME=FILENAME(IB:LN)
      ELSE
        PATH=' '
        NAME=' '
        FEXT=' '
      ENDIF
      END
