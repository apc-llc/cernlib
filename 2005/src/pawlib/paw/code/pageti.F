*
* $Id: pageti.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: pageti.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/00 13/04/93  08.58.32  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAGETI(ID)
*.
*.       Read histogram identifier from CHTEMP
*.
      CHARACTER*8 CHTEMP
      SAVE CHTEMP
*
      CALL KUGETC(CHTEMP,NCH)
      IDOT=INDEX(CHTEMP,'.')
      IF(IDOT.NE.0)THEN
         IF(IDOT.NE.NCH)THEN
            CALL HBUG('+Illegal IDentifier: '//CHTEMP,'PAGETI',0)
         ENDIF
         NCH=IDOT-1
      ENDIF
      CALL HGETCO(CHTEMP(1:NCH),ID)
      END

