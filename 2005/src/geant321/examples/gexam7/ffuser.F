*
* $Id: ffuser.F,v 1.1.1.1 1995/10/24 10:22:20 cernlib Exp $
*
* $Log: ffuser.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE FFUSER(IKEY)
C---------------------------------------------------------------------
C  CALLED WHEN A *NAME CARD IS READ
C---------------------------------------------------------------------
      CHARACTER*4 KEYW
 
      CALL UHTOC(IKEY,4,KEYW,4)
      IF (KEYW.EQ.'BOOT') CALL GBOOTI
 
      END
