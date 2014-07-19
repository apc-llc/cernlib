*
* $Id: gettitle.F,v 1.1.1.1 1996/03/01 11:39:05 mclareni Exp $
*
* $Log: gettitle.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/00 25/10/93  12.59.00  by  Rene Brun
*-- Author :    Fons Rademakers   20/03/92
      SUBROUTINE GETTITLE(LIST, OBJ, TITLE)
************************************************************************
*                                                                      *
* GETTITLE                                                             *
*                                                                      *
* Gets full name of object in LIST. If LIST=1 then object is a file    *
* LUN and TIT will be the full filename, if LIST=2 then the object     *
* is an item in the file and TITLE will be its full name.              *
*                                                                      *
************************************************************************
*
#include "hbook/hcdire.inc"
#include "paw/quest.inc"
*
      CHARACTER*(*) OBJ, TITLE
      CHARACTER*128 CHPATH
      INTEGER       LIST
*
      IF (LIST .EQ. 1) THEN
         DO 10 I = 1, NCHTOP
            IF (OBJ .EQ. CHTOP(I)) THEN
               TITLE = HFNAME(I)
               RETURN
            ENDIF
10       CONTINUE
      ELSE
         CALL HCDIR(CHPATH, 'R')
         CALL IZCTOI(OBJ,IOBJ)
         IF (IQUEST(1) .EQ. 1) THEN
            TITLE = 'Directory'
            RETURN
         ENDIF
         CALL MHLATTR(CHPATH, IOBJ, TITLE, NX, XMIN, XMAX,
     +                                   NY, YMIN, YMAX, ISW, .FALSE.)
      ENDIF
*
      END
