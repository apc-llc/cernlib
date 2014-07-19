*
* $Id: poly2i.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: poly2i.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE POLY2I(N,IPOS)
#if defined(CERNLIB_NDP)
      DIMENSION IPOS(1000,2)
      IF (N .GT. 0) THEN
         IF (N .EQ. 1) THEN
            CALL SETPIX(IPOS(1,1),IPOS(1,2))
         ELSE
            CALL MOVE2I(IPOS(1,1),IPOS(1,2))
            DO 10 I=2,N
               CALL DRAW2I(IPOS(I,1),IPOS(I,2))
   10       CONTINUE
            IF (N .GT. 2) CALL DRAW2I(IPOS(1,1),IPOS(1,2))
#endif
#if !defined(CERNLIB_NDP)
      DIMENSION IPOS(2,1000)
      IF (N .GT. 0) THEN
         IF (N .EQ. 1) THEN
            CALL SETPIX(IPOS(1,1),IPOS(2,1))
         ELSE
            CALL MOVE2I(IPOS(1,1),IPOS(2,1))
            DO 10 I=2,N
               CALL DRAW2I(IPOS(1,I),IPOS(2,I))
   10       CONTINUE
            IF (N .GT. 2) CALL DRAW2I(IPOS(1,1),IPOS(2,1))
#endif
         END IF
      END IF
      END
#endif
