*
* $Id: ftubes.F,v 1.1.1.1 1995/10/24 10:22:24 cernlib Exp $
*
* $Log: ftubes.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE FTUBES(ITYP,UCELL,VCELL,IFLAG,NEWMED)
*******************************************************************
*
*    Search for the tube (NEWMED = 5) next to the vertex
*     INPUT:
*        ITYP=0 SEARCHING IN X, Y AND Z.
*        ITYP=1 SEARCHING IN X ONLY.
*        ITYP=2 SEARCHING IN Y ONLY.
*        UCELL = COORDINATES OF THE ORIGIN
*     OUTPUT:
*        VCELL = COORDINATES OF A POINT INSIDE THE TUBE NEXT.
*        IFLAG = 1 IF O.K., = 0 IF FAILED.
*
*     CALLED BY HBOOTG
*******************************************************************
#include "geant321/gcflag.inc"
#include "geant321/gctmed.inc"
      DIMENSION VCELL(3),UCELL(3),SHIFT(3),NMEDA(100,100),BSHIFT(2)
      DATA SHIFT/0.1,0.1,0.74/,BSHIFT/0.21,-0.21/
*
      IFLAG=0
      NFAIL=0
      CALL UCOPY(UCELL,VCELL,3)
#if defined(CERNLIB_DEBUG)
      DO 10 I=1,100
         VCELL(3)=UCELL(3)+0.015*I
         DO 10 J=1,100
            VCELL(1)=UCELL(1)+0.01*J
            CALL GMEDIA(VCELL,NEWMED)
            IF(N0.NE.NEWMED) PRINT 10000, I,J,VCELL,NEWMED
10000         FORMAT(' I,J,VCELL,NEWMED',2I4,3F9.3,I3)
            NMEDA(J,I)=NEWMED
            N0=NEWMED
   10 CONTINUE
      PRINT 10100,UCELL,NMEDA
10100  FORMAT(' UCELL',3F9.5,'  NEWMED',100(/,100I1))
#endif
*
      IF(ITYP.EQ.0.AND.NUMED.EQ.6) GOTO 30
*
      IF(ITYP.EQ.0) THEN
         VCELL(3)=VCELL(3)+SHIFT(3)
      ELSE
         VCELL(ITYP)=VCELL(ITYP)+SHIFT(ITYP)
      ENDIF
   20 CALL GMEDIA(VCELL,NEWMED)
*
      IF(NEWMED.EQ.5) GOTO 50
      IF(NEWMED.EQ.0.OR.NEWMED.EQ.7) GOTO 60
      NFAIL=NFAIL+1
      IF(NFAIL.GT.3) GOTO 60
      IF(ITYP.EQ.0) THEN
         VCELL(1)=VCELL(1)+0.1
         VCELL(2)=VCELL(2)+0.1
         VCELL(3)=VCELL(3)+0.02
      ELSE
         VCELL(ITYP)=VCELL(ITYP)+SHIFT(ITYP)
      ENDIF
      GOTO 20
*
*     THE VERTEX IS IN THE BRASS
*          SEARCHING IN TWO DIRECTIONS: UP AND DOWN
   30 CONTINUE
      DO 40  J=1,2
         CALL UCOPY(UCELL,VCELL,3)
         VCELL(3)=UCELL(3)+BSHIFT(J)
         CALL GMEDIA(VCELL,NEWMED)
         IF(NEWMED.EQ.5) GOTO 50
         IF(NEWMED.EQ.0.OR.NEWMED.EQ.7) GOTO 60
         IF(NEWMED.EQ.3.OR.NEWMED.EQ.1) GOTO 40
         IF(NEWMED.EQ.6) THEN
            VCELL(1)=VCELL(1)+0.1
            VCELL(2)=VCELL(2)+0.1
            CALL GMEDIA(VCELL,NEWMED)
            IF(NEWMED.EQ.5) GOTO 50
            IF(NEWMED.EQ.0.OR.NEWMED.EQ.7) GOTO 60
*           IF(NEWMED.EQ.3.OR.NEWMED.EQ.1) GOTO 510
         ENDIF
   40 CONTINUE
      RETURN
*
   50 IFLAG=1
*
   60 CONTINUE
      END
