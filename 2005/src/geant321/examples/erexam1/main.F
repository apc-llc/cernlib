*
* $Id: main.F,v 1.1.1.1 1996/03/06 15:36:25 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:25  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
*
*
#include "geant321/gcflag.inc"
#include "geant321/gclist.inc"
#include "geant321/gctime.inc"
#include "geant321/gcunit.inc"
 
      PARAMETER (IGSPAC=100000 ,IHSPAC=10000)
      COMMON/GCBANK/Q(IGSPAC)
      COMMON/PAWC  /H(IHSPAC)
*
      CHARACTER*4 CLGET(20)
      DATA INPUT  /1/
*
* *** Initializations
*
      CALL GZEBRA( IGSPAC)
      CALL HLIMIT(-IHSPAC)
      CALL GINIT
      CALL GFFGO
      CALL GZINIT
*
* *** Build or readin the following Geant banks : JVOLU  JMATE  JTMED  JPART
      IF (NGET.LE.0) THEN
         CALL UGEOM
         CALL GPART
      ELSE
         DO  5 JGET=1, NGET
            CALL UHTOC(LGET(JGET),4,CLGET(JGET),4)
   5     CONTINUE
         CALL GOPEN (INPUT, 'I' ,0,IER)
         CALL GFIN  (INPUT,CLGET,NGET,IDENT,'I',IER)
         IF (IER.LT.0) GO TO 90
      ENDIF
*
      CALL GPHYSI
*
* *** Print control
      CALL GPRINT('MATE',0)
      CALL GPRINT('TMED',0)
      CALL GPRINT('VOLU',0)
 
      CALL TIMEX(TIMINT)
*
* *** Process the events
*
  10  CALL GTRIGC
      CALL GTRIGI
*
*     geant tracking , if required
      IF (ISWIT(3).GE.3) CALL GTRIG
*     geane tracking
      IF (ISWIT(3).GE.1) CALL ERTEST
*
      IF(IEVENT.LT.NEVENT) THEN
         IEVENT = IEVENT + 1
         GO TO 10
      ENDIF
*
* *** End of run
*
      IEORUN = 1
      CALL TIMEX(TIMNOW)
      TMEAN = 1000.*(TIMNOW - TIMINT)/FLOAT(IEVENT)
      PRINT 4000,TMEAN
*
   90 CONTINUE
*
      STOP
*
 4000 FORMAT(10X,'Time per event =',F10.3,' mseconds')
      END
