*
* $Id: gukine.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: gukine.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GUKINE
*
*     Card  'kine' :  itype    momentum
*
#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
*
      COMMON /COORD/CTRU(6,10),NMEA(10),charg
*
      DIMENSION VERTEX(3),PLAB(3)
      LOGICAL FIRST, LRANP, LRANC
      SAVE    FIRST,VERTEX,PLAB, LRANP, LRANC
      DATA FIRST /.TRUE./
      DATA VERTEX/-109.998,0.0,0.0/,PLAB/0.0,0.0,0.0/
*
**____________________________________________________________________
*
      IF (FIRST) THEN
         LRANP = .FALSE.
         IF (PKINE(1).LE.0) THEN
           LRANP = .TRUE.
           PKINE(1) = ABS(PKINE(1))
           print *,' Random momentum'
         ENDIF
         LRANC = .FALSE.
         IF (IKINE.LE.0) THEN
           LRANC = .TRUE.
           IKINE = ABS(IKINE)
           print *,' Random charge'
         ENDIF
         JPA = LQ(JPART-IKINE)
         XMASS   = Q(JPA+7)
         PLAB(1) = PKINE(1)
         PLAB(2) = .1*PLAB(1)
         PLAB(3) = .1*PLAB(1)
         EVERT   = sqrt(dble(plab(1))**2+dble(xmass)**2) - xmass
      ENDIF
*
      LKINE = IKINE
      IF (LRANC.AND. RNDM(IKINE).LE.0.5) LKINE = IKINE + 1
      JPA = LQ(JPART-LKINE)
      ITRTYP = Q(JPA+6)
      AMASS  = Q(JPA+7)
      CHARGE = Q(JPA+8)
      TLIFE  = Q(JPA+9)
      charg = charge
*
      CALL GSVERT(VERTEX,0,0,0,0,NVERT)
*
      IF (LRANP) THEN
        PP  = 10**(1.+RNDM(PP))
        PLAB(2) = .2*PP*(1.-2.*RNDM(PLAB(2)))
        PLAB(3) = .2*PP*(1.-2.*RNDM(PLAB(3)))
        PLAB(1) = SQRT(PP**2-PLAB(2)**2-PLAB(3)**2)
        EVERT   = sqrt(dble(PP)**2+dble(xmass)**2) - xmass
      ENDIF
*
      CALL GSKINE(PLAB,LKINE,NVERT,0,0,NT)
*
      IF (FIRST) THEN
         CALL GPRINT('VERT',0)
         CALL GPRINT('KINE',0)
         FIRST = .FALSE.
      ENDIF
*
      END
