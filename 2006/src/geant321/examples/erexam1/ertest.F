*
* $Id: ertest.F,v 1.1.1.1 1996/03/06 15:36:25 mclareni Exp $
*
* $Log: ertest.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:25  mclareni
* Add geane321 examples
*
*
*      GEANE TRACKING
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE ERTEST
*
*     Test ERTRAK by generating a particle
*
*     Card Kine : itype  momentum  teta  phi
*
#include "geant321/ertrio.inc"
#include "geant321/gconst.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
*
      PARAMETER (NPMX = 5)
      DIMENSION X1(3),P1(3),ER1(15)
      DIMENSION X2(3),P2(3)
      DIMENSION XL(NPMX),PPL(3,3,NPMX),PLI(3,2)
      DIMENSION NUMV(NPMX),IOVL(NPMX)
      CHARACTER * 4 CNAMV(NPMX)
*
*
* *** Data for option 'P' and 'L'
*
      DATA XL    / 25.   , 50.     , 120.     , 220.     , 300.     /
*
* *** Data for option 'V'
*
      DATA CNAMV / 'CDET', 'ECAL', 'ECAL', 'HCAL', 'HCAL'  /
      DATA  NUMV /   1   ,   1   ,   1   ,   1   ,   1     /
      DATA  IOVL /   1   ,   1   ,   2   ,   1   ,   2     /
*
      CALL VZERO(ER1,15)
      CALL VZERO(PLI,6)
      CALL VZERO(PPL,9*NPMX)
*
      PLI(2,1) = 1.
      PLI(3,2) = 1.
*
      NP = 5
      DO 1 IP=1,NP
         PPL(2,1,IP) = 1.
         PPL(3,2,IP) = 1.
         PPL(1,3,IP) = XL(IP)
    1 CONTINUE
*
* *** Forward Tracking
*
      X1(1) = 0.
      X1(2) = 0.
      X1(3) = 0.
*
      PMOM  = PKINE(1)
      TETA  = PKINE(2)*DEGRAD
      PHI   = PKINE(3)*DEGRAD
      P1(1) = PMOM * SIN(TETA) * COS(PHI)
      P1(2) = PMOM * SIN(TETA) * SIN(PHI)
      P1(3) = PMOM * COS(TETA)
*
* *** Prepare the common /EROPTS/
*
      CALL EUFILL (NP,ER1,XL)
      CALL EUFILP (NP,ER1,PLI,PPL)
      CALL EUFILV (NP,ER1,CNAMV,NUMV,IOVL)
*
      CALL ERTRAK (X1,P1,X2,P2,IKINE,'P')
      IF (IDEBUG.EQ.1) THEN
         WRITE (LOUT, 1001)
         WRITE (LOUT, 1002) (X2(IP),IP=1,3),(P2(IP),IP=1,3),
     +               ((ERXOUT(IP,IR),IP=1,3),IR=1,NP),
     +               ((ERPOUT(IP,IR),IP=1,3),IR=1,NP),
     +               ((ERROUT(IP,IR),IP=1,15),IR=1,NP),
     +               (((ERTRSP(IP,IQ,IR),IP=1,5),IQ=1,5),IR=1,NP)
      END IF
*
* *** Backward tracking
*
      IF (ISWIT(3).LT.2) RETURN
*
      DO 2 I=1,3
         X1(I) = VECT(I)
         P1(I) = VECT(I+3)*VECT(7)
    2 CONTINUE
      CALL VZERO(ER1,15)
      NP = 5
*
      CALL EUFILV(NP,ER1,CNAMV,NUMV,IOVL)
*
      CALL ERTRAK(X1,P1,X2,P2,IKINE,'VB')
      IF (IDEBUG.EQ.1) THEN
         WRITE (LOUT, 1001)
         WRITE (LOUT, 1003) (X2(IP),IP=1,3),(P2(IP),IP=1,3),
     +               ((ERXOUT(IP,IR),IP=1,3),IR=1,NP),
     +               ((ERPOUT(IP,IR),IP=1,3),IR=1,NP),
     +               ((ERROUT(IP,IR),IP=1,15),IR=1,NP),
     +               (((ERTRSP(IP,IQ,IR),IP=1,5),IQ=1,5),IR=1,NP)
      END IF
*
 1001 FORMAT (/'  Result of the tracking'/)
 1002 FORMAT (/'  Track stopped at :',3F12.3,
     +         '     with momentum :',3F12.3,
     +        /'  Stored coordinates :  ',/,5(2X,3F12.3,/),
     +        /'  Stored momenta in SD :',/,5(2X,3F12.3,/),
     +        /'  Stored error-matrices in SD :',
     +        /   5(2X,10E13.4,/,2X,5E13.4,//),
     +        /'  Stored transport matrices in SD :',
     +        /   5(5(2X,5E13.4/),/))
 1003 FORMAT (/'  Track stopped at :',3F12.3,
     +         '     with momentum :',3F12.3,
     +        /'  Stored coordinates :  ',/,4(2X,3F12.3,/),
     +        /'  Stored momenta in SC :',/,4(2X,3F12.3,/),
     +        /'  Stored error-matrices in SC :',
     +        /   4(2X,10E13.4,/,2X,5E13.4,//),
     +        /'  Stored transport matrices in SC :',
     +        /   4(5(2X,5E13.4/),/))
*
      END
