*
* $Id: hwbmas.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbmas.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBMAS.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWBMAS
C------------------------------------------------------------------------
C     Passes  backwards through a  jet cascade  calculating the masses
C     and magnitudes of the longitudinal and transverse three momenta.
C     Components given relative to direction of parent for a time-like
C     vertex and with respect to z-axis for space-like vertices.
C
C     On input PPAR(1-5,*) contains:
C     (E*sqrt(Xi),Xi,3-mom (if external),E,M-sq (if external))
C
C     On output PPAR(1-5,*) (if TMPAR(*)), containts:
C     (P-trans,Xi or Xilast,P-long,E,M)
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IPAR,JPAR,KPAR,MPAR
      DOUBLE PRECISION HWUSQR,EXI,PISQ,PJPK,EJEK,PTSQ
      IF (IERROR.NE.0) RETURN
      IF (NPAR.GT.2) THEN
         DO 10 MPAR=NPAR-1,3,-2
         JPAR=MPAR
C Find parent and partner of this branch
         IPAR=JMOPAR(1,JPAR)
         KPAR=JPAR+1
C Determine type of branching
         IF (TMPAR(IPAR)) THEN
C Time-like branching
C           Compute mass of parent
            EXI=PPAR(1,JPAR)*PPAR(1,KPAR)
            PPAR(5,IPAR)=PPAR(5,JPAR)+PPAR(5,KPAR)+2.*EXI
C           Compute three momentum of parent
            PISQ=PPAR(4,IPAR)*PPAR(4,IPAR)-PPAR(5,IPAR)
            PPAR(3,IPAR)=HWUSQR(PISQ)
C           Compute daughter' transverse and longitudinal momenta
            PJPK=PPAR(3,JPAR)*PPAR(3,KPAR)
            EJEK=PPAR(4,JPAR)*PPAR(4,KPAR)-EXI
            PTSQ=(PJPK+EJEK)*(PJPK-EJEK)/PISQ
            PPAR(1,JPAR)=HWUSQR(PTSQ)
            PPAR(3,JPAR)=HWUSQR(PPAR(3,JPAR)*PPAR(3,JPAR)-PTSQ)
            PPAR(1,KPAR)=-PPAR(1,JPAR)
            PPAR(3,KPAR)= PPAR(3,IPAR)-PPAR(3,JPAR)
         ELSE
C Space-like branching
C           Re-arrange such that JPAR is time-like
            IF (TMPAR(KPAR)) THEN
               KPAR=JPAR
               JPAR=JPAR+1
            ENDIF
C           Compute time-like branch
            PTSQ=(2.-PPAR(2,JPAR))*PPAR(1,JPAR)*PPAR(1,JPAR)
     &          -PPAR(5,JPAR)
            PPAR(1,JPAR)=HWUSQR(PTSQ)
            PPAR(3,JPAR)=(1.-PPAR(2,JPAR))*PPAR(4,JPAR)
            PPAR(3,IPAR)=PPAR(3,KPAR)-PPAR(3,JPAR)
            PPAR(5,IPAR)=0.
            PPAR(1,KPAR)=0.
         ENDIF
C Reset Xi to Xilast
         PPAR(2,KPAR)=PPAR(2,IPAR)
  10     CONTINUE
      ENDIF
      DO 20 IPAR=2,NPAR
  20  PPAR(5,IPAR)=HWUSQR(PPAR(5,IPAR))
      PPAR(1,2)=0.
      PPAR(2,2)=0.
      END
