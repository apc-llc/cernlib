*
* $Id: tadiv.F,v 1.1.1.1 1996/02/15 17:48:43 mclareni Exp $
*
* $Log: tadiv.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:43  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE          RADIV(IDIM,X,Y,Z)
          REAL                X(*), Y(*), Z(*)
#include "adiv.inc"
          SUBROUTINE          DADIV(IDIM,X,Y,Z)
          DOUBLE PRECISION    X(*), Y(*), Z(*)
#include "adiv.inc"
          SUBROUTINE          CADIV(IDIM,X,Y,Z)
          COMPLEX             X(*), Y(*), Z(*)
#include "adiv.inc"
          REAL FUNCTION       RAMPA(IDIM,X,Y,S)
          REAL                X(*), Y(*), S
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM
#endif
          RAMPA  =  S
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMRDBLE)
          DO 10     J  =  1, IDIM
             RAMPA  =  RAMPA + X(J)*Y(J)
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMRDBLE)
          SUM  =  DBLE(S)
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DBLE(X(J))*DBLE(Y(J))
  10         CONTINUE
          RAMPA  =  SNGL(SUM)
#endif
          RETURN
          END
          DOUBLE PRECISION FUNCTION DAMPA(IDIM,X,Y,S)
          DOUBLE PRECISION    X(*), Y(*), S
          DAMPA  =  S
          IF(IDIM  .LE.  0)  RETURN
          DO 10     J  =  1, IDIM
             DAMPA  =  DAMPA + X(J)*Y(J)
  10         CONTINUE
          RETURN
          END
          COMPLEX FUNCTION    CAMPA(IDIM,X,Y,S)
          COMPLEX             X(*), Y(*), S
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX             A
          COMPLEX*16          SUM, DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
#endif
          CAMPA  =  S
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMCDBLE)
          DO 10     J  =  1, IDIM
             CAMPA  =  CAMPA + X(J)*Y(J)
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMCDBLE)
          SUM  =  S
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DC(X(J))*DC(Y(J))
  10         CONTINUE
          CAMPA  =  SUM
#endif
          RETURN
          END
          COMPLEX FUNCTION    CAMPAC(IDIM,X,Y,S)
          COMPLEX             X(*), Y(*), S
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX             A
          COMPLEX*16          SUM, DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
#endif
          CAMPAC  =  S
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMCDBLE)
          DO 10     J  =  1, IDIM
             CAMPAC  =  CAMPAC + X(J)*CONJG(Y(J))
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMCDBLE)
          SUM  =  S
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DC(X(J))*DCONJG(DC(Y(J)))
  10         CONTINUE
          CAMPAC  =  SUM
#endif
          RETURN
          END
          REAL FUNCTION       RAMPY(IDIM,X,Y)
          REAL                X(*), Y(*)
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    SUM
#endif
          RAMPY  =  0.
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMRDBLE)
          DO 10     J  =  1, IDIM
             RAMPY  =  RAMPY + X(J)*Y(J)
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMRDBLE)
          SUM  =  0.D0
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DBLE(X(J))*DBLE(Y(J))
  10         CONTINUE
          RAMPY  =  SNGL(SUM)
#endif
          RETURN
          END
          DOUBLE PRECISION FUNCTION DAMPY(IDIM,X,Y)
          DOUBLE PRECISION    X(*), Y(*)
          DAMPY  =  0.D0
          IF(IDIM  .LE.  0)  RETURN
          DO 10     J  =  1, IDIM
             DAMPY  =  DAMPY + X(J)*Y(J)
  10         CONTINUE
          RETURN
          END
          COMPLEX FUNCTION    CAMPY(IDIM,X,Y)
          COMPLEX             X(*), Y(*)
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX             A
          COMPLEX*16          SUM, DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
#endif
          CAMPY  =  (0.,0.)
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMCDBLE)
          DO 10     J  =  1, IDIM
             CAMPY  =  CAMPY + X(J)*Y(J)
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMCDBLE)
          SUM  =  (0.D0,0.D0)
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DC(X(J))*DC(Y(J))
  10         CONTINUE
          CAMPY  =  SUM
#endif
          RETURN
          END
          COMPLEX FUNCTION    CAMPYC(IDIM,X,Y)
          COMPLEX             X(*), Y(*)
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX             A
          COMPLEX*16          SUM, DC
          DC(A)  =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
#endif
          CAMPYC  =  (0.,0.)
          IF(IDIM  .LE.  0)  RETURN
#if !defined(CERNLIB_NUMCDBLE)
          DO 10     J  =  1, IDIM
             CAMPYC  =  CAMPYC + X(J)*CONJG(Y(J))
  10         CONTINUE
#endif
#if defined(CERNLIB_NUMCDBLE)
          SUM  =  (0.D0,0.D0)
          DO 10     J  =  1, IDIM
             SUM  =  SUM + DC(X(J))*DCONJG(DC(Y(J)))
  10         CONTINUE
          CAMPYC  =  SUM
#endif
          RETURN
          END
