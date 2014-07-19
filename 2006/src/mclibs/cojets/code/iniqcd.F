*
* $Id: iniqcd.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: iniqcd.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE INIQCD
C     *****************
C-- INITIALIZES PARAMETERS USED BY JETQCD
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/itapes.inc"
#include "cojets/qcd.inc"
#include "cojets/tabpsq.inc"
#include "cojets/transl.inc"
      DATA ICALL/0/
C
      IF(ICALL.GT.0) RETURN
      ICALL=1
C
C-- QCD CONSTANTS
      PI=4.*ATAN(1.)
      COLORS=3.
      CA=COLORS
      CF=(COLORS**2-1.)/(2.*COLORS)
      NFLAVT=NFLATR
      BALPH=(11.*COLORS-2.*FLOAT(NFLAVT))/(12.*PI)
C
C-- CUTOFFS
      QSQMAX=QSQMXT
      ALAMBD=ALAMBT
      QZSQ=QZ**2
      DO 1 L=1,NFLAVT
      QZD=QZ
      QZMADD=0.49
      QZFL(L)=MAX(QZD,QZMASS(L)+QZMADD)
      QZFLSQ(L)=QZFL(L)**2
1     QTHRSQ(L)=(QZ+QZFL(L))**2
      LGLU=NFLAVT+1
      LGLUT=LGLU
      QZFL(LGLU)=QZ
      QZFLSQ(LGLU)=QZSQ
      QTHRSQ(LGLU)=4.*QZSQ
C
      CALL TABLES
C
      RETURN
      END
