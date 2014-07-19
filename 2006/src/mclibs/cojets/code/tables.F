*
* $Id: tables.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: tables.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE TABLES
C     *****************
C-- PREPARES TABLES FOR MASS DEGRADATION OF QUANTA
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/event.inc"
#include "cojets/fint.inc"
#include "cojets/itapes.inc"
#include "cojets/qcd.inc"
#include "cojets/tabpsq.inc"
      EXTERNAL FIQRK, FIGLU
C
      DO 1 L=1,NFLAVT
1     IF(2.*(QZ+QZMASS(L)).LT.ECM) NFLAX=L
      NFLAVT=NFLAX
      NPREC=7
      FACT=1./(2.*PI*BALPH)
      ALLAM2=LOG(ALAMBD**2)
      NTAB=2**NPREC+1
      ALMAX=LOG(QSQMAX)*(1.+1.E-6)
      EPSI=FLOAT(NTAB)**(-2)
      KFLA=0
      OLQTHR=0.
C
      DO 10 L=1,LGLU
      ALMIN(L)=LOG(QTHRSQ(L))
      DSTEP(L)=(ALMAX-ALMIN(L))/FLOAT(NTAB-1)
      IF(ABS(QTHRSQ(L)-OLQTHR).LT.1.E-5.AND.L.NE.LGLU) GO TO 9
      LFLA=L
      KFLA=KFLA+1
      IF(KFLA.GT.MAXTAB) GO TO 500
      ALW=ALMIN(L)-DSTEP(L)
      BUP=ALMIN(L)
      TABPSQ(KFLA,1)=0.
C
      IF(L.EQ.LGLU) GO TO 12
      DO 11 I=2,NTAB
      ALW=ALW+DSTEP(L)
      BUP=BUP+DSTEP(L)
11    TABPSQ(KFLA,I)=FACT*ASIMP(ALW,BUP,EPSI,M,2,FIQRK)
     1               +TABPSQ(KFLA,I-1)
      OLQTHR=QTHRSQ(L)
      GO TO 9
C
12    DO 13 I=2,NTAB
      ALW=ALW+DSTEP(L)
      BUP=BUP+DSTEP(L)
13    TABPSQ(KFLA,I)=FACT*ASIMP(ALW,BUP,EPSI,M,2,FIGLU)
     1               +TABPSQ(KFLA,I-1)
9     CONTINUE
      KODTAB(L)=KFLA
10    CONTINUE
C
      RETURN
C
C-- ABNORMAL EXIT
500   OLQTHR=0.
      KFLA=0
      DO 501 L=1,NFLAVT
      IF(ABS(QTHRSQ(L)-OLQTHR).LT.1.E-5) GO TO 501
      OLQTHR=QTHRSQ(L)
      KFLA=KFLA+1
501   CONTINUE
      KFLA1=KFLA+1
      WRITE(ITLIS,502) MAXTAB,NFLAVT,KFLA,KFLA1
502   FORMAT(1H1,51HDIMENSIONS OF TABPSQ( , ) ARE INSUFFICIENT (TABPSQ(,
     1I1,18H, 129) AT PRESENT)
     2/1X,I2,33H FLAVORS HAVE BEEN REQUESTED WITH
     3,I2,18H DIFFERENT CUTOFFS
     4/1X,'INCREASE MAXTAB TO ',I2)
      STOP
      END
