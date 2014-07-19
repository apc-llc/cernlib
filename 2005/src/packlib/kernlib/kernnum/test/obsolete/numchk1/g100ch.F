*
* $Id: g100ch.F,v 1.1.1.1 1996/02/15 17:48:38 mclareni Exp $
*
* $Log: g100ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:38  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
      SUBROUTINE G100CH(NREP,OK)
      DOUBLE PRECISION DPI,SUM,DX,DEXPO,DROOT
      DIMENSION MARGIN(8),NN(8)
#include "kernnumt/sysdat.inc"
      LOGICAL OK,OKT
#if defined(CERNLIB_NUMHIPRE)
      DATA MARGIN /2*5,4*100,2*5 000 000 000 /
#endif
#if defined(CERNLIB_NUMLOPRE)
      DATA MARGIN /2*5,4*100,2*500 000 /
#endif
      IRESF(RES)=NINT(RES/RELPR)
      OK=.TRUE.
      REL=1.
      DPI=4.*DATAN(1.D0)
C
C
      I=0
      DO 25 J=1,3,2
            I=I+1
            NN(I)=J
   25 CONTINUE
      DO 26 J=2,127,25
            I=I+1
            NN(I)=J
   26 CONTINUE
      DO 90 JREP=1,NREP
            N=NN(JREP)
            IF(JREP .GT. 2) GO TO 30
            X=FLOAT(N)
            PP=PROB(X,N)-2.*(1.-FREQ(SQRT(X)))
            SUM=0.5D0*DBLE(FLOAT(N)-1.)*DSQRT(2.D0*X/DPI)*
     1          DEXP(-0.5D0*X)
            RES=PP-SUM
            RES=ABS(RES)
            GO TO 85
   30       CONTINUE
            X=FLOAT(N)
            DX=DBLE(X)
            DEXPO=DEXP(-0.5D0*DX)
                IF(N .EQ. 2) GO TO 60
            M=N/2
            IF(2.*M .EQ. N) GO TO 50
            DROOT=DSQRT(2.D0*DX/DPI)
            SUM=DEXPO*DROOT
            DO 40 J=2,M
                  DROOT=DROOT*DX/DBLE(2.*FLOAT(J)-1.)
   40             SUM=SUM+DEXPO*DROOT
            PP=PROB(X,N)-2.*(1.-FREQ(SQRT(X)))
            RES=PP-SUM
            RES=ABS(RES)
            GO TO 85
   50       CONTINUE
            MM1=M-1
   60       SUM=DEXPO
            IF(N .EQ. 2) GO TO 80
            DROOT=1.D0
            DO 70 J=1,MM1
                  DROOT=0.5D0*DX*DROOT/DBLE(FLOAT(J))
                  SUM=SUM+DEXPO*DROOT
   70             CONTINUE
   80       PP=PROB(X,N)
            RES=PP-SUM
            RES=ABS(RES)
   85       CONTINUE
            IREL=IRESF(RES)
            OKT=IREL .LE. MARGIN(JREP)
            IF(.NOT.OKT) WRITE(*,600) JREP,REL,IREL
            OK=OK .AND. OKT
   90 CONTINUE
      IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)
      IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)
      IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)
      N     =  0
      CHI2  =  -1.
      PP    =  PROB(CHI2,N)
      N     =  1
      CHI2  =  -1.
      PP    =  PROB(CHI2,N)
      RETURN
  101 FORMAT(/' TWO ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  102 FORMAT(/' TWO ERROR MESSAGES SHOULD NOW FOLLOW ...')
  103 FORMAT(/' TWO ABEND MESSAGES SHOULD NOW FOLLOW ...')
#endif
  600 FORMAT(/ 25H *** ARITHMETIC ERROR ***, I8,1P,E12.3,I12)
      END
