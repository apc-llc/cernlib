*
* $Id: g110m.F,v 1.1.1.1 1996/04/01 15:01:30 mclareni Exp $
*
* $Log: g110m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:30  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE G110M
C     This routine tests the package of subroutines LANDAU (G110)
C     The six functions DENLAN, DISLAN, DIFLAN, XM1LAN, XM2LAN, RANLAN
C     are all tested for various values of the parameter X
C     Set largest relative error permitted for successful test

      DIMENSION X1(15),X3(10),R1E(15),R2E(15),R3E(15),RE(-1:10),
     + X1E(10),X2E(10)
      LOGICAL LTEST
      PARAMETER ( TSTERR=5E-5 )
#include "iorc.inc"
      DATA (X1(I),I=1,15)
     1/-6.,-4.,-2.,-1.5,0.4,1.5,3.,5.,8.,15.,30.,100.,500.,1000.,10000./

      DATA (X3(I),I=1,10)
     1/-6.,-4.,-2.,0.5,4.,10.,20.,60.,100.,500./


       DATA (R1E(I),R2E(I),R3E(I),I=1,15)/
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_QIEEE))
C      These three numbers are too small for the Vax and for QIEEE
     +   0.1705125E-63 , 0.1145071E-65 , 0.2522112E-61,
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_QIEEE)
     +   0E0           , 0E0           ,0E0           ,
#endif
     +   0.3389938E-08 , 0.1648393E-09 , 0.6640147E-07,
     +   0.4398549E-01 , 0.1409436E-01 , 0.9809309E-01,
     +   0.1005507E+00 , 0.4982426E-01 , 0.1172569E+00,
     +   0.1687040E+00 , 0.3565814E+00 ,-0.3329862E-01,
     +   0.1242211E+00 , 0.5183430E+00 ,-0.4082071E-01,
     +   0.7424766E-01 , 0.6642060E+00 ,-0.2538776E-01,
     +   0.3916338E-01 , 0.7730271E+00 ,-0.1152962E-01,
     +   0.1809028E-01 , 0.8534598E+00 ,-0.4055079E-02,
     +   0.5401332E-02 , 0.9232939E+00 ,-0.7258360E-03,
     +   0.1298670E-02 , 0.9632503E+00 ,-0.8989537E-04,
     +   0.1076111E-03 , 0.9895709E+00 ,-0.2208708E-05,
     +   0.4085715E-05 , 0.9979767E+00 ,-0.1648340E-07,
     +   0.1012057E-05 , 0.9989935E+00 ,-0.2034224E-08,
     +   0.1001660E-07 , 0.9998999E+00 ,-0.2004778E-11/

       DATA (RE(I),I=-1,10)/
     +       -0.345390E+01,
     +       -0.256661E+01,
     +       -0.108254E+01,
     +       -0.474905E+00,
     +        0.817656E-01,
     +        0.673583E+00,
     +        0.136656E+01,
     +        0.225775E+01,
     +        0.354898E+01,
     +        0.581236E+01,
     +        0.118083E+02,
     +        0.561444E+03/
c    +        0.986920E+06/

       DATA (X1E(I),X2E(I),I=1,10)/
     +  -0.6006671E+01 , 0.3608014E+02,
     +  -0.4046505E+01 , 0.1637619E+02,
     +  -0.2258286E+01 , 0.5146864E+01,
     +  -0.6293224E+00 , 0.9086789E+00,
     +   0.6111705E+00 , 0.2720909E+01,
     +   0.1608095E+01 , 0.9648612E+01,
     +   0.2394375E+01 , 0.2186150E+02,
     +   0.3592685E+01 , 0.6772610E+02,
     +   0.4129987E+01 , 0.1113155E+03,
     +   0.5778184E+01 , 0.5259707E+03/

      CALL HEADER('G110',0)
      ERMAX=0
      LTEST= .TRUE.
      WRITE(LOUT,101)
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_QIEEE))
      DO 1 I = 1,15
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_QIEEE)
      DO 1 I = 2,15
#endif
      R1=DENLAN(X1(I))
      R2=DISLAN(X1(I))
      R3=DIFLAN(X1(I))
      ER1=ABS((R1-R1E(I))/R1E(I))
      ER2=ABS((R2-R2E(I))/R2E(I))
      ER3=ABS((R3-R3E(I))/R3E(I))
      ERMAX=MAX(ERMAX,ER1,ER2,ER3)
      WRITE(LOUT,'(5X,F10.1,3E16.7)') X1(I),R1,R2,R3
    1 CONTINUE
      LTEST=LTEST .AND. ERMAX .LE. TSTERR

      WRITE(LOUT,102)
      DO 2 I = -1,10
      X=0.1*FLOAT(I)+0.0014
      IF(I .EQ. -1) X=0.000001
      IF(I .EQ. 10) X=0.9982
c     IF(I .EQ. 11) X=0.999999
      R=RANLAN(X)
      ER=ABS((R-RE(I))/RE(I))
      ERMAX=MAX(ERMAX,ER)
      WRITE(LOUT,'(5X,F15.6,E20.6)') X,R
    2 CONTINUE
      LTEST=LTEST .AND. ERMAX .LE. TSTERR

      WRITE(LOUT,103)
      DO 3 I = 1,10
      R1=XM1LAN(X3(I))
      R2=XM2LAN(X3(I))
      ER4=ABS((R1-X1E(I))/X1E(I))
      ER5=ABS((R2-X2E(I))/X2E(I))
      ERMAX=MAX(ERMAX,ER4,ER5)
      WRITE(LOUT,'(5X,F10.1,2E16.7)') X3(I),R1,R2
    3 CONTINUE
      LTEST=LTEST .AND. ERMAX .LE. TSTERR
  101 FORMAT(///9X,'LAMBDA',6X,'DENLAN',10X,'DISLAN',10X,'DIFLAN'/)
  102 FORMAT(///16X,'X',13X,'RANLAN'/)
  103 FORMAT(///12X,'X',8X,'XM1LAN',10X,'XM2LAN'/)
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,E10.1)') ERMAX
      IRC=ITEST('G110',LTEST)
      CALL PAGEND('G110')
      RETURN
      END
