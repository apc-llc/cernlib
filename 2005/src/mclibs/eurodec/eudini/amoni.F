*
* $Id: amoni.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: amoni.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION AMONI(IP)
C.----------------------------------------------------------------------
C.
C.    CALCULATE:
C.    IP = 0: TOPONIUM PROPERTIES (ALWAYS),
C.    IP > 0: TOPONIUM MASSES IN CASE TOP MASS DIFFERENT FROM DEFAULT.
C.    LAST UPDATE: 02/10/89
C.
C.----------------------------------------------------------------------
#include "eurodec/wekpar.inc"
#include "eurodec/convrt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/picons.inc"
#include "eurodec/onipot.inc"
      DIMENSION EPSRIC(3,10),GAMRIC(4,7),RHORIC(4,3),DELRIC(4,10),
     &EPSQCD(3,10),GAMQCD(4,7),RHOQCD(4,3),DELQCD(4,10),EBIND(10)
      SAVE EBIND
      DATA EPSRIC/ -1.584, 0.908, 0.148, -0.628, 0.492, 0.188,
     &             -1.584, 0.908, 0.148, -0.628, 0.492, 0.188,
     &             -0.263, 0.403, 0.165, -0.037, 0.352, 0.052,
     &              0.128, 0.379, 0.199, -0.725, 0.501, 0.164,
     &             -0.324, 0.400, 0.152, -0.085, 0.373, 0.172/
      DATA GAMRIC/  7.713,  0.225, -0.118,  0.014,
     &              2.025, -0.017, -0.177,  0.029,
     &              7.713,  0.225, -0.118,  0.014,
     &              2.025, -0.017, -0.177,  0.029,
     &              1.091,  0.062,  0.066,  0.134,
     &              0.763,  0.249,  0.376,  0.357,
     &              0.604, -0.059,  0.127,  0.169/
      DATA RHORIC/ 23.960, -0.939, -0.488,  0.036,
     &             16.570, -1.359, -0.716,  0.036,
     &             12.950, -1.020, -0.240,  0.079/
      DATA DELRIC/  0.000,  0.000,  0.000,  0.000,
     &              0.000,  0.000,  0.000,  0.000,
     &              0.000,  0.000,  0.000,  0.000,
     &              1.018, -0.189,  0.836,  0.806,
     &              0.940, -1.093,  0.092, -0.096,
     &              1.586, -0.746,  0.233,  1.875,
     &              2.280, -7.428, -9.077, 24.650,
     &             50.850, -0.793, -0.638,  0.006,
     &             26.580,  0.511,  0.878,  1.207,
     &             18.660, -4.131, -3.904,  5.374/
      DATA EPSQCD/ -1.946, 0.589, 0.160, -1.278, 0.390, 0.164,
     &             -1.946, 0.589, 0.160, -1.278, 0.390, 0.164,
     &             -0.968, 0.373, 0.190, -0.752, 0.378, 0.200,
     &             -0.582, 0.386, 0.208, -1.367, 0.372, 0.157,
     &             -1.039, 0.363, 0.230, -0.806, 0.363, 0.191/
      DATA GAMQCD/  4.066,  0.060, -0.233,  0.035,
     &              1.311, -0.063, -0.028,  0.146,
     &              4.066,  0.060, -0.233,  0.035,
     &              1.311, -0.063, -0.028,  0.146,
     &              0.831, -0.123, -0.038,  0.108,
     &              0.630, -0.170,  0.026,  0.099,
     &              0.514, -0.241, -0.020,  0.034/
      DATA RHOQCD/ 10.040, -0.994, -0.187,  0.240,
     &              8.790, -1.390, -0.396,  0.109,
     &              7.910, -1.171, -0.082,  0.154/
      DATA DELQCD/  0.000,  0.000,  0.000,  0.000,
     &              0.000,  0.000,  0.000,  0.000,
     &              0.000,  0.000,  0.000,  0.000,
     &              0.958, -1.330,  0.307,  0.283,
     &              1.329, -0.321,  1.267,  1.419,
     &              1.666, -4.486, -3.239,  3.681,
     &              1.539, -2.814, -1.502,  0.764,
     &             24.500, -0.119,  0.306,  0.440,
     &             15.530, -0.507,  0.096,  0.550,
     &             11.910, -0.637, -0.048,  0.048/
C--
C--   STRONG COUPLING CONSTANT FOR BOUND STATES, LAMBDA = 0.2 GEV
      ONIALP(X)=6.*PI/23./LOG(X/.2)
C--
C--   INITIALIZE TOPONIUM BINDING ENERGIES FOR MODIFIED TOP QUARK MASS
C--   AND GAM0 USING THE RICHARDSON OR QCD POTENTIAL ANSATZ
      IF (IP.EQ.0) THEN
        AMT=PM(ICONV(6))
        DELT=1.-AMT/45.
        DO 10 I=1,10
          IF (IONIPT.EQ.0) THEN
            EBIND(I)=EPSRIC(1,I)+DELT*(EPSRIC(2,I)+DELT*EPSRIC(3,I))
            IF (I.GT.7) THEN
              ONIRP0(I,3)=1.E-6*RHORIC(1,I-7)*(AMT/45.)**RHORIC(2,I-7)*
     &        (1.+DELT*(RHORIC(3,I-7)+DELT*RHORIC(4,I-7)))*(2.*AMT+
     &        EBIND(I))**4
            ELSE
              IF (I.LE.2) EBIND(I)=EBIND(I)-1.E-3*40.*SQRT(AMT/45.)
              ONIG0(I,3)=1.E-6*GAMRIC(1,I)*(AMT/45.)**GAMRIC(2,I)
     &        *(1.+DELT*(GAMRIC(3,I)+DELT*GAMRIC(4,I)))
              ONIR0(I,3)=9.*(2.*AMT+EBIND(I))**2*ONIG0(I,3)/16./ALPHA**2
     &        /(1.-16.*ONIALP(2.*AMT+EBIND(I))/3./PI)
            ENDIF
            GAMDIP(I)=1.E-6*DELRIC(1,I)*(AMT/45.)**DELRIC(2,I)
     &      *(1.+DELT*(DELRIC(3,I)+DELT*DELRIC(4,I)))
          ELSE
            EBIND(I)=EPSQCD(1,I)+DELT*(EPSQCD(2,I)+DELT*EPSQCD(3,I))
            IF (I.GT.7) THEN
              ONIRP0(I,3)=1.E-6*RHOQCD(1,I-7)*(AMT/45.)**RHOQCD(2,I-7)*
     &        (1.+DELT*(RHOQCD(3,I-7)+DELT*RHOQCD(4,I-7)))*(2.*AMT+
     &        EBIND(I))**4
            ELSE
              IF (I.LE.2) EBIND(I)=EBIND(I)-1.E-3*40.*SQRT(AMT/45.)
              ONIG0(I,3)=1.E-6*GAMQCD(1,I)*(AMT/45.)**GAMQCD(2,I)
     &        *(1.+DELT*(GAMQCD(3,I)+DELT*GAMQCD(4,I)))
              ONIR0(I,3)=9.*(2.*AMT+EBIND(I))**2*ONIG0(I,3)/16./ALPHA**2
     &        /(1.-16.*ONIALP(2.*AMT+EBIND(I))/3./PI)
            ENDIF
            GAMDIP(I)=1.E-6*DELQCD(1,I)*(AMT/45.)**DELQCD(2,I)
     &      *(1.+DELT*(DELQCD(3,I)+DELT*DELQCD(4,I)))
          ENDIF
   10   CONTINUE
        AMONI=0.
      ELSE
        IDONI=ABS(IPC(IP))-659
        IF (IDONI.GE.9) IDONI=8
        AMONI=2.*PM(ICONV(6))+EBIND(IDONI)
      ENDIF
      RETURN
      END
