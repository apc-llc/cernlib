*
* $Id: v700m.F,v 1.1.1.1 1996/04/01 15:01:31 mclareni Exp $
*
* $Log: v700m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:31  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE V700M
C     This program tests the operation of MATHLIB subprograms
C     RVNSPC and DVNSPC

#include "gen/imp64.inc"
      REAL RVNSPC,RV,VTR,RA0,RA1,RX0,REPS,RFV700,RRS2,RRC2,RB
      REAL            ERR,GAUSS,ERRMAXS
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERRD=5D-10 )
      PARAMETER ( TSTERRS=5D-4 )


      EXTERNAL FV700,RFV700,GAUSS,DGAUSS
      COMMON /FORINT2/ RS2,RC2,B
      COMMON /RORINT2/ RRS2,RRC2,RB

      PARAMETER (Z0 = 0, Z1 = 1, HF = Z1/2)
#include "iorc.inc"

      CALL HEADER('V700',0)
      ERRMAXD=0
      ERRMAXS=0

#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/7X,''Rsph.'',5X,''Rcyl.'',11X,''DVNSPC'',12X,
     +''RVNSPC'',4X,''ErrorD'',4x,''ErrorR'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/7X,''Rsph.'',5X,''Rcyl.'',11X,''RVNSPC'',4X,
     +''ErrorR'')')
#endif

      EPS=1D-14
      REPS=1D-6
      DO 1 IRS = 1,5
      WRITE(LOUT,'(1X)')
      RS=IRS
      RS2=RS**2
#if defined(CERNLIB_DOUBLE)
      RRS2=SNGL(RS2)
#endif
      DO 1 IIRC = 1,5
      WRITE(LOUT,'(1X)')
      RC=HF*IIRC
      RC2=RC**2
#if defined(CERNLIB_DOUBLE)
      RRC2=SNGL(RC2)
#endif
      DO 1 IB = 0,5
      B=0.25*IB
#if defined(CERNLIB_DOUBLE)
      RB=SNGL(B)
#endif
      X0=0
      IF(B .GT. 0) X0=(RS2-RC2+B**2)/(2*B)
      A0=MAX(B-RC,-RS)
      A1=MIN(B+RC,RS)
      IF(A0 .GE. A1) THEN
       V=0
      RV=0
      ELSEIF(A0 .LT. X0 .AND. X0. LT. A1) THEN
#if !defined(CERNLIB_DOUBLE)
      RV=2*(GAUSS(FV700,A0,X0,EPS)+GAUSS(FV700,X0,A1,EPS))
#endif
#if defined(CERNLIB_DOUBLE)
       V=2*(DGAUSS(FV700,A0,X0,EPS)+DGAUSS(FV700,X0,A1,EPS))
          RA0=SNGL(A0)
          RA1=SNGL(A1)
          RX0=SNGL(X0)
      RV=2*(GAUSS(RFV700,RA0,RX0,REPS)+GAUSS(RFV700,RX0,RA1,REPS))
#endif
      ELSE
#if !defined(CERNLIB_DOUBLE)
       RV=2*GAUSS(FV700,A0,A1,EPS)
#endif
#if defined(CERNLIB_DOUBLE)
       V=2*DGAUSS(FV700,A0,A1,EPS)
          RA0=SNGL(A0)
          RA1=SNGL(A1)
       RV=2*GAUSS(RFV700,RA0,RA1,REPS)
#endif
      ENDIF
#if defined(CERNLIB_DOUBLE)
      VTD=DVNSPC(RS,RC,B)
      VTR=RVNSPC(SNGL(RS),SNGL(RC),SNGL(B))
#endif
#if !defined(CERNLIB_DOUBLE)
      VTR=RVNSPC(RS,RC,B)
#endif
      ERD=0D0
      ERR=0E0
#if defined(CERNLIB_DOUBLE)
      IF(VTD .NE. 0) ERD= ABS((V-VTD)/VTD)
      ERRMAXD= ABS(MAX(ERRMAXD,ERD))
#endif
      IF(VTR .NE. 0) ERR=ABS((RV-VTR)/VTR)
      ERRMAXS= ABS(MAX(ERRMAXS,ERR))
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,2F10.2,F25.16,F12.6,1P,2D10.1)')
     1          RS,RC,VTD,VTR,ERD,ERR
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(1X,2F10.2,F25.16,1P,E10.1)')
     1          RS,RC,VTR,ERR
#endif
    1 CONTINUE

#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/'' Largest Rel. Error for DVNSPC was'',1P,D10.1)')
     +  ERRMAXD
#endif
      WRITE(LOUT,'(/'' Largest Rel. Error for RVNSPC was'',1P,D10.1)')
     +  ERRMAXS
C     Check if the test was successful
#if !defined(CERNLIB_DOUBLE)
      IRC=ITEST('V700',(ERRMAXS.LE.TSTERRD))
#endif
#if defined(CERNLIB_DOUBLE)
      IRC=ITEST('V700',(ERRMAXD.LE.TSTERRD).AND.(ERRMAXS.LE.TSTERRS))
#endif
      CALL PAGEND('V700')
      RETURN
      END

      FUNCTION FV700(X)
#include "gen/imp64.inc"
      COMMON /FORINT2/ RS2,RC2,B
      H0=RS2-X**2
      H1=MIN(RC2-(X-B)**2,H0)
      FV700=SQRT(H1*(H0-H1))+H0*ASIN(SQRT(H1/H0))
      RETURN
      END
      FUNCTION RFV700(RX)
      COMMON /RORINT2/ RRS2,RRC2,RB
      RH0=RRS2-RX**2
      RH1=MIN(RRC2-(RX-RB)**2,RH0)
      RFV700=SQRT(RH1*(RH0-RH1))+RH0*ASIN(SQRT(RH1/RH0))
      RETURN
      END
