*
* $Id: ectrad.F,v 1.1.1.1 1996/04/01 15:03:14 mclareni Exp $
*
* $Log: ectrad.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:14  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE ECTRAD(COEF,N,ECOEF,NTE,A,B,ACC,IOP)
C
C     CERN PROGRAM LIBRARY NO. E401
C
C     BASIC VERSION OF AUGUST 1970.
C
C     PURPOSE = TO PROVIDE THE FOLLOWING THREE FACILITIES DEPENDING ON
C               THE VALUE OF THE PARAMETER I AS DESCRIBED BELOW.
C
      DOUBLE PRECISION COEF(N) , ECOEF(N) , A , B , ACC
C
      DOUBLE PRECISION CZ , DUMMY , D
C
      COMMON /COFD/ CZ(100) , DUMMY(100) , D(100)
C
 10   FORMAT(//5X,'ECTRAD OPTION PARAMETER IOP OUTSIDE RANGE'//)
 20   FORMAT(//5X,'ECTRAD COMMON BLOCK /COFD/ TOO SMALL'//)
C
      IF (N.LE.100) GO TO 1000
C
C     COMMON BLOCK /COFD/ TOO SMALL.PRINT ERROR MESSAGE.
C
      WRITE(6,20)
      STOP
C
C     COMPUTE OPTION NUMBERS I AND IOPT
C
 1000 IF (IOP.LT.1) GO TO 1020
      IF (IOP.LT.10) IOP=10*IOP
      DO 1010 II=1,3
      I   =II
      IOPT=IOP-10*I
      IF (IOPT.LT.3) GO TO 1030
 1010 CONTINUE
C
C     OPTION NUMBER IOP OUTSIDE RANGE.PRINT ERROR MESSAGE.
C
 1020 WRITE(6,10)
      STOP
C
 1030 IF (I.EQ.1) IOPT=0
      IF (IOPT.EQ.2) IOPT=-1
C
C     ******************************************************************
C     *                                                                *
C
C     I       = 1 , REARRANGEMENT OF A CHEBYSHEV SERIE TO A TAYLOR SERIE
C               VALID IN THE SAME RANGE.
C
C     PARAMETERS
C
C     COEF    = INPUTTED ARRAY OF CHEBYSHEV COEFFICIENTS.
C     N       = INPUTTED NUMBER OF TERMS IN THE CHEBYSHEV SERIE.
C     ECOEF   = OUTPUTTED ARRAY OF THE TAYLOR COEFFICIENTS.
C
C     COMMON BLOCK /COFD/
C
C     CZ      = USED FOR INTERMEDIATE STORAGE.
C
      IF (I.NE.1) GO TO 1040
      CALL TRANSD(COEF,ECOEF,N,CZ)
      NTE=N
      RETURN
C
C     *                                                                *
C     ******************************************************************
C     *                                                                *
C
C     I       = 2 , REARRANGEMENT OF A TAYLOR SERIE IN X VALID IN THE
C               RANGE (A,B) TO A CHEBYSHEV SERIE IN Z VALID IN THE RANGE
C               (-1,1) WHERE Z=(2*X-B-A)/(B-A).
C
C     PARAMETERS
C
C     COEF    = INPUTTED ARRAY OF THE TAYLOR COEFFICIENTS.
C     N       = INPUTTED NUMBER OF TE940 95 THE TAYLOR SERIE.
C     A       = INPUTTED LOWER BOUNDARY.
C     B       = INPUTTED UPPER BOUNDARY.
C     ECOEF   = OUTPUTTED ARRAY OF THE CHEBYSHEV COEFFICIENTS
C
C     COMMON BLOCK /COFD/
C
C     CZ      = OUTPUTTED ARRAY OF THE TAYLOR COEFFICIENTS FOR THE SERIE
C               TRANSFORMED TO THE VARIABLE Z.
C
C     IOPT    = OPTION PARAMETER.
C               IOPT=-1 , THE ODD CASE.
C               IOPT= 0 , THE GENERAL CASE.
C               IOPT= 1 , THE EVEN CASE.
C
 1040 IF (I.NE.2) GO TO 1050
      CALL TAYCHD(A,B,COEF,ECOEF,N,IOPT,CZ)
      NTE=N
      RETURN
C
C     *                                                                *
C     ******************************************************************
C     *                                                                *
C
C     I       = 3 , ECONOMIZATION OF A POWER SERIE IN X VALID IN THE
C               RANGE (A,B) TO A SERIE OF LOWER DEGREE TO A GIVEN MAX-
C               IMUM ABSOLUTE ERROR WITHIN THE RANGE .IN THIS CASE THE
C               OUTPUT COEFFICIENTS ARE IN TERMS OF Z=(2*X-B-A)/(B-A) SO
C               THE OUTPUTTED SERIE IS VALID IN THE RANGE (-1,1).
C
C     PARAMETERS
C
C     COEF    = INPUTTED ARRAY OF THE TAYLOR COEFFICIENTS.
C     N       = INPUTTED NUMBER OF TERMS IN THE ORIGINAL TAYLOR SERIE.
C     A       = INPUTTED LOWER BOUNDARY.
C     B       = INPUTTED UPPER BOUNDARY.
C     ACC     = INPUTTED REQUIRED MAXIMUM COMMITTED ABSOLUTE ERROR IN
C               ECONOMIZATION WITHIN (A,B).
C     ECOEF   = OUTPUTTED ARRAY OF ECONOMIZED TAYLOR COEFFICIENTS.
C     NTE     = OUTPUTTED NUMBER OF TERMS IN ECONOMIZED TAYLOR SERIE.
C
C     COMMON BLOCK /COFD/
C
C     CZ      = OUTPUTTED ARRAY OF TAYLOR COEFFICIENTS IN Z AS DEFINED
C               ABOVE,BEFORE ECONOMIZATION.
C     DUMMY   = USED FOR INTERMEDIATE STORAGE.
C     D       = USED FOR INTERMEDIATE STORAGE.
C
C     IOPT    = OPTION PARAMETER.
C               IOPT=-1 , THE ODD CASE.
C               IOPT= 0 , THE GENERAL CASE.
C               IOPT= 1 , THE EVEN CASE.
C
 1050 CALL ECONDE(COEF,ECOEF,N,NTE,ACC,A,B,IOPT,DUMMY,CZ,D)
      RETURN
C
C     *                                                                *
C     ******************************************************************
C
      END
