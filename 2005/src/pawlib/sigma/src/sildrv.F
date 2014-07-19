*
* $Id: sildrv.F,v 1.2 1998/03/06 09:02:36 couet Exp $
*
* $Log: sildrv.F,v $
* Revision 1.2  1998/03/06 09:02:36  couet
* - ATG is now disabled
*
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/02 16/08/93  16.48.37  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SILDRV
C
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            PROCESS LIBRARY FUNCTIONS FOR REAL ARGUMENTS
C
C         USAGE
C            CALL SILDRV
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            SINERR
C            IGETT2
C            SILSKK
C            SILIF1
C            SILBC7
C            NEXT
C            NGET
C            SISTR2
C            SIUSTK
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
C                           LIBRARY FUNCTIONS
C
C
C       LIBRARY FUNCTIONS FOR REAL ARGUMENTS
C       ------------------------------------
C
C 001    ACOS           COMPILER FUNCTION
C
C 002    ASIN           COMPILER FUNCTION
C
C 003    TAN            COMPILER FUNCTION
C
C 004    SINH           (B200)
C
C 005    COSH           (B200)
C
C 006    ERF            (C300)
C
C 007    ERFC           (C300)
C
C 008    FREQ           (C300)
C
C 009    DILOG          (C304)
C
C 010    GAMMA          (C305)
C
C 011    ELLICK         (C308)
C
C 012    ELLICE         (C308)
C
C 013    BESJ0          (C312)
C
C 014    BESJ1          (C312)
C
C 015    BESY0          (C312)
C
C 016    BESY1          (C312)
C
C 017    BESI0          (C313)
C
C 018    BESI1          (C313)
C
C 019    BESK0          (C313)
C
C 020    BESK1          (C313)
C
C 021    EBESI0         (C313)
C
C 022    EBESI1         (C313)
C
C 023    EBESK0         (C313)
C
C 024    EBESK1         (C313)
C
C 025    SININT         (C336)
C
C 026    COSINT         (C336)
C
C 027    EXPINT         (C337)
C
C 028    DAWSON         (C339)
C
C 029    ALOGAM         (C341)
C
C 030    RNDM           COMPILER FUNCTION
C
C
C
C       LIBRARY FUNCTIONS FOR COMPLEX ARGUMENTS
C       ---------------------------------------
C 101    CGAMMA         (C306)
C
C 102    CDIGAM         (C307)
C
C 103    CLOGAM         (C333)
C
C 104    CWERF          (C335)
C    ....................................................
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/siclin.inc"
C
C
C
      DIMENSION DIM(10)
C
C     CALL SITRAC(' SILDRV  ')
C
C     CHECK THE NUMBER OF ARGUMENTS
C     ONE MISSING ARG IS NOT POSSIBLE AS COMPILER COMPILES
C     LIB-FN() AS HAVING ZERO ARGUMENTS
      CALL SINEXT(N)
      IF(N.NE.1) CALL SINERR(42)
C
C     IF ARGUMENT IS NOT A NUMERICAL QUANTITY
      DIM(1)=1.0
      CALL SINGET(ISI,1,DIM)
      IF(IERRNO.NE.0)RETURN
      IF(ISI.GE.3) GOTO 9108
      IZMODA=MODE
      LA2=IADDR
C
C     UNSTACK THE NAME OF THE LIBRARY FUMCTION
      CALL SIUSTK
C
      IF(KLASS.LT.1.OR.KLASS.GT.45) GOTO 10
C
C     REAL LIB-FN CALLED, GET REAL RESULT AREA
      ISTRI=0
      MODE=1
      LENGTH=LENGTH/IZMODA
       CALL SIGTT2(IADDR,LENGTH,NDIM,DIM)
      IZADB=IADDR+LENGTH-MODE
C
      DO 4 I=IADDR,IZADB
      CALL SILIF1(DYNA(LA2),DYNA(I))
      IF(IERRNO.NE.0)RETURN
      LA2=LA2+IZMODA
 4    CONTINUE
C
      GOTO 20
C
C
 10   CONTINUE
      IF(KLASS.LT.101.OR.KLASS.GT.104) CALL SINERR(6)
C
C     COMPLEX LIB-FN, GET COMPLEX REAULT AREA
      ISTRI=0
      MODE=2
      LENGTH=LENGTH*MODE/IZMODA
       CALL SIGTT2(IADDR,LENGTH,NDIM,DIM)
      IZADB=IADDR+LENGTH-MODE
C
 15   CONTINUE
      CALL SILBC7
C
C
 20   CONTINUE
      CALL SISTR2(1)
      RETURN
C
C
 9108 CONTINUE
      CALL SILSKK(0,INAME,CNAME)
      CALL SINERR(18)
      END
