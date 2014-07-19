*
* $Id: sivari.F,v 1.2 1996/04/02 22:17:39 thakulin Exp $
*
* $Log: sivari.F,v $
* Revision 1.2  1996/04/02 22:17:39  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/00 22/07/93  11.38.05  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIVARI
C    ....................................................
C
C
C         PURPOSE
C            PROCESS VARIABLE KLASS 007 IN SIFAM0
C
C         USAGE
C            CALL SIVARI
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            PUT CONSTANT PI WITH POINTER INTO DYNA !!!
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SILSTK
C            SIMSTK
C            GETT2
C            NEXT
C            SEE
C            TRACE
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      DIMENSION DIM(10),NAM(2)
#if !defined(CERNLIB_CRAY)
      CHARACTER INAM*8
#endif
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      CHARACTER*4 CMM(2)
      EQUIVALENCE(INAM,CMM(1))
#else
      EQUIVALENCE(NAM(1),INAM)
#endif
C
      CALL SITRAC (' SIVARI  ')
C
      CALL SINEXT(I)
      DO 33 J=1,ITAM
      INAME(J)=0
  33  CONTINUE
C---  ITYPE =17 (NAME OF SOMETHING,ARRAY,OR SINGLE VARIABLE)
      INAME(1)=17
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      CALL SINEXT(transfer(CMM(1),I))
      CALL SINEXT(transfer(CMM(2),I))
#else
      CALL SINEXT(NAM(1))
      CALL SINEXT(NAM(2))
#endif
C
C---  PUT IN CNAME THE NAME
      WRITE(CNAME,7)INAM
   7  FORMAT(A8)
   8  FORMAT(1X,4A18)
C---  IF NAME IS PI,SET IT AS A CONSTANT
      IF(CNAME.EQ.'PI      ') GO TO 2
      CALL SIGSEE
      IF(KIT.GT.0)GO TO 1
 1    CONTINUE
      CALL SILSTK (INAME,CNAME)
      CALL SIMSTK (0,0)
      RETURN
C
2     CONTINUE
      DIM(1)=1.0
      MODE=1
      CALL SIGTT2(IDX,1,1,DIM)
      IF(IERRNO.NE.0)RETURN
         IADDR=IDX
      DYNA(IADDR)=3.14159265358979
C
 999  END
