*
* $Id: silbc7.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: silbc7.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SILBC7
C
C
C
C    ..................................................
C
C
C         PURPOSE
C            TO EXECUTE COMPLEX LIBRARY FUNCTIONS FROM LIBDRIV
C
C         USAGE
C            CALL SILBC7
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C
C         EXTERNAL
C            CGAMMA
C            CDIGAM
C            CLOGAM
C            CWERF
C
C
C         AUTHOR.   JURIS REINFELDS      DATE 26/02/75
C
C...PAW VERSION    MAY 1988
C
C    ..................................................
C
C
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
C
C
C   DUMMY           EXTERNAL CGAMMA,CDIGAM,CLOGAM
C
C     DUMMY CALL TO CWERF
C     EXTERNAL CWERF
C
C
C
      DIMENSION RCA(2),RCX(2)
      EQUIVALENCE (CPLA,RCA(1)),(CPLX,RCX(1))
      EQUIVALENCE (RA,RCA(1)),(CA,RCA(2)),(RX,RCX(1)),(CX,RCX(2))
C
C
      CA=0.
C
C     DO 10 I=IADDR,IZADB,MODE
C     RA=DYNA(IZADA)
C     IF(IZMODA.EQ.2) CA=DYNA(IZADA+1)
C   DUMMY      IF(KLASS.EQ.101) CPLX=CGAMMA(CPLA)
C   DUMMY      IF(KLASS.EQ.102) CPLX=CDIGAM(CPLA)
C   DUMMY      IF(KLASS.EQ.103) CPLX=CLOGAM(CPLA)
C
C
C      DUMMY CALL TO CWERF
C      IF(KLASS.EQ.104) CPLX=CWERF(CPLA)
C
C
C     DYNA(I)=RX
C     DYNA(I+1)=CX
C     IZADA=IZADA+IZMODA
C10   CONTINUE
C
C
 999  END
