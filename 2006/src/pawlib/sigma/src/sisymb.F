*
* $Id: sisymb.F,v 1.2 1996/04/02 22:17:38 thakulin Exp $
*
* $Log: sisymb.F,v $
* Revision 1.2  1996/04/02 22:17:38  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  14.40.01  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SISYMB
#include "sigma/sigkq.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/sicph1.inc"
C
      CHARACTER CM*10,CA*1,SINSTR*1
      DIMENSION IM(2),CA(10)

*
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      CHARACTER*4 CMM(2)
      EQUIVALENCE(CM,CMM(1))
#else
      EQUIVALENCE(CM,M),(IM(1),M)
#endif
      LOGICAL SICOMP
      LOGICAL SIWHAT,IND
      CALL SITRAX(' SISYMB    ')
      I=1
      DO 3 J=1,10
      CA(J)=' '
  3   CONTINUE
1     CONTINUE
      IF(SICOMP(KQBLAN)) GO TO 1
      IND=.FALSE.
      IF(.NOT.SIWHAT(1)) RETURN
      IND=.TRUE.
      CA(1) = ISGN
      DO 10 I=2,63
      IC = IC + 1
      ISGN = SINSTR(IC)
      IF(SIWHAT(1) .OR. SIWHAT(0)) GO TO 2
      IF(I.GE.8) GO TO 12
      GO TO 12
    2 CONTINUE
C
C    NO MORE THAN 10 .....
      IF(I.GT.10)PRINT *,' SYMBOL WITH MORE THAN 10 CHARS.'
      IF (I.GT.10) GOTO 12
      CA(I)=SINSTR(IC)
   10 CONTINUE
   13 CONTINUE
      IND=.FALSE.
      RETURN
  12  CONTINUE
      CM=CA(1)//CA(2)//CA(3)//CA(4)//CA(5)//CA(6)//CA(7)//CA(8)
     1//CA(9)//CA(10)
      IF(I .GT. 8) GO TO 13
  15  CONTINUE
C---  OUTPUT 3 FOR NEXT IS NAME (2 * CHAR*4)
      CALL SIOUT (3)
#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
C---  OUTPUT FIRST PART  OF NAMES
      CALL SIOUT (transfer(CMM(1),I))
C---  OUTPUT SECOND PART OF NAMES
      CALL SIOUT (transfer(CMM(2),I))
#else
C---  OUTPUT FIRST PART  OF NAMES
      CALL SIOUT (IM(1))
C---  OUTPUT SECOND PART OF NAMES
      CALL SIOUT (IM(2))
#endif
 999  END
