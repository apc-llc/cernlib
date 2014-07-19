*
* $Id: sicoma.F,v 1.1.1.1 1995/12/12 14:36:16 mclareni Exp $
*
* $Log: sicoma.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  12.10.16  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SICOMA
C
C    ..................................................
C...  PAW VERSION  ...  MAY 1988
C    ..................................................
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CALL SITRAX(' SICOMAN ')
      IPOOLD = IPOINT
      ICMPI=1
      CALL SICMPL
       I=IPOINT
       CALL SIOUTP(777)
       IPOINT=I
       IF(SITRAK(6).EQ.0) GO TO 10
       DO 5 I = IPOOLD,IPOINT
  5    WRITE(NPRINT, 12) I,IPROGR(I),XPROGR(I),IPROGR(I)
  10   CONTINUE
       IPOINT = IPOOLD
 12   FORMAT (' ',I4,5X,I4,F12.6,A4)
 999  END
