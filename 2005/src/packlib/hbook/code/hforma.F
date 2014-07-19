*
* $Id: hforma.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hforma.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 19/04/93  10.13.24  by  Rene Brun
*-- Author :
      SUBROUTINE HFORMA(I)
*.==========>
*.            THIS ROUTINE PRINTS DIFFERENT FORMATS OF HBOOK
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
      COMMON/HFORM/IA(127),IDU
#include "hbook/hcprin.inc"
#include "hbook/hcrlf.inc"
*.___________________________________________
      IF(I.EQ.1)THEN
         DO 10 K=127,1,-1
            IF(IA(K).NE.IDG(41))GO TO 20
  10     CONTINUE
  20     WRITE(LOUT,1000)(IA(J),J=1,K)
         GO TO 999
      ELSEIF(I.EQ.2)THEN
         WRITE(LOUT,2000)
      ELSEIF(I.EQ.3)THEN
         WRITE(LOUT,3000)CRLF
      ELSEIF(I.EQ.4)THEN
         WRITE(LERR,1000)IA
      ELSE
         WRITE(LERR,2000)
      ENDIF
*
 1000 FORMAT(' ',127A1)
 2000 FORMAT(' ')
 3000 FORMAT(A)
 999  RETURN
      END
