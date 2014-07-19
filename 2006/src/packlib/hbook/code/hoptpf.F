*
* $Id: hoptpf.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: hoptpf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  4.21/05 29/11/93  15.21.22  by  Fons Rademakers
*-- Author :    Fons Rademakers   29/11/93
      SUBROUTINE HOPTPF
*.==========>
*.           Send HCOPT options to PIAF
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcopt.inc"
#include "hbook/hcpiaf.inc"
*
      IF (CONNPF) THEN
         WRITE(CHSMPF,'(A,10I5)') 'HCOPT:',(ICOPT(I),I=1,10)
         CALL CZPUTA(CHSMPF,ISTAT)
      ENDIF
*
      END
#endif
