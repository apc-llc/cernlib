*
* $Id: izcfa.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: izcfa.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.06/03 12/12/88  13.58.49  by  O.Couet
*-- Author :
      SUBROUTINE IZCFA(IATTCO,IUD)
*.===========>
*.
*.   Change the Flag Attribute (of the current NT) IATTCO to the value IUD .
*.
*. _Input parameters:
*.
*.  INTEGER IATTCO : Code of the attribute function .
*.  INTEGER IUD : New flag state Up/Down (0/1) .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatnb.inc"
#include "higz/hiflat.inc"
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
*.______________________________________
*
      IF(IATTCO.LT.80)THEN
         INDEX=IATTCO-49
         CALL SBIT(IUD,IQ(LHNT+7),INDEX)
         IRFLAT(INDEX)=IUD
      ELSE
         INDEX=IATTCO-79
         CALL SBIT(IUD,IQ(LHNT+6),INDEX)
         IIFLAT(INDEX)=IUD
      ENDIF
*
      END
#endif
