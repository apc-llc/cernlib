*
* $Id: isa.F,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: isa.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_CDC)
      LOGICAL FUNCTION ISA(IW)
C              TRUE IF WORD ALPHANUMERIC
      DIMENSION IPOSTT (63)
      DIMENSION IA(10)
C
C-----------------------------------------------------------------------
C     M A C H I N E   D E P E N D E N T   V A R I A B L E S
C     -----------------------------------------------------
C              NCARW IS THE NUMBER OF CHARACTERS PER WORD
C              NCARW=10 CDC 6600, NCARW = 4 IBM 360, NCARW = 6 UNIVAC
      DATA NCARW /10/
C-----------------------------------------------------------------------
C
      DATA IPOSTT/1H ,1H.,1H+,1H-,1H0,1H1,1H2,1H3,1H4,1H5,
     1            1H6,1H7,1H8,1H9,1HA,1HB,1HC,1HD,1HE,1HF,
     2            1HG,1HH,1HI,1HJ,1HK,1HL,1HM,1HN,1HO,1HP,
     3            1HQ,1HR,1HS,1HT,1HU,1HV,1HW,1HX,1HY,1HZ,
     4            1H*,1H/,1H(,1H),1H$,1H[,1H,,1H[,1H-,1H ,
     5            1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H /
C
      CALL UBLOW (IW,IA,NCARW)
C
      DO 120 J1=1,NCARW
      IAJ1=IA(J1)
      DO 110 J2=1,63
      IF (IPOSTT(J2).EQ.IAJ1) GO TO 120
 110  CONTINUE
      ISA=.FALSE.
      GO TO 999
C
 120  CONTINUE
C
      ISA=.TRUE.
 999  RETURN
      END
#endif
