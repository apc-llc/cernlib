*
* $Id: iposup.F,v 1.1.1.1 1996/04/01 15:03:18 mclareni Exp $
*
* $Log: iposup.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:18  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      FUNCTION IPOSUP(IBUF,ICODE)
C-- RETURN A BIT STRING RIGHT ADJ., ZERO FILLED
C-- INPUT
C   IBUF       ARRAY TO BE DECODED
C   ICODE      10000*NW+100*IBUP+IBLOW  , WHERE
C              NW=WORD IN IBUF TO BE DECODED,
C              IBUP = UPPER BIT POSITION,
C              IBLOW = LOWER BIT POSITION
      DIMENSION IBUF(2)
      NW=ICODE/10000
      IBUP=(ICODE-10000*NW)/100
      IBLOW=MOD(ICODE,100)
      NBIT=IBUP-IBLOW+1
      IPOSUP=SHIFT(AND(SHIFT(IBUF(NW),59-IBUP),MASK(NBIT)),NBIT)
      RETURN
      END
