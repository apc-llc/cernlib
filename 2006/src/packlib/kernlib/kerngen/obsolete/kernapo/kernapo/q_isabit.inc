*
* $Id: q_isabit.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_isabit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_ISABIT_INC
#define CERNLIB_KERNAPO_Q_ISABIT_INC
*
*
* q_isabit.inc
*
#if defined(CERNLIB_P5BOOT)
      BTEST(IZW,IZP) = AND (IZW,LSHFT(1,IZP)) .NE. 0
      IBSET(IZW,IZP) =  OR (IZW,LSHFT(1,IZP))
      IBCLR(IZW,IZP) = AND (IZW,NOT(LSHFT(1,IZP)))
      IBITS (IZW,IZP,NZB) = RSHFT (LSHFT(IZW,32-IZP-NZB), 32-NZB)
#endif

#endif
