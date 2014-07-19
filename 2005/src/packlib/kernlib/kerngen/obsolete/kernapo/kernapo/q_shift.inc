*
* $Id: q_shift.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_shift.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_SHIFT_INC
#define CERNLIB_KERNAPO_Q_SHIFT_INC
*
*
* q_shift.inc
*
#if defined(CERNLIB_P5BOOT)
      ISHFTL (IZW,NZB) = LSHFT (IZW, NZB)
      ISHFTR (IZW,NZB) = RSHFT (IZW, NZB)
#endif

#endif
