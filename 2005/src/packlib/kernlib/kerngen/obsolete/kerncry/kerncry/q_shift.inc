*
* $Id: q_shift.inc,v 1.1.1.1 1996/02/15 17:52:32 mclareni Exp $
*
* $Log: q_shift.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_Q_SHIFT_INC
#define CERNLIB_KERNCRY_Q_SHIFT_INC
*
*
* q_shift.inc
*
#if defined(CERNLIB_P5BOOT)
      ISHFTL (IZW,NZB) = SHIFTL (IZW, NZB)
      ISHFTR (IZW,NZB) = SHIFTR (IZW, NZB)
#endif

#endif
