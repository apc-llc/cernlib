*
* $Id: q_shift.inc,v 1.1.1.1 1996/02/15 17:52:11 mclareni Exp $
*
* $Log: q_shift.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:11  mclareni
* Kernlib
*
*
*
* q_shift.inc
*
#if defined(CERNLIB_P5BOOT)
      ISHFTL (IZW,NZB) = ISHFT (IZW, NZB)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)
#endif
