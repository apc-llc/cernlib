*
* $Id: q_shift.inc,v 1.1.1.1 1996/02/15 17:51:12 mclareni Exp $
*
* $Log: q_shift.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:12  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCDC_Q_SHIFT_INC
#define CERNLIB_KERNCDC_Q_SHIFT_INC
*
*
* q_shift.inc
*
#if 0
      ISHFTL (IZW,NZB) = SHIFT (IZW,NZB) .AND. MASK(60-NZB)
#endif
#if 0
      ISHFTR (IZW,NZB) = SHIFT (IZW.AND.MASK(60-NZB), 60-NZB)
#endif

#endif
