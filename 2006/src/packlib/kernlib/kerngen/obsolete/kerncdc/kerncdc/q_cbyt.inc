*
* $Id: q_cbyt.inc,v 1.1.1.1 1996/02/15 17:51:12 mclareni Exp $
*
* $Log: q_cbyt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:12  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCDC_Q_CBYT_INC
#define CERNLIB_KERNCDC_Q_CBYT_INC
*
*
* q_cbyt.inc
*
#if 0
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = SHIFT (
#endif
#if 0
     +            (SHIFT(IZW,61-IZP-NZB) .AND. .NOT.MASK(NZB))
#endif
#if 0
     +       .OR. (SHIFT (MZ,61-IZQ-NZB) .AND. MASK(NZB)), IZP+NZB-1)
#endif

#endif
