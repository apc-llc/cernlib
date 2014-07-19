*
* $Id: q_jbytet.inc,v 1.1.1.1 1996/02/15 17:51:12 mclareni Exp $
*
* $Log: q_jbytet.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:12  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCDC_Q_JBYTET_INC
#define CERNLIB_KERNCDC_Q_JBYTET_INC
*
*
* q_jbytet.inc
*
#if 0
      JBYTET (MZ,IZW,IZP,NZB) = MZ .AND. SHIFT(IZW,61-IZP)
#endif
#if 0
     +                             .AND. .NOT.MASK(60-NZB)
#endif
#if 0
      JBYTOR (MZ,IZW,IZP,NZB) = MZ .OR. (SHIFT(IZW,61-IZP)
#endif
#if 0
     +                             .AND. .NOT.MASK(60-NZB) )
#endif
#if 0
      MBYTET (MZ,IZW,IZP,NZB) = IZW .AND.
#endif
#if 0
     +                          SHIFT (MZ .OR. MASK(60-NZB),IZP-1)
#endif
#if 0
      MBYTOR (MZ,IZW,IZP,NZB) = IZW .OR.
#endif
#if 0
     +                       SHIFT (MZ .AND. .NOT.MASK(60-NZB),IZP-1)
#endif

#endif
