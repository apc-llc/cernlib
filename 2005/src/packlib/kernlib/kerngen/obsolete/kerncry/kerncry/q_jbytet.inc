*
* $Id: q_jbytet.inc,v 1.1.1.1 1996/02/15 17:52:34 mclareni Exp $
*
* $Log: q_jbytet.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:34  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_Q_JBYTET_INC
#define CERNLIB_KERNCRY_Q_JBYTET_INC
*
*
* q_jbytet.inc
*
#if defined(CERNLIB_P5BOOT)
      JBYTET (MZ,IZW,IZP,NZB) = MZ .AND. SHIFTR(IZW,IZP-1)
     +                             .AND. MASK(128-NZB)
      JBYTOR (MZ,IZW,IZP,NZB) = MZ .OR. (SHIFTR(IZW,IZP-1)
     +                             .AND. MASK(128-NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = IZW .AND.
     +                             SHIFT (MZ.OR. MASK(64-NZB), IZP-1)
      MBYTOR (MZ,IZW,IZP,NZB) = IZW .OR.
     +                             SHIFT (MZ.AND.MASK(128-NZB),IZP-1)
#endif

#endif
