*
* $Id: q_sbyt.inc,v 1.1.1.1 1996/02/15 17:52:32 mclareni Exp $
*
* $Log: q_sbyt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_Q_SBYT_INC
#define CERNLIB_KERNCRY_Q_SBYT_INC
*
*
* q_sbyt.inc
*
#if defined(CERNLIB_P5BOOT)
      MSBYT (MZ,IZW,IZP,NZB) = (IZW .AND. SHIFT(MASK(64-NZB),IZP-1))
     +                       .OR. SHIFT(MZ.AND.MASK(128-NZB),IZP-1)
#endif

#endif
