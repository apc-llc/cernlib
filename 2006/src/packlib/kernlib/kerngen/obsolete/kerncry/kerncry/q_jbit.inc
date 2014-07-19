*
* $Id: q_jbit.inc,v 1.1.1.1 1996/02/15 17:52:32 mclareni Exp $
*
* $Log: q_jbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_Q_JBIT_INC
#define CERNLIB_KERNCRY_Q_JBIT_INC
*
*
* q_jbit.inc
*
#if defined(CERNLIB_P5BOOT)
      JBIT (IZW,IZP)     = SHIFTR(IZW,IZP-1) .AND. MASK(127)
      JBYT (IZW,IZP,NZB) = SHIFTR(IZW,IZP-1) .AND. MASK(128-NZB)
#endif

#endif
