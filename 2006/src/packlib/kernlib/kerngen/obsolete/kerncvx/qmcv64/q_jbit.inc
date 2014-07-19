*
* $Id: q_jbit.inc,v 1.1.1.1 1996/02/15 17:52:08 mclareni Exp $
*
* $Log: q_jbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:08  mclareni
* Kernlib
*
*
*
* q_jbit.inc
*
#if defined(CERNLIB_P5BOOT)
      JBIT (IZW,IZP)     = IBITS (IZW,IZP-1,1)
      JBYT (IZW,IZP,NZB) = IBITS (IZW,IZP-1,NZB)
#endif
