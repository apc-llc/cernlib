*
* $Id: q_jbit.inc,v 1.1.1.1 1996/02/15 17:54:25 mclareni Exp $
*
* $Log: q_jbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:25  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNDGE_Q_JBIT_INC
#define CERNLIB_KERNDGE_Q_JBIT_INC
*
*
* q_jbit.inc
*
      JBIT (IZW,IZP)     = IBITS (IZW,IZP-1,1)
      JBYT (IZW,IZP,NZB) = IBITS (IZW,IZP-1,NZB)

#endif
