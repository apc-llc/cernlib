*
* $Id: q_jbit.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_jbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_JBIT_INC
#define CERNLIB_KERNAPO_Q_JBIT_INC
*
*
* q_jbit.inc
*
#if defined(CERNLIB_P5BOOT)
      JBIT (IZW,IZP)    =   AND (RSHFT(IZW,IZP-1), 1)
      JBYT (IZW,IZP,NZB)= RSHFT (LSHFT(IZW,33-IZP-NZB), 32-NZB)
#endif

#endif
