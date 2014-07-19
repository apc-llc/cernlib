*
* $Id: q_cbyt.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_cbyt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_CBYT_INC
#define CERNLIB_KERNAPO_Q_CBYT_INC
*
*
* q_cbyt.inc
*
#if defined(CERNLIB_P5BOOT)
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = OR (
     +      AND (IZW, NOT(LSHFT (RSHFT(NOT(INT4(0)),32-NZB), IZP-1)))
     +    , RSHFT (LSHFT(RSHFT(MZ,IZQ-1),32-NZB), 33-IZP-NZB) )
#endif

#endif
