*
* $Id: q_sbyt.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_sbyt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_SBYT_INC
#define CERNLIB_KERNAPO_Q_SBYT_INC
*
*
* q_sbyt.inc
*
#if defined(CERNLIB_P5BOOT)
      MSBYT (MZ,IZW,IZP,NZB) = OR (
     +      AND (IZW, NOT(LSHFT (RSHFT(NOT(INT4(0)),32-NZB), IZP-1)))
     +    , RSHFT (LSHFT(MZ,32-NZB), 33-IZP-NZB) )
#endif

#endif
