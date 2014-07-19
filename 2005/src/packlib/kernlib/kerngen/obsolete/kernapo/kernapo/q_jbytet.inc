*
* $Id: q_jbytet.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_jbytet.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_JBYTET_INC
#define CERNLIB_KERNAPO_Q_JBYTET_INC
*
*
* q_jbytet.inc
*
#if defined(CERNLIB_P5BOOT)
      JBYTET (MZ,IZW,IZP,NZB) = AND (MZ,
     +                        RSHFT (LSHFT(IZW,33-IZP-NZB), 32-NZB) )
      JBYTOR (MZ,IZW,IZP,NZB) = OR  (MZ,
     +                        RSHFT (LSHFT(IZW,33-IZP-NZB), 32-NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = AND (IZW, NOT(RSHFT (
     +                          LSHFT(NOT(MZ),32-NZB), 33-IZP-NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = OR (IZW, RSHFT (
     +                               LSHFT(MZ,32-NZB), 33-IZP-NZB) )
#endif

#endif
