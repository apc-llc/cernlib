*
* $Id: q_jbytet.inc,v 1.1.1.1 1996/02/15 17:54:47 mclareni Exp $
*
* $Log: q_jbytet.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:47  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNOR_Q_JBYTET_INC
#define CERNLIB_KERNNOR_Q_JBYTET_INC
*
*
* q_jbytet.inc
*
      JBYTET (MZ,IZW,IZP,NZB) = IAND (MZ,
     +                        ISHFT (ISHFT(IZW,33-IZP-NZB),-32+NZB) )
      JBYTOR (MZ,IZW,IZP,NZB) = IOR  (MZ,
     +                        ISHFT (ISHFT(IZW,33-IZP-NZB),-32+NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = IAND (IZW, NOT(ISHFT (
     +                          ISHFT(NOT(MZ),32-NZB),-33+IZP+NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = IOR (IZW, ISHFT (
     +                               ISHFT(MZ,32-NZB),-33+IZP+NZB) )

#endif
