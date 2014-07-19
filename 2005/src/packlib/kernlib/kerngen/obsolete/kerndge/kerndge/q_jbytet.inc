*
* $Id: q_jbytet.inc,v 1.1.1.1 1996/02/15 17:54:25 mclareni Exp $
*
* $Log: q_jbytet.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:25  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNDGE_Q_JBYTET_INC
#define CERNLIB_KERNDGE_Q_JBYTET_INC
*
*
* q_jbytet.inc
*
      JBYTET (MZ,IZW,IZP,NZB) = IAND (MZ, IBITS(IZW,IZP-1,NZB))
      JBYTOR (MZ,IZW,IZP,NZB) = IOR  (MZ, IBITS(IZW,IZP-1,NZB))
      MBYTET (MZ,IZW,IZP,NZB) = IAND (IZW, NOT(ISHFT (
     +                          ISHFT(NOT(MZ),32-NZB),-33+IZP+NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = IOR (IZW, ISHFT (
     +                               ISHFT(MZ,32-NZB),-33+IZP+NZB) )

#endif
