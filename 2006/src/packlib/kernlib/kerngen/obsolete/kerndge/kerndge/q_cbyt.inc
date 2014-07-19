*
* $Id: q_cbyt.inc,v 1.1.1.1 1996/02/15 17:54:24 mclareni Exp $
*
* $Log: q_cbyt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:24  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNDGE_Q_CBYT_INC
#define CERNLIB_KERNDGE_Q_CBYT_INC
*
*
* q_cbyt.inc
*
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = IOR ( IAND (IZW, NOT(
     +                   ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +        , ISHFT (ISHFT(ISHFT(MZ,-IZQ+1),32-NZB), -33+IZP+NZB) )

#endif
