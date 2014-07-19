*
* $Id: q_andor.inc,v 1.1.1.1 1996/02/15 17:50:59 mclareni Exp $
*
* $Log: q_andor.inc,v $
* Revision 1.1.1.1  1996/02/15 17:50:59  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNAPO_Q_ANDOR_INC
#define CERNLIB_KERNAPO_Q_ANDOR_INC
*
*
* q_andor.inc
*
#if defined(CERNLIB_P5BOOT)
      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      IEOR (IZV,IZW) = XOR (IZV, IZW)
*    NOT  is standard on APOLLO
#endif

#endif
