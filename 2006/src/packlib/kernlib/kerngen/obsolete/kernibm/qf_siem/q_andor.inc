*
* $Id: q_andor.inc,v 1.1.1.1 1996/02/15 17:53:02 mclareni Exp $
*
* $Log: q_andor.inc,v $
* Revision 1.1.1.1  1996/02/15 17:53:02  mclareni
* Kernlib
*
*
*
* q_andor.inc
*
      IAND (IZV,IZW) = LAND (IZV, IZW)
      IOR  (IZV,IZW) =  LOR (IZV, IZW)
      IEOR (IZV,IZW) = LXOR (IZV, IZW)
      NOT (IZW)      = LCOMPL (IZW)
