*
* $Id: q_andor.inc,v 1.1.1.1 1996/02/15 17:54:11 mclareni Exp $
*
* $Log: q_andor.inc,v $
* Revision 1.1.1.1  1996/02/15 17:54:11  mclareni
* Kernlib
*
*
*
* q_andor.inc
*
      IAND (IZV,IZW) = IZV .AND. IZW
      IOR  (IZV,IZW) = IZV .OR.  IZW
      IEOR (IZV,IZW) = IZV .XOR. IZW
      NOT (IZW)      = .NOT. IZW
