*
* $Id: pahpr.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahpr.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/04 28/06/95  10.16.43  by  O.Couet
*-- Author :    O.Couet   28/06/95
      SUBROUTINE PAHPR(ID)
*.===========>
*.
*.  PAW histogram printing routine. It takes care of
*.  ranges. Ex:
*.                H/PRINT id(i:j)
*.  Note:
*.  This routine assume the range has been decoded
*.  by HGETID. So it should be called AFTER HGETID.
*.
*..==========>
#include "paw/pcrang.inc"
      Logical Hexist
*.______________________________________
*
      If (Icrang.Ne.0) Then
         Idc = Id
  10     Idc = Idc+100
         If (Hexist(Idc)) Goto 10
         Call Hcopyr(Id,Idc,' ',Ibx1,Ibx2,Iby1,Iby2,' ')
         Call Hprint(Idc)
         Call Hdelet(Idc)
      Else
         Call Hprint(Id)
      Endif
*
      End
