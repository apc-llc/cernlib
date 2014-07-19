*
* $Id: fmsuwd.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmsuwd.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSUWD
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatuwd.inc"
#include "fatmen/fatinit.inc"
 
      PRINT *,'FMSUWD. range of user words (-1 = no check)'
 
      DO 10 I=1,10
         PRINT *,'FMSUWD. user word # ',I,': ',
     +      IFUSER(1,I), ' to ',IFUSER(2,I)
   10 CONTINUE
 
      END
