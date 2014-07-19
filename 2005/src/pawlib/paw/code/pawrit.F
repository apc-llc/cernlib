*
* $Id: pawrit.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawrit.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/01 16/12/88  17.11.16  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWRIT(LUN,V,N,IERR)
*
*                To write to an unformatted file
*
      DIMENSION V(N)
*________________________________________________________
*
*
      WRITE(UNIT=LUN,ERR=20)V
      IERR=0
      RETURN
  20  IERR=-1
      END

