*
* $Id: paclos.F,v 1.1.1.1 1996/03/01 11:38:38 mclareni Exp $
*
* $Log: paclos.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:38  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/12 28/01/94  17.22.53  by  Fons Rademakers
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PACLOS(LUN)
*
*                To close unit LUN
*
#if defined(CERNLIB_IBM)
#include "paw/pcchar.inc"
      CHARACTER*3 CHLUN
#endif
*________________________________________________________
*
      CLOSE (LUN)
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      WRITE(CHLUN,'(I3)')LUN
      CHTEMP='FILEDEF '//CHLUN//'  CLEAR'
      CALL VMCMS(CHTEMP,ISTAT)
#endif
*
      END
