*
* $Id: pawbrk.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: pawbrk.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/16 03/08/94  18.48.54  by  Fons Rademakers
*-- Author :    Rene Brun   12/12/89
      SUBROUTINE PAWBRK
************************************************************************
*                                                                      *
* PAWBRK (vsn 1.00)                                                    *
*                                                                      *
* Reset some variables to their default value after a break.           *
*                                                                      *
************************************************************************
*
*
*-- invalidate the cache of the columns currently in use
*
      CALL HBFREE(-1)
*
*      CALL TRACEQ(0,20)
*
      END

