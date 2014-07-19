*
* $Id: rdload.F,v 1.1.1.1 1996/02/15 17:47:46 mclareni Exp $
*
* $Log: rdload.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:46  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE RDLOAD(STRING)
C READ STRING INTO INTERNAL BUFFER OF RDWORD
C********************************************************************
      CHARACTER*(*) STRING
#include "i303cd.inc"
      BUFFER=STRING
      IBPTR=0
      CALL RDNEXT
      RETURN
      END
