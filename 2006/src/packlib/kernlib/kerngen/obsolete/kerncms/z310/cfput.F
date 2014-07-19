*
* $Id: cfput.F,v 1.1.1.1 1996/02/15 17:51:58 mclareni Exp $
*
* $Log: cfput.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:58  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CFPUT  (LUNDES, MEDIUM, NWREC, MBUF, ISTAT)
C
C CERN PROGLIB#    Z310 CFIO            .VERSION KERNCMS  1.07  911119
C ORIG. 19/11/91, Miguel Marquina
C Dummy version of CFIO routines for portability of code
C
      PRINT *,'Z310 CFPUT : routine not available on VM/CMS.' ,
     +        'Program stopped.'
      STOP
      END
