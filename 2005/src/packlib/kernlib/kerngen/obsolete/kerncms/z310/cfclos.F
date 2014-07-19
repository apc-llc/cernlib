*
* $Id: cfclos.F,v 1.1.1.1 1996/02/15 17:51:57 mclareni Exp $
*
* $Log: cfclos.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:57  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE CFCLOS (LUNDES, MEDIUM)
C
C CERN PROGLIB#    Z310 CFIO            .VERSION KERNCMS  1.07  911119
C ORIG. 19/11/91, Miguel Marquina
C Dummy version of CFIO routines for portability of code
C
      PRINT *,'Z310 CFCLOS: routine not available on VM/CMS.' ,
     +        'Program stopped.'
      STOP
      END
