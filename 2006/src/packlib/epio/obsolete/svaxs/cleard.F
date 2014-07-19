*
* $Id: cleard.F,v 1.1.1.1 1996/03/08 15:21:53 mclareni Exp $
*
* $Log: cleard.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:53  mclareni
* Epio
*
*
#if defined(CERNLIB_VAXS)
#include "sys/CERNLIB_machine.h"
#include "_epio/pilot.h"
        SUBROUTINE CLEARD(I,J)
C
C       A.F.R.  2-9-80  VERSION 0
C
C       UTILITY ROUTINE TO CHANGE THE LENGTH OF A DESCRIPTOR
C
        INTEGER*2 I
        I=J
        RETURN
        END
#endif
