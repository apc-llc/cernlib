*
* $Id: charr.F,v 1.1.1.1 1996/02/15 17:47:44 mclareni Exp $
*
* $Log: charr.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:44  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_NEVER)
      CHARACTER*(*) FUNCTION CHARR(R)
*
* CERN PROGLIB# M443    CHARN            .VERSION KERNBIT  1.07  910711
* Author: Miguel Marquina 91/07/11
*
* Build CHARACTER CHARR from floating-point number R
*
* Mods       Date   Comments
*
      CHARACTER*(*) CHARR
      CHARACTER*6   CHFMT
      DATA CHFMT/'(Fn.m)'/
      LN=LEN(CHARR)
      RETURN
      END
#endif
