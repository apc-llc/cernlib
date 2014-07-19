*
* $Id: ffcomm.inc,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: ffcomm.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_FFCOMM_INC
#define CERNLIB_FFREAD_FFCOMM_INC
*
*
* ffcomm.inc
*
      PARAMETER (NFJUMP=99)
      COMMON /FFCOMM/ FFFLAG, KUJUMP, KFJUMP(NFJUMP)
#if defined(CERNLIB_SAVE)
#include "ffread/ffcommsv.inc"
#endif
C
C     FFFLAG gives the number of errors that occured during reading of
C     the data cards.
C

#endif
