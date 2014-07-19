*
* $Id: ffchar.inc,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: ffchar.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_FFCHAR_INC
#define CERNLIB_FFREAD_FFCHAR_INC
*
*
* ffchar.inc
*
      COMMON /FFCHAR/ BUFFER
      CHARACTER*(LENMAX) BUFFER
#if defined(CERNLIB_SAVE)
#include "ffread/ffcharsv.inc"
#endif
C
C     FFCHAR is a character string which contains the input data card
C

#endif
