*
* $Id: fatbug.inc,v 1.1.1.1 1996/03/07 15:17:58 mclareni Exp $
*
* $Log: fatbug.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:58  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATBUG_INC
#define CERNLIB_FATMEN_FATBUG_INC
*
*
* fatbug.inc
*
#if defined(CERNLIB_TYPE)
#include "fatmen/fttbug.inc"
#endif
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
     +              , LLNLFA, LLNHFA

#endif
