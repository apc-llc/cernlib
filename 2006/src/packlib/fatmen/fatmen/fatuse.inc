*
* $Id: fatuse.inc,v 1.1.1.1 1996/03/07 15:17:58 mclareni Exp $
*
* $Log: fatuse.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:58  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATUSE_INC
#define CERNLIB_FATMEN_FATUSE_INC
*
*
* fatuse.inc
*
#if defined(CERNLIB_TYPE)
#include "fatmen/fttbug.inc"
#endif
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
     +              , LLNLFA, LLNHFA

#endif
