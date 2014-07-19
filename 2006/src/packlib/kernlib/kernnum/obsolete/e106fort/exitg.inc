*
* $Id: exitg.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: exitg.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_EXITG_INC
#define CERNLIB_KERNNUM_EXITG_INC
*
*
* exitg.inc
*
#if defined(CERNLIB_QMIBMVF)
#if defined(CERNLIB_IBM)
         LM    2,12,28(13)
         BR    14
#endif
#endif

#endif
