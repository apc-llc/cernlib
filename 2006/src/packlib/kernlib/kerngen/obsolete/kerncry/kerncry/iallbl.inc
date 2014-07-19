*
* $Id: iallbl.inc,v 1.1.1.1 1996/02/15 17:52:32 mclareni Exp $
*
* $Log: iallbl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_IALLBL_INC
#define CERNLIB_KERNCRY_IALLBL_INC
*
*     set a word of all blanks
*
* iallbl.inc
*
#if defined(CERNLIB_P5BOOT)
      PARAMETER   (IALLBL = X'2020202020202020')
#endif

#endif
