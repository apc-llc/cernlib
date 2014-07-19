*
* $Id: jetset.inc,v 1.1.1.1 1996/01/11 14:14:52 mclareni Exp $
*
* $Log: jetset.inc,v $
* Revision 1.1.1.1  1996/01/11 14:14:52  mclareni
* Cojets
*
*
#ifndef CERNLIB_COJETS_JETSET_INC
#define CERNLIB_COJETS_JETSET_INC
*
*
* jetset.inc
*
      PARAMETER (MXJSET=400,JPACK=10000)
      COMMON/JETSET/NJSET,PJSET(5,MXJSET),JORIG(MXJSET),JTYPE(MXJSET)
     1,JDCAY(MXJSET)
      REAL PJSET

#endif
