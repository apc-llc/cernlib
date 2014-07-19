*
* $Id: partcl.inc,v 1.1.1.1 1996/01/11 14:14:55 mclareni Exp $
*
* $Log: partcl.inc,v $
* Revision 1.1.1.1  1996/01/11 14:14:55  mclareni
* Cojets
*
*
#ifndef CERNLIB_COJETS_PARTCL_INC
#define CERNLIB_COJETS_PARTCL_INC
*
*
* partcl.inc
*
      PARAMETER (MXPTCL=2000,IPACK=10000)
      COMMON/PARTCL/NPTCL,PPTCL(5,MXPTCL),IORIG(MXPTCL),IDENT(MXPTCL)
     1,IDCAY(MXPTCL)
      REAL PPTCL

#endif
