*
* $Id: zevel.inc,v 1.1.1.1 1996/01/11 14:14:59 mclareni Exp $
*
* $Log: zevel.inc,v $
* Revision 1.1.1.1  1996/01/11 14:14:59  mclareni
* Cojets
*
*
#ifndef CERNLIB_COJETS_ZEVEL_INC
#define CERNLIB_COJETS_ZEVEL_INC
*
*
* zevel.inc
*
      PARAMETER (NTRIES=1)
      PARAMETER (MAXLEN=1024)
      COMMON/ZEVEL/IZEVEL(MAXLEN)
      REAL ZEVEL(MAXLEN)
      EQUIVALENCE(ZEVEL(1),IZEVEL(1))
      EQUIVALENCE (IZVL1,IZEVEL(1))
      EQUIVALENCE (IZVL2,IZEVEL(2))

#endif
