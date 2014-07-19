*
* $Id: farnge.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: farnge.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FARNGE_INC
#define CERNLIB_FATMEN_FARNGE_INC
*
*
* farnge.inc
*
*     NFRNGE - range count (e.g. [8:10] = 3)
*     IFRNGE - 8,9,10 (in previous example)
      COMMON/FARNGE/NFRNGE(20),IFRNGE(100,20),
     +              IFELEM(100,20),IFVAL(100,20)

#endif
