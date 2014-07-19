*
* $Id: cdrnge.inc,v 1.1.1.1 1996/02/28 16:24:03 mclareni Exp $
*
* $Log: cdrnge.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:03  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CDRNGE_INC
#define CERNLIB_HEPDB_CDRNGE_INC
*
*
* cdrnge.inc
*
*     NCRNGE - range count (e.g. [8:10] = 3)
*     ICRNGE - 8,9,10 (in previous example)
      COMMON/CDRNGE/NCRNGE(20),ICRNGE(100,20),
     +              ICELEM(100,20),ICVAL(100,20)

#endif
