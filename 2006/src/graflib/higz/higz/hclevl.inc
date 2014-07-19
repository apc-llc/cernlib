*
* $Id: hclevl.inc,v 1.1.1.1 1996/02/14 13:10:17 mclareni Exp $
*
* $Log: hclevl.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCLEVL_INC
#define CERNLIB_HIGZ_HCLEVL_INC
*
*
* hclevl.inc
*
      PARAMETER         (LLEV=256)
      COMMON /HCLEVL/   NLEV,FUNLEV(LLEV+1),ICLEV(LLEV+2)

#endif
