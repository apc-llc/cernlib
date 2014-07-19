*
* $Id: hcline.inc,v 1.1.1.1 1996/02/14 13:10:17 mclareni Exp $
*
* $Log: hcline.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCLINE_INC
#define CERNLIB_HIGZ_HCLINE_INC
*
*
* hcline.inc
*
      PARAMETER         (LLINES=200)
      COMMON /HCLINE/   NLINES,ILLEVL(LLINES),PLINES(3,2,LLINES)

#endif
