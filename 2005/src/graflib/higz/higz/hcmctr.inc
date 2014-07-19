*
* $Id: hcmctr.inc,v 1.2 2002/05/22 09:16:20 couet Exp $
*
* $Log: hcmctr.inc,v $
* Revision 1.2  2002/05/22 09:16:20  couet
* - the dimension of ABCD was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCMCTR_INC
#define CERNLIB_HIGZ_HCMCTR_INC
*
*
* hcmctr.inc
*
      PARAMETER         (NISO = 4, NMAXP = NISO*13, NMAXT = NISO*12)
      REAL              XYZ(3,NMAXP),XYZN(3,NMAXP),GRAD(3,NMAXP)
      REAL              DTRIA(6,NMAXT),ABCD(4,NMAXT)
      INTEGER           ITRIA(3,NMAXT),IATTR(NMAXT),IORDER(NMAXT)
      COMMON /HCMCTR/   XYZ,XYZN,GRAD,DTRIA,ABCD,ITRIA,IATTR,IORDER

#endif
