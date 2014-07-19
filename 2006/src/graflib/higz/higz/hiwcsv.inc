*
* $Id: hiwcsv.inc,v 1.1 2006/08/31 13:55:47 couet Exp $
*
* $Log: hiwcsv.inc,v $
* Revision 1.1  2006/08/31 13:55:47  couet
* - Needed for the SVG driver
*
* Revision 1.1.1.1  1996/02/14 13:10:15  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIWCSV_INC
#define CERNLIB_HIGZ_HIWCSV_INC
*
*
* hiwcps.inc
*
*-- Author : O.Couet
      IXWCSV(U)=NINT(WKMAX(IPSI)*(FX*(U-X1W)+X1V))
      IYWCSV(V)=IYSVG-NINT(WKMAX(IPSI)*(FY*(V-Y1W)+Y1V))

#endif
