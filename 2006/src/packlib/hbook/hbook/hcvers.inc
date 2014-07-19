*
* $Id: hcvers.inc,v 1.2 1998/11/10 09:20:08 couet Exp $
*
* $Log: hcvers.inc,v $
* Revision 1.2  1998/11/10 09:20:08  couet
* - common HCVERC added to store the date with the millenium
*
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCVERS_INC
#define CERNLIB_HBOOK_HCVERS_INC
*
*
* hcvers.inc
*
*-- Author : Rene Brun 
      INTEGER       INSTAL(20),IVERS(5),IDATE(8)
      CHARACTER*10  CHDATE
      COMMON/HCVERS/INSTAL,IVERS,IDATE
      COMMON/HCVERC/CHDATE
*
#endif
