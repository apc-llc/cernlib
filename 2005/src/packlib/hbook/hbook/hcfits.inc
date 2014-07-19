*
* $Id: hcfits.inc,v 1.1.1.1 1996/01/16 17:07:52 mclareni Exp $
*
* $Log: hcfits.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:52  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCFITS_INC
#define CERNLIB_HBOOK_HCFITS_INC
*
*
* hcfits.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
      INTEGER       NCFITS,NPFITS,NFPAR
      REAL                              FITCHI,FITPAR    ,FITSIG
     +             ,FITDER
      COMMON/HCFITS/NCFITS,NPFITS,NFPAR,FITCHI,FITPAR(35),FITSIG(35)
     +             ,FITDER(35)
      CHARACTER*8 FITNAM
      COMMON/HCFITN/FITNAM(35)
*

#endif
