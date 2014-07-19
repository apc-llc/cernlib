*
* $Id: hcntpar.inc,v 1.1.1.1 1996/01/16 17:07:52 mclareni Exp $
*
* $Log: hcntpar.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:52  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCNTPAR_INC
#define CERNLIB_HBOOK_HCNTPAR_INC
*
*
* hcntpar.inc
*
*CMZ :  4.20/00 21/06/93  12.39.17  by  Fons Rademakers
*-- Author :
      INTEGER   ZBITS,  ZNDIM,  ZNOENT, ZNPRIM, ZNRZB,  ZIFCON,
     +          ZIFNAM, ZIFCHA, ZIFINT, ZIFREA, ZNWTIT, ZITIT1,
     +          ZNCHRZ, ZDESC,  ZLNAME, ZNAME,  ZARIND, ZRANGE, ZNADDR,
     +          ZIBLOK, ZNBLOK, ZLCONT, ZIFBIT, ZIBANK, ZIFTMP, ZITMP,
     +          ZID,    ZNTMP,  ZNTMP1, ZLINK
      PARAMETER(ZBITS=1,   ZNDIM=2,   ZNOENT=3,  ZNPRIM=4,  ZLCONT=6,
     +          ZNRZB=5,   ZIFCON=7,  ZIFNAM=4,  ZIFCHA=5,  ZIFINT=6,
     +          ZIFREA=7,  ZNWTIT=8,  ZITIT1=9,  ZNCHRZ=13, ZIFBIT=8,
     +          ZDESC=1,   ZLNAME=2,  ZNAME=3,   ZRANGE=4,  ZNADDR=12,
     +          ZARIND=11, ZIBLOK=8,  ZNBLOK=10, ZIBANK=9,  ZIFTMP=11,
     +          ZID=12,    ZITMP=10,  ZNTMP=6,   ZNTMP1=3,  ZLINK=6)
*

#endif
