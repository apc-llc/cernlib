*
* $Id: epvaxcom.inc,v 1.1.1.1 1996/03/08 15:21:47 mclareni Exp $
*
* $Log: epvaxcom.inc,v $
* Revision 1.1.1.1  1996/03/08 15:21:47  mclareni
* Epio
*
*
#ifndef CERNLIB_EPIO_EPVAXCOM_INC
#define CERNLIB_EPIO_EPVAXCOM_INC
*
*
* epvaxcom.inc
*
#if defined(CERNLIB_VAX)||defined(CERNLIB_STF77VX)
      COMMON/EPVAXN/EPVXUN(10)
      CHARACTER*255 EPVXUN
#endif

#endif
