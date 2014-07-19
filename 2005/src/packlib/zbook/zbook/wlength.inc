*
* $Id: wlength.inc,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: wlength.inc,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
*
* wlength.inc
*
#if defined(CERNLIB_DOUBLE)
      DATA NBIT/32/
#endif
#if (defined(CERNLIB_SINGLE))&&(!defined(CERNLIB_CDC))
      DATA NBIT/64/
#endif
#if defined(CERNLIB_CDC)
      DATA NBIT/60/
#endif
#if defined(CERNLIB_UNIVAC)||defined(CERNLIB_PDP10)
      DATA NBIT/36/
#endif
#if defined(CERNLIB_BESM6)
      DATA NBIT/48/
#endif
