*
* $Id: machid.inc,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: machid.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
*
* machid.inc
*
#if !defined(CERNLIB_IBM)
#if defined(CERNLIB_VAX)
      DATA IMAC / 4HVAX  /
      ISIT = 1
#endif
#endif
#if !defined(CERNLIB_IBM)
#if (!defined(CERNLIB_NOSBE))&&(defined(CERNLIB_CDC))
      DATA IMAC / 4H7600 /
      ISIT = 2
#endif
#endif
#if !defined(CERNLIB_IBM)
#if (defined(CERNLIB_NOSBE))&&(defined(CERNLIB_CDC))
      DATA IMAC / 4H6400 /
      ISIT = 3
#endif
#endif
#if !defined(CERNLIB_IBM)
#if defined(CERNLIB_APOLLO)
      DATA IMAC / 4HAPO  /
      ISIT = 9
#endif
#endif
#if !defined(CERNLIB_IBM)
#if defined(CERNLIB_CONVEX)
      DATA IMAC / 4HCVX  /
      ISIT=13
#endif
#endif
#if !defined(CERNLIB_IBM)
#if defined(CERNLIB_CRAY)
      DATA IMAC / 4HCRAY /
      ISIT=12
#endif
#endif
