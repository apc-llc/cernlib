*
* $Id: hiwcdc.inc,v 1.3 1998/01/28 14:34:09 couet Exp $
*
* $Log: hiwcdc.inc,v $
* Revision 1.3  1998/01/28 14:34:09  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:57:10  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIWCDC_INC
#define CERNLIB_HIGZ_HIWCDC_INC
*
*   Convert WC to DC for the GL interface
*
* hiwcdc.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if defined(CERNLIB_GL)
      IXWCDC(U)=INT(WKSC(IWKNB)*(RVXMIN+XRATIO*(U-RWXMIN)))
      IYWCDC(V)=INT(WKSC(IWKNB)*(RVYMIN+YRATIO*(V-RWYMIN)))

#endif
#endif
*
*   Convert WC to DC for X11
*
* hiwcdc.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_VAX))
      IXWCDC(U)=MAX(MIN(XWKV1(IWKNB)+
     +XWKR(IWKNB)*(XRATIO*(U-RWXMIN)+RVXMIN-XWKW1(IWKNB))
     +,32767.),-32766.)
      IYWCDC(V)=MAX(MIN(YWKSIZ(IWKNB)-(YWKV1(IWKNB)+
     +YWKR(IWKNB)*(YRATIO*(V-RWYMIN)+RVYMIN-YWKW1(IWKNB)))
     +,32767.),-32766.)

#endif
#endif
*
*   Convert WC to DC for X11
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_VAX))
      IXWCDC(U)=XWKV1(IWKNB)+
     +XWKR(IWKNB)*(XRATIO*(U-RWXMIN)+RVXMIN-XWKW1(IWKNB))
      IYWCDC(V)=YWKSIZ(IWKNB)-(YWKV1(IWKNB)+
     +YWKR(IWKNB)*(YRATIO*(V-RWYMIN)+RVYMIN-YWKW1(IWKNB)))

#endif
#endif
*
*   Convert WC to DC for MacIntosh
*
* hiwcdc.inc
*
#if (defined(CERNLIB_KERNEL))&&(!defined(CERNLIB_MSDOS))
#if defined(CERNLIB_MACMPW)
      IXWCDC(U)=WKSC(IWKNB)*(RVXMIN+XRATIO*(U-RWXMIN))
      IYWCDC(V)=YWKSIZ(IWKNB)-WKSC(IWKNB)*(RVYMIN+YRATIO*(V-RWYMIN))
#endif
#endif
*
*   Convert WC to DC for the PC AT 386/486 interface
*
* hiwcdc.inc
*
#if defined(CERNLIB_MSDOS)
      IXWCDC(U)=NINT(WKSC(IWKNB)*(RVXMIN+XRATIO*(U-RWXMIN)))
      IYWCDC(V)=NINT(YWKSIZ(IWKNB)
     *               -WKSC(IWKNB)*(RVYMIN+YRATIO*(V-RWYMIN)))
#endif

#endif
