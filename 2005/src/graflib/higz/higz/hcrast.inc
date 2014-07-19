*
* $Id: hcrast.inc,v 1.1.1.1 1996/02/14 13:10:16 mclareni Exp $
*
* $Log: hcrast.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:16  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCRAST_INC
#define CERNLIB_HIGZ_HCRAST_INC
*
*
* hcrast.inc
*
      PARAMETER          (NBW=30, L1=NBW/2, L2=(NBW+1)/2)
      PARAMETER          (LMASK=(NBW+1)*L1+(NBW-L1*2)*L2)
      INTEGER            IRAST(4),JMASK(NBW),MASK(LMASK)
      COMMON /HCRAST/    NXRAST,NYRAST,XRAST,DXRAST,YRAST,DYRAST,
     &                   IFRAST,JRAST,IRAST,JMASK,MASK

#endif
