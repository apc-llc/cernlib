*
* $Id: debugvf1.inc,v 1.1.1.1 1996/03/06 10:47:03 mclareni Exp $
*
* $Log: debugvf1.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:03  mclareni
* Zebra
*
*
*  VFORT (Put just in front of SUBROUTINE st.)
*
* debugvf1.inc
*
#if defined(CERNLIB_QMIBM)
      DEBUG TRACE,SUBTRACE,INIT
      AT 1001
      TRACE ON
      END DEBUG
#endif
