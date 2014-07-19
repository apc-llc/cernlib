*
* $Id: mzcwk.inc,v 1.1.1.1 1996/03/06 10:46:53 mclareni Exp $
*
* $Log: mzcwk.inc,v $
* Revision 1.1.1.1  1996/03/06 10:46:53  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_MZCWK_INC
#define CERNLIB_ZEBRA_MZCWK_INC
*
*                MQ Working area
*
* mzcwk.inc
*
      PARAMETER            (NQWKTT=2560)
      COMMON /MZCWK/ IQWKTB(NQWKTT), IQWKFZ(NQWKTT)

#endif
