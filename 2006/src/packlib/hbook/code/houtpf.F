*
* $Id: houtpf.F,v 1.1.1.1 1996/01/16 17:07:44 mclareni Exp $
*
* $Log: houtpf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  4.20/05 22/08/93  11.52.15  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   14/04/93
      SUBROUTINE HOUTPF(IDH)
*
* Send a histogram.
* Set bit 6 in status word to 1. This bit will be reset to 0 by any
* filling routine. This bit can be checked to prevent sending an unchanged
* histogram again.
*
#include "hbook/hcbook.inc"
 
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,IDH)
      LBANK=LQ(LTAB-IDPOS)
      CALL FZOUT(998,IHDIV,LBANK,1,' ',0,0,0)
      CALL SBIT1(IQ(LBANK),6)
 
      END
#endif
