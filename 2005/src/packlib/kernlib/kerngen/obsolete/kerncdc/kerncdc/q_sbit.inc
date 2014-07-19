*
* $Id: q_sbit.inc,v 1.1.1.1 1996/02/15 17:51:12 mclareni Exp $
*
* $Log: q_sbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:51:12  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCDC_Q_SBIT_INC
#define CERNLIB_KERNCDC_Q_SBIT_INC
*
*
* q_sbit.inc
*
#if 0
      MSBIT0 (IZW,IZP)   = IZW .AND. .NOT.SHIFT(MASK(1),IZP)
#endif
#if 0
      MSBIT1 (IZW,IZP)   = IZW .OR.       SHIFT(MASK(1),IZP)
#endif
#if 0
      MSBIT (MZ,IZW,IZP) =(IZW .AND. .NOT.SHIFT(MASK(1),IZP))
#endif
#if 0
     +                  .OR. SHIFT(MZ .AND. .NOT.MASK(59),IZP-1)
#endif
#if 0
      MSBYT (MZ,IZW,IZP,NZB) = SHIFT (
#endif
#if 0
     +            (SHIFT(IZW,61-IZP-NZB) .AND. .NOT.MASK(NZB))
#endif
#if 0
     +         .OR.   (SHIFT (MZ,60-NZB) .AND. MASK(NZB)), IZP+NZB-1)
#endif

#endif
