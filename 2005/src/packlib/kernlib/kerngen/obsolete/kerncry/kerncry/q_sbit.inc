*
* $Id: q_sbit.inc,v 1.1.1.1 1996/02/15 17:52:32 mclareni Exp $
*
* $Log: q_sbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNCRY_Q_SBIT_INC
#define CERNLIB_KERNCRY_Q_SBIT_INC
*
*
* q_sbit.inc
*
#if defined(CERNLIB_P5BOOT)
      MSBIT0 (IZW,IZP)       =  IZW .AND. SHIFT(MASK(65),IZP)
      MSBIT1 (IZW,IZP)       =  IZW .OR.  SHIFT(MASK(1), IZP)
      MSBIT (MZ,IZW,IZP)     = (IZW .AND. SHIFT(MASK(65),IZP))
     +                     .OR. SHIFT(MZ.AND.MASK(127),IZP-1)
#endif

#endif
