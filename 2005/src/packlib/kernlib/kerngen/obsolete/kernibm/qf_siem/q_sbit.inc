*
* $Id: q_sbit.inc,v 1.1.1.1 1996/02/15 17:53:03 mclareni Exp $
*
* $Log: q_sbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:53:03  mclareni
* Kernlib
*
*
*
* q_sbit.inc
*
      MSBIT0 (IZW,IZP)   = LAND (IZW, LCOMPL(SHFTL(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = LOR  (IZW,        SHFTL(1,IZP-1)  )
      MSBIT (MZ,IZW,IZP) = LOR (LAND (IZW, LCOMPL(SHFTL(1,IZP-1)) )
     +                                 , SHFTL(LAND(MZ,1),IZP-1) )
