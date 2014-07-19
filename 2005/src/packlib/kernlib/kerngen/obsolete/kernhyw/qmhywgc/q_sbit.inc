*
* $Id: q_sbit.inc,v 1.1.1.1 1996/02/15 17:52:47 mclareni Exp $
*
* $Log: q_sbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:52:47  mclareni
* Kernlib
*
*
*
* q_sbit.inc
*
      MSBIT0 (IZW,IZP)   = IAND (IZW, NOT(ILS(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = IOR  (IZW,     ILS(1,IZP-1)  )
      MSBIT (MZ,IZW,IZP) = IOR (IAND (IZW, NOT(ILS(1,IZP-1)) )
     +                              , ILS(IAND(MZ,1),IZP-1) )
      MSBYT (MZ,IZW,IZP,NZB) = IOR ( IAND (IZW, NOT(
     +                         ILS (IRL(NOT(0),36-NZB), IZP-1)))
     +                           , IRL (ILS(MZ,36-NZB), 37-IZP-NZB) )
