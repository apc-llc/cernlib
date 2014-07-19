*
* $Id: q_isabit.inc,v 1.2 2006/09/15 09:35:16 mclareni Exp $
*
* $Log: q_isabit.inc,v $
* Revision 1.2  2006/09/15 09:35:16  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:49:20  mclareni
* Kernlib
*
*
*
* q_isabit.inc
*
#if (defined(CERNLIB_QMLNX)&&!defined(CERNLIB_GFORTRAN))||(defined(CERNLIB_QMDOS)&&defined(CERNLIB_QF_F2C))
      BTEST(IZW,IZP) = AND (IZW,LSHIFT(1,IZP)) .NE. 0
      IBSET(IZW,IZP) =  OR (IZW,LSHIFT(1,IZP))
      IBCLR(IZW,IZP) = AND (IZW,NOT(LSHIFT(1,IZP)))
      IBITS (IZW,IZP,NZB) = ISHFTR (LSHIFT(IZW,32-IZP-NZB), 32-NZB)
#elsif defined(CERNLIB_GFORTRAN)
      BTEST(IZW,IZP) = IAND (IZW,ISHFT(1,IZP)) .NE. 0
      IBSET(IZW,IZP) =  IOR (IZW,ISHFT(1,IZP))
      IBCLR(IZW,IZP) = IAND (IZW,NOT(ISHFT(1,IZP)))
      IBITS (IZW,IZP,NZB) = ISHFT(ISHFT(IZW,32-NZB-IZP),NZB-32)
#endif
