*
* $Id: q_xor.inc,v 1.2 2006/09/15 09:35:34 mclareni Exp $
*
* $Log: q_xor.inc,v $
* Revision 1.2  2006/09/15 09:35:34  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1  1996/04/18 16:15:26  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
*
*
#ifndef CERNLIB_ZEBRA_Q_XOR_INC
#define CERNLIB_ZEBRA_Q_XOR_INC
*
* q_xor.inc
*
#if defined(CERNLIB_APOLLO)
      IEOR (IZV,IZW) = XOR (IZV, IZW)

#elif defined(CERNLIB_CDC)
      IEOR (IZV,IZW) = IZV .XOR. IZW

#elif defined(CERNLIB_MSDOS)&&defined(CERNLIB_QF_F2C)
      IEOR(IZV,IZW)  = XOR(IZV,IZW)

#elif defined(CERNLIB_QMIBMSI)
      IEOR (IZV,IZW) = LXOR (IZV, IZW)

#elif defined(CERNLIB_LNX) && !defined(CERNLIB_GFORTRAN)
      IEOR(IZV,IZW)  = XOR(IZV,IZW)

#elif defined(CERNLIB_SUN)
      IEOR (IZV,IZW) = XOR (IZV, IZW)
 
#elif 1

#endif

#endif
