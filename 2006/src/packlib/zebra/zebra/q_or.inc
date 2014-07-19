*
* $Id: q_or.inc,v 1.2 2006/09/15 09:35:33 mclareni Exp $
*
* $Log: q_or.inc,v $
* Revision 1.2  2006/09/15 09:35:33  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1  1996/04/18 16:15:13  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
*
*
#ifndef CERNLIB_ZEBRA_Q_OR_INC
#define CERNLIB_ZEBRA_Q_OR_INC
*
* q_or.inc
*
#if defined(CERNLIB_APOLLO)
      IOR  (IZV,IZW) =  OR (IZV, IZW)

#elif defined(CERNLIB_CDC)
      IOR  (IZV,IZW) = IZV .OR.  IZW

#elif defined(CERNLIB_MSDOS)&&defined(CERNLIB_QF_F2C)
       IOR(IZV,IZW)  =  OR(IZV,IZW)

#elif defined(CERNLIB_QMIBMSI)
      IOR  (IZV,IZW) =  LOR (IZV, IZW)

#elif defined(CERNLIB_LNX) && !defined(CERNLIB_GFORTRAN)
       IOR(IZV,IZW)  =  OR(IZV,IZW)

#elif defined(CERNLIB_SUN)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
 
#elif 1

#endif

#endif
