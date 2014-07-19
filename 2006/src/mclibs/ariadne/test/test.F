*
* $Id: test.F,v 1.2 2006/09/15 09:34:56 mclareni Exp $
*
* $Log: test.F,v $
* Revision 1.2  2006/09/15 09:34:56  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/03/28 16:15:44  mclareni
* Ariadne 4.08
*
* Revision 1.1.1.1  1996/03/08 16:51:07  mclareni
* Ariadne
*
*
 
C...ARiadne TEST by calling artest with argument IPRINT= 0 or 1
 
C...Performs various tests on Ariadne
 
      EXTERNAL LUDATA,PYDATA
#if defined(CERNLIB_GFORTRAN)
      call dummysub(ludata)
      call dummysub(pydata)
      call dummysub(ardata)
#endif
      IPRINT=0

      CALL ARTEST(IPRINT)
      END
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif
