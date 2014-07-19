*
* $Id: fowl.F,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: fowl.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
      SUBROUTINE FOWL
#if defined(CERNLIB_CDC)
      COMMON // NDIMEN,BINS(7005)
      NDIMEN = 7000
#endif
#if !defined(CERNLIB_CDC)
      COMMON // NDIMEN,BINS(25000)
      NDIMEN = 24898
#endif
      CALL FOWLMP
      RETURN
      END
