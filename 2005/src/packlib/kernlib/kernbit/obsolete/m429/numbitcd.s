*
* $Id: numbitcd.s,v 1.1.1.1 1996/02/15 17:47:42 mclareni Exp $
*
* $Log: numbitcd.s,v $
* Revision 1.1.1.1  1996/02/15 17:47:42  mclareni
* Kernlib
*
*
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
          IDENT NUMBIT
          ENTRY NUMBIT
*         FUNCTION NUMBIT(I)= SUM OF ONES BITS IN I
          VFD         36/6HNUMBIT,24/NUMBIT
 NUMBIT   DATA        0
          SA1         X1
          CX6         X1
          JP          NUMBIT
          END
#endif
