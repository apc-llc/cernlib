*
* $Id: ztrpass.s,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: ztrpass.s,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#if defined(CERNLIB_PR1ME)
*---
*
*     ZTRAP ,   V01.00  14/03/83   WW     -64V MODE-
*
*     CALLING SEQUENCE (FORTRAN):
*
*     CALL ZTRAP(IZ,RECOVER)
*
*     THIS IS USER ROUTINE , RECOVER HAS TO BE DECLARED EXTERNAL
*
*---
      ENT   ZTRAP,ZTRP1      ENTRY DECLARATION
*
      SEG                    64V-MODE SEGMENTED ADDRESSING
      RLIT                   PLACE LITERALS IN PROCEDURE FRAME
*
ZTRAP EQU   *                ECB CAUSES CONTROL TO BE PASSED HERE ON CALL
      ARGT                   TRANSFER POINTERS TO ARGUMENTS
*
      LDL    AADR,*
      LLS    1
      ADL    AADR
      STL    STACK
      LDL    BADR,*           F77 CONVENTION FOR PASSING EXTERNALS
      LDX    =36
      STL    STACK,*X
*
*---RETURN FROM THE ROUTINE
      PRTN
*
      DYNM  AADR(3),BADR(3),STACK(2)
*
      LINK
*
ZTRP1 ECB   ZTRAP,,AADR,2
*
      END
#endif
