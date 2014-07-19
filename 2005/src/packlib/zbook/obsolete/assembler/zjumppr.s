*
* $Id: zjumppr.s,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: zjumppr.s,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#if defined(CERNLIB_PR1ME)
*---
*
*     ZJUMP ,   V01.00  14/03/83   WW     -64V MODE-
*
*     CALLING SEQUENCE (FORTRAN):
*
*     CALL ZJUMP(IZ(JZ+19),A1,A2,A3,A4)
*
*     THIS ROUTINE IS CALLED FROM ZERROR (BY ZBOOK SYSTEM)
*
*---
      ENT   ZJUMP,ZJMP1      ENTRY DECLARATION
*
      SEG                    64V-MODE SEGMENTED ADDRESSING
*
ZJUMP EQU   *                ECB CAUSES CONTROL TO BE PASSED HERE ON CALL
      ARGT                   TRANSFER POINTERS TO ARGUMENTS
*
      LDL   AADR,*
      STL   LADR
*
*---PERFORM CALL TO RECOVERY ROUTINE
      PCL   LADR,*
*
*---CREATE THE ARGUMENT LIST
      AP    BADR,*S
      AP    CADR,*S
      AP    DADR,*S
      AP    EADR,*SL
*
*---RETURN FROM THE ROUTINE
      PRTN
*
      DYNM  AADR(3),BADR(3),CADR(3),DADR(3),EADR(3)
*
      LINK
LADR  OCT  0
      OCT  0
*
ZJMP1 ECB   ZJUMP,,AADR,5
*
      END
#endif
