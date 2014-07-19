*
* $Id: noarg.s,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: noarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
NOARG    CSECT
*
* CERN PROGLIB# Z029    NOARG           .VERSION KERNIBX  1.01  900524
* ORIG.  March 90, Roger Howard, Vancouver
*
*      THIS IS A SUBROUTINE TO DETERMINE THE NUMBER OF ARGS
*      CALLING SEQUENCE : "CALL NOARG(N)"
*
*      AIX calling sequence: argument addresses are placed in
*        GR0, GR1, GR2, GR3 and then successive stack locations
*        starting at byte 88. The list is terminated by fullword
*        "-1", or by "-2" if it is of double length because
*        there are character arguments.
*      Roger Howard, UBC, Vancouver. 1990/01/25
*
         USING *,15
         ENTRY _noarg_
_noarg_  STM   2,1,16(13)         save registers
         LR    11,0               argument address
         L     9,4(,13)           get previous save area
         SR    0,0                initialize result counter
         LA    8,1                GR8 = 1
         LM    1,2,72(9)          previous registers 0 and 1
         AR    1,8                is 1st address -1?
         BC    8,MINUS1
         AR    1,8                is 1st address -2?
         BC    8,MINUS2
         AR    0,8                there is 1 argument
         AR    2,8                is 2nd address -1?
         BC    8,MINUS1
         AR    2,8                is 2nd address -2?
         BC    8,MINUS2
         AR    0,8                there are 2 arguments
         LM    2,3,16(9)          previous registers 2 and 3
         AR    2,8                is 3rd address -1?
         BC    8,MINUS1
         AR    2,8                is 3rd address -2?
         BC    8,MINUS2
         AR    0,8                there are 3 arguments
         AR    3,8                is 4th address -1?
         BC    8,MINUS1
         AR    3,8                is 4th address -2?
         BC    8,MINUS2
         AR    0,8                there are 4 arguments
         LA    1,88(,9)           stack location of next parameters
LOOP     L     2,0(,1)            next parameter
         AR    2,8                is it -1?
         BC    8,MINUS1
         AR    2,8                is it -2?
         BC    8,MINUS2
         AR    0,8                increment argument count
         LA    1,4(,1)            address next parameter
         BC    15,LOOP
MINUS2   SRL   0,1                halve count for character arguments
MINUS1   ST    0,0(,11)           return argument to caller
         LM    2,1,16(13)
         BR    14
         END
#ifdef CERNLIB_CCGEN_NOARG
#undef CERNLIB_CCGEN_NOARG
#endif
#ifdef CERNLIB_TCGEN_NOARG
#undef CERNLIB_TCGEN_NOARG
#endif
