*
* $Id: iargc.s,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: iargc.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
IARGC    CSECT
*
* CERN PROGLIB# Z264    IARGC           .VERSION KERNIBX  1.01  900524
* ORIG.  March 90, Roger Howard, Vancouver
*
*    FUNCTION IARGC()  returns number of words on command line
*
         USING *,15
         ENTRY _iargc_
_iargc_  STM   2,4,16(13)   Save registers
         LR    2,13         GR2: caller's stack frame
         L     3,4(,2)      GR3: previous stack frame
AGAIN    L     4,4(,3)      GR4: pointer in previous frame
         LTR   4,4          Check for end of chain
         BC    8,FOUND
         LR    2,3          Move up chain
         LR    3,4
         BC    15,AGAIN
FOUND    L     0,72(,2)     GR0 restored to main entry value
         BCTR  0,0          Reduce count by one
         LM    2,4,16(13)
         BCR   15,14
         END
