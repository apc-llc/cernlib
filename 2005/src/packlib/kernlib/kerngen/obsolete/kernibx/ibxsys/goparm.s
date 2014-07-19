*
* $Id: goparm.s,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: goparm.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
GOPARM   CSECT
*
* CERN PROGLIB# Z262    GOPARM          .VERSION KERNIBX  1.01  900524
* ORIG.  March 90, Roger Howard, Vancouver
*
*    SUBROUTINE GOPARM(LENGTH,PARMS)
*    Return command line parameter as character string PARMS
*
         USING *,15
         ENTRY _goparm_
_goparm_ STM   2,6,16(13)    Save registers
         LR    2,13          GR2: caller's stack frame
         L     3,4(,2)       GR3: previous stack frame
AGAIN    L     4,4(,3)       pointer in previous stack frame
         LTR   4,4           Check for end of chain
         BC    8,FOUND
         LR    2,3           Move up chain
         LR    3,4
         BC    15,AGAIN
FOUND    LM    2,3,72(2)     GR0 and GR1 on entry to main
         SR    4,4           zero character count
         BCT   2,GETPAR      parameter word count - 1
         BC    15,RETURN
GETPAR   L     5,4(,3)       address of 1st word
NEXT     CLI   0(5),X'00'    Is byte a hex zero?
         BC    7,MORE
         CLI   1(5),X'00'    Is there another hex zero?
         BC    8,RETURN
         MVI   0(1),C' '     replace it with blank
         BC    15,ADVANCE
MORE     MVC   0(1,1),0(5)   copy character to arg2
ADVANCE  LA    5,1(,5)       Address of next byte
         LA    1,1(,1)       Next result byte
         LA    4,1(,4)       Increment count
         BC    15,NEXT
RETURN   LR    6,0           Address of arg1 = LENGTH
         ST    4,0(,6)       Return arg1 value
         LM    2,6,16(13)
         BCR   15,14
         END
