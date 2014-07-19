*
* $Id: getarg.s,v 1.1.1.1 1996/02/15 17:54:44 mclareni Exp $
*
* $Log: getarg.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:44  mclareni
* Kernlib
*
*
GETARG   CSECT
*
* CERN PROGLIB# Z264    GETARG          .VERSION KERNIBX  1.01  900524
* ORIG.  March 90, Roger Howard, Vancouver
*
*    SUBROUTINE GETARG(N,ARG)
*    Return character string ARG: Nth command line argument
*
         USING *,15
         ENTRY _getarg_
_getarg_ STM   2,8,16(13)    Save registers
         L     3,0(,3)       GR3: length of arg2 = ARG
         LR    2,1           GR2: ADDRESS OF ARG2 = ARG
FILL     MVI   0(2),C' '     Fill next byte with blank
         LA    2,1(,2)       Advance to next byte
         BCT   3,FILL        Repeat for length of ARG
         LR    2,13          GR2: caller's stack frame
         L     3,4(,2)       GR3: previous stack frame
AGAIN    L     4,4(,3)       pointer in previous stack frame
         LTR   4,4           Check for end of chain
         BC    8,FOUND
         LR    2,3           Move up chain
         LR    3,4
         BC    15,AGAIN
FOUND    L     5,76(,2)      GR5: main entry value of GR1
         LR    6,0
         L     6,0(,6)       value of arg1 = N
         SLL   6,2           4*N
         L     7,0(6,5)      address of Nth ARG
         LR    8,7           copy starting address
NEXT     CLI   0(7),X'00'    Is byte a hex zero?
         BC    8,RETURN
         LA    7,1(,7)       Address of next byte
         BC    15,NEXT
RETURN   SR    7,8           Byte count minus 1
         BC    12,EXIT       Exit on count less than 1
         BCTR  7,0           Remove trailing hex 0
         EX    7,COPY        Copy string
EXIT     LM    2,8,16(13)
         BCR   15,14
COPY     MVC   0(0,1),0(8)
         END
