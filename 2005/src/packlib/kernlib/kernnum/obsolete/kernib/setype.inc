*
* $Id: setype.inc,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: setype.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_SETYPE_INC
#define CERNLIB_KERNNUM_SETYPE_INC
*
*
* setype.inc
*
           MACRO
           SETYPE
#include "kernnum/global.inc"
           AIF       (&TYPE EQ &RTYPE).RSET
           AIF       (&TYPE EQ &DTYPE).DSET
           AIF       (&TYPE EQ &CTYPE).CSET
           AIF       (&TYPE EQ &CCTYPE).CSET
           AIF       (&TYPE EQ &CTYPEC).CSET
           MNOTE     12,'ILLEGAL ARRAY TYPE'
           MEXIT
 .RSET     ANOP
 &T        SETC      'R'
 &W        SETC      'E'                 L&W GIVES OPERATION CODE LE
 &LENGTH   SETA      4                   WORD LENGTH OF VARIABLE
 &LOGLEN   SETA      2                   SHIFT COUNT FOR &LENGTH
           MEXIT
 .DSET     ANOP
 &T        SETC      'D'
 &W        SETC      'D'                 L&W GIVES OPERATION CODE LD
 &LENGTH   SETA      8                   WORD LENGTH OF VARIABLE
 &LOGLEN   SETA      3                   SHIFT COUNT FOR &LENGTH
           MEXIT
 .CSET     ANOP
 &T        SETC      'C'
 &W        SETC      'E'                 L&W GIVES OPERATION CODE LE
 &LENGTH   SETA      8                   WORD LENGTH OF VARIABLE
 &LOGLEN   SETA      3                   SHIFT COUNT FOR &LENGTH
 &D        SETA      4                   DISPLACEMENT FOR IMAGINARY PART
 &CC       SETA      &TYPE-&CTYPE        INITIAL SETTING IS 0, 1, OR 2
           MEND

#endif
