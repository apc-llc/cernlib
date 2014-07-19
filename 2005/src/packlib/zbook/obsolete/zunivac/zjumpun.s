*
* $Id: zjumpun.s,v 1.1.1.1 1996/03/08 12:01:16 mclareni Exp $
*
* $Log: zjumpun.s,v $
* Revision 1.1.1.1  1996/03/08 12:01:16  mclareni
* Zbook
*
*
#if defined(CERNLIB_UNIVAC)
@ASM,IS ,ZJUMP
      AXR$
$(1),ZJUMP*
      LA          A2,0,A0                   .
      LA          A3,0,A2                   . ADDR(NAME)
      AA,U        A0,1                      . AO POINTS TO P1-ITEM IN
      SSC         A0,18                     . PARAMETER LIST
      ANA,U       A0,1                      . SUBTRACT # OF ARGS BY ONE
      OR,U        A0,01000                  . DISABLE TYPE-CHECKING
      LA          A0,A1                     .
      SSC         A0,18                     .
      J           0,A3                      . JUMP ON TO 'NAME'
      END
#endif
