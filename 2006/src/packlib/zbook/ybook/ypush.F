*
* $Id: ypush.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: ypush.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
      SUBROUTINE YPUSH(IZ,ID,NPUSH)
C
C     ******************************************************************
C     *                                                                *
C     *       REQUESTS N MORE (OR LESS) WORDS IN BANK ID (BLANK COMMON)*
C     *       IF NOT ENOUGH SPACE REQUESTS MORE WORDS FOR              *
C     *       THE DYNAMIC STRUCTURE THROUGH THE HBOOK ROUTINE HISTGO   *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 17) = 1
      CALL ZPUSHS(IZ,ID,NPUSH,0)
      RETURN
C
      END
