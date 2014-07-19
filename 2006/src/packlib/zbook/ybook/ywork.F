*
* $Id: ywork.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: ywork.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
      SUBROUTINE YWORK(IZ,IWORK)
C
C     ******************************************************************
C     *                                                                *
C     *       RESERVES NWORK WORDS OF WORKINS SPACE IN BLANK COMMON    *
C     *       REDEFINES SIZE OF DYNAMIC STRUCTURE IF NOT ENOUGH SPACE  *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      CALL ZWORK(IZ,IWORK)
      RETURN
      END
