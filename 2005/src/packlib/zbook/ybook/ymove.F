*
* $Id: ymove.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: ymove.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
      SUBROUTINE YMOVE(IZ,N)
C
C     ******************************************************************
C     *                                                                *
C     *       MOVE END OF DYNAMIC AREA IZ BY N WORDS                   *
C     *       N POSITIVE OR NEGATIVE                                   *
C     *       IF N.GT.0 POSSIBILITY OF MEMORY EXTENSION                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      CALL ZMOVE(IZ,N)
      RETURN
      END
