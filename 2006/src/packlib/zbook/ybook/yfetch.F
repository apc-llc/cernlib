*
* $Id: yfetch.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: yfetch.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
      SUBROUTINE YFETCH(IZ,ID,LUNIT,KEY)
C
C     ******************************************************************
C     *                                                                *
C     *       FETCHS BANK ID STORED ON LOGICAL UNIT LUNIT              *
C     *       WITH PASSWORD=KEY AND CREATES A BANK                     *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1),KEY(1)
C
C     ------------------------------------------------------------------
C
      CALL ZFETCH(IZ,ID,LUNIT,KEY)
      RETURN
      END
