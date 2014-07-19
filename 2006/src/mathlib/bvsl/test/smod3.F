*
* $Id: smod3.F,v 1.1.1.1 1996/03/21 17:19:55 mclareni Exp $
*
* $Log: smod3.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:55  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.44.00  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.28  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE SMOD3 (ILENG,A1,A2,A3,OUT,IVEC)
      DIMENSION IVEC(*),A1(*),A2(*),A3(*),OUT(*)
      DO I = 1,ILENG
        OUT(IVEC(I)) = SQRT (A1(IVEC(I))**2+A2(IVEC(I))**2+
     .                       A3(IVEC(I))**2)
      ENDDO
      RETURN
      END
