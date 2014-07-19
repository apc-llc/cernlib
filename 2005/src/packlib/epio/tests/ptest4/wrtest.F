*
* $Id: wrtest.F,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: wrtest.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
      PROGRAM WRTEST
*
*--- Step 1: create EPIO file
*
      DIMENSION IHEAD(4),IOUT(3000),IBUF(5000)
      DATA LUN/11/
 
      PRINT 10000
10000 FORMAT(//' ++++++++++++ EPIO random access test',
     +         ' ++++++++++++'//
     +         '              pass 1 starting execution'/)
      CALL EPINIT
      CALL EPSETW(LUN,3,32,IERR)
      CALL EPSETW(LUN,5,-1,IERR)
#if defined(CERNLIB_VAX)
      CALL EPDEFU(LUN,'TEST.DATA',IERR)
#endif
      DO 10  I=1,21
      NW=100*I
      CALL VFILL(IOUT,NW,I)
      IF(MOD(I,5).EQ.1)  THEN
         IHEAD(2)=1
      ELSE
         IHEAD(2)=0
      ENDIF
      CALL EPOUTL(LUN,3,4,IHEAD,NW,IOUT,IBUF,IERR)
   10 CONTINUE
      CALL EPCLOS(LUN,IBUF,IERR)
      PRINT 10010
10010 FORMAT(//' ------------ EPIO random access test',
     +         ' ------------'//
     +         '              pass 1 terminates.'/)
      END
