*
* $Id: rdtest.F,v 1.1.1.1 1996/03/08 15:21:46 mclareni Exp $
*
* $Log: rdtest.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:46  mclareni
* Epio
*
*
      PROGRAM RDTEST
*
*--- Step 2: scan for block count and off-set, write onto unit 22
*
      DIMENSION IHEAD(4),IW(31),IOUT(3000),IBUF(5000)
      DIMENSION IB(100),IP(100)
      DATA LUN/11/,MODE/20/,MULT/100/,MB/0/
 
      PRINT 10000
10000 FORMAT(//' ++++++++++++ EPIO random access test',
     +         ' ++++++++++++'//
     +         '              pass 2 starting execution'/)
 
      CALL EPINIT
      NR=0
#if defined(CERNLIB_VAX)
      CALL EPDEFU(LUN,'TEST.DATA',IERR)
#endif
   10 CONTINUE
      CALL EPREAD(LUN,MODE,NW,IOUT,IBUF,IERR)
      IF(IERR.NE.0)  GOTO 100
      NR=NR+1
   20 CONTINUE
      CALL EPGETA(LUN,31,IW,IERR)
      PRINT *,(IOUT(J),J=1,NW)
      IF(IOUT(2).EQ.1)  THEN
         MB=MB+1
*--- get block count
         IB(MB)=IW(11)
*--- get record off-set in block
         IP(MB)=IW(31)
      ENDIF
      PRINT *,NR,IW(11),IW(12),IW(22),IW(23),IW(31)
      GOTO 10
  100 CONTINUE
      IF(IERR.EQ.1)  THEN
      PRINT 10010
10010 FORMAT(//' ------------ EPIO random access test',
     +         ' ------------'//
     +         '              pass 2 terminates.'/)
*
*--- write block numbers and off-sets on a temporary file
*
      WRITE(22,*)  MB,(IB(I),IP(I),I=1,MB)
      ELSE
      PRINT *,' ********** STOP because of EPIO error.'
      ENDIF
      END
