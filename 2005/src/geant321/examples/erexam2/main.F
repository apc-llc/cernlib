*
* $Id: main.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      PROGRAM EREXAM2
*
*      GEANE test program
*
*      It includes fitting examples by
*      V.Innocente, PPE Division, CERN
*
*
      PARAMETER (NG=100000,NH=100000)
      COMMON/PAWC  /H(NH)
      COMMON/GCBANK/Q(NG)
*
#include "geant321/gcflag.inc"
*
      COMMON /COORD/CTRU(6,10),NMEA(10),charg
*
      PARAMETER (NDAT= 8*(6+5+5+1)+1)
      REAL X(NDAT)
      CHARACTER*8 TAGS(NDAT)
      DATA TAGS/
     +      'xt1','yt1','zt1','pt1','dyt1','dzt1','p1','dy1','dz1','y1',
     +      'z1','sp1','sdy1','sdz1','sy1','sz1','c1',
     +      'xt2','yt2','zt2','pt2','dyt2','dzt2','p2','dy2','dz2','y2',
     +      'z2','sp2','sdy2','sdz2','sy2','sz2','c2',
     +      'xt3','yt3','zt3','pt3','dyt3','dzt3','p3','dy3','dz3','y3',
     +      'z3','sp3','sdy3','sdz3','sy3','sz3','c3',
     +      'xt4','yt4','zt4','pt4','dyt4','dzt4','p4','dy4','dz4','y4',
     +      'z4','sp4','sdy4','sdz4','sy4','sz4','c4',
     +      'xt5','yt5','zt5','pt1','dyt5','dzt5','p5','dy5','dz5','y5',
     +      'z5','sp5','sdy5','sdz5','sy5','sz5','c5',
     +      'xt6','yt6','zt6','pt6','dyt6','dzt6','p6','dy6','dz6','y6',
     +      'z6','sp6','sdy6','sdz6','sy6','sz6','c6',
     +      'xt7','yt7','zt7','pt7','dyt7','dzt7','p7','dy7','dz7','y7',
     +      'z7','sp7','sdy7','sdz7','sy7','sz7','c7',
     +      'xt8','yt8','zt8','pt8','dyt8','dzt8','p8','dy8','dz8','y8',
     +      'z8','sp8','sdy8','sdz8','sy8','sz8','c8','chi2'/
*
*_____________________________________________________________________
*
*
c      call apollo_errrec
*
      CALL GZEBRA( NG)
      CALL HLIMIT(-NH)
*
      CALL UGINIT
*
      IF (ISWIT(3).GE.10) THEN
*         geant mode
*
        IF (ISWIT(3).LT.15) THEN
          call hbook1(1,'[D]x',100,-1.,1.,0.)
          call hbook1(2,'[D]y',100,-1.,1.,0.)
          call hbook1(3,'[D]z',100,-1.,1.,0.)
          call hbook1(4,'[D](p?y!/p?x!)',100,-.01,.01,0.)
          call hbook1(5,'[D](p?z!/p?x!)',100,-.01,.01,0.)
          call hbook1(6,'[D]p/p',100,-.1,.1,0.)
          call hbook1(11,'[D]x/[s]',100,-5.,5.,0.)
          call hbook1(12,'[D]y/[s]',100,-5.,5.,0.)
          call hbook1(13,'[D]z/[s]',100,-5.,5.,0.)
          call hbook1(14,'[D](p?y!/p?x!)/[s]',100,-5.,5.,0.)
          call hbook1(15,'[D](p?z!/p?x!)/[s]',100,-5.,5.,0.)
          call hbook1(16,'[D](1/p)/[s]',100,-5.,5.,0.)
          call hbook1(100,'P([v]^2!)',100,0.,1.,0.)
          do i=1,15
            call hbook1(100+i,'[D]x+y/[s]?x+y!',100,-5.,5.,0.)
          enddo
*
        ELSE
*            fit mode
          CALL VZERO(CTRU,70)
*
          call hbook1(1,'[D]p/p',100,-.5,.5,0.)
          call hbook1(2,'[D](p?y!/p?x!)',100,-.015,.015,0.)
          call hbook1(3,'[D](p?z!/p?x!)',100,-.015,.015,0.)
          call hbook1(4,'[D]y',100,-0.2,0.2,0.)
          call hbook1(5,'[D]z',100,-0.5,0.5,0.)
          do i=1,8
            call hbook1(10*i+1,'[D](1/p)/[s]',100,-5.,5.,0.)
            call hbook1(10*i+2,'[D](p?y!/p?x!)/[s]',100,-5.,5.,0.)
            call hbook1(10*i+3,'[D](p?z!/p?x!)/[s]',100,-5.,5.,0.)
            call hbook1(10*i+4,'[D]y/[s]',100,-5.,5.,0.)
            call hbook1(10*i+5,'[D]z/[s]',100,-5.,5.,0.)
             call hbook1(10*i+6,'[D]?m!y/[s]',100,-5.,5.,0.)
            call hbook1(10*i+7,'[D]]?m!z/[s]',100,-5.,5.,0.)
            call hbook1(10*i,'P([v]^2!)',100,0.,1.,0.)
*            call hbook1(10*i+8,'ysigma',100,0.,5.,0.)
*            call hbook1(10*i+9,'zsigma',100,0.,5.,0.)
          enddo
          call hbook1(1000,'P([v]^2!)',100,0.,1.,0.)
          do i=1,3
            call hbook1(1000*i+1,'P([v]^2!)',100,0.,1.,0.)
            call hbook1(1000*i+2,'P([v]^2!)',100,0.,1.,0.)
            call hbook1(1000*i+3,'P([v]^2!?t!)',100,0.,1.,0.)
            call hbook1(1000*i+4,'P([v]^2!?m!)',100,0.,1.,0.)
          enddo
          call hbprof(1011,'[s]?1/p!',8,0.,8.,0.,10.,' ')
          call hbprof(1012,'[s]?py/px!',8,0.,8.,0.,10.,' ')
          call hbprof(1013,'[s]?pz/px!',8,0.,8.,0.,10.,' ')
          call hbprof(1014,'[s]?y!',8,0.,8.,0.,10.,' ')
          call hbprof(1015,'[s]?z!',8,0.,8.,0.,10.,' ')
*
*  book a n-tuple
*
        CALL HROPEN(62,'EREX2','erexam2.ntp','N',1024,IHSTA)
        IF (IHSTA.NE.0) THEN
          PRINT *, 'PROBLEMS WITH HROPEN!!!!!'
        ENDIF
        CALL HBOOKN(999,'erexam2 NTUPLE',NDAT,'EREX2',1000,TAGS)
 
 
*
        ENDIF
*
        CALL GRUN
      ELSE
*       geane mode
*
        IF (ISWIT(3).GE.4) THEN
          call hbook1(100,'[d](1/p)',100,-.1,.1,0.)
          call hbook1(200,'[d](p?y!/p?x!)',100,-.2,.2,0.)
          call hbook1(300,'[d](p?z!/p?x!)',100,-.2,.2,0.)
          call hbook1(400,'[d]y',100,-1.0,1.0,0.)
          call hbook1(500,'[d]z',100,-1.0,1.0,0.)
          call hbook1(1,'[Dd](1/p)',100,-.01,.01,0.)
          call hbook1(2,'[Dd](p?y!/p?x!)',100,-.01,.01,0.)
          call hbook1(3,'[Dd](p?z!/p?x!)',100,-.01,.01,0.)
          call hbook1(4,'[Dd]y',100,-0.1,0.1,0.)
          call hbook1(5,'[Dd]z',100,-0.1,0.1,0.)
          call hbook1(11,'[Dd](1/p)/[d]',100,-.2,.2,0.)
          call hbook1(12,'[Dd](p?y!/p?x!)/[d]',100,-.2,.2,0.)
          call hbook1(13,'[Dd](p?z!/p?x!)/[d]',100,-.2,.2,0.)
          call hbook1(14,'[Dd]y/[d]',100,-0.2,0.2,0.)
          call hbook1(15,'[Dd]z/[d]',100,-0.2,0.2,0.)
        ENDIF
*
        CALL TIMEX(TIME)
        CALL ERTEST
        CALL TIMEX(TIM2)
        print *, 'elapsed time ',tim2-time
      ENDIF
*
      CALL GLAST
*
* dump last record of n-tuple and close file
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//EREX2',' ')
      CALL HROUT(999,ICYCLE,' ')
      CALL HREND('EREX2')
      CALL HDELET(999)
*
      call HRPUT(0,'erexam2.hrz',' ')
C      CALL HISTDO
*
      STOP
      END
