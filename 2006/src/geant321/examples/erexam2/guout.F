*
* $Id: guout.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: guout.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
*
*     Call for Geane tracking at the end of Geant tracking.
*          Then compare the two tracking.
*
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/ertrio.inc"
*
      real gxi(3),gpi(4),gxo(3),gpo(4)
      real ubuf(10)
      real plani(3,2),plano(3,3,1),xxo(3),po(4),rc(15)
      real dx(3),dp(3),dd(5),err(5,5),ww(5,5)
*
      double precision p,gp,chi2
*
*     ------------------------------------------------------------------
*
      IF (ISWIT(3).GE.15) THEN
*            fit mode
        CALL GEAFIT
        RETURN
      ENDIF
      call vzero(rc,15)
      do i=1,3
        gxo(i) = vect(i)
        gpo(i) = vect(7)*vect(3+i)
      enddo
      gxo(1) = gxo(1) - 0.002
*
      call gfkine(1,gxi,gpi,ipart,nvert,ubuf,nwbuf)
*
      CALL VZERO(PLANI,6)
      PLANI(2,1) =  1.
      PLANI(3,2) =  1.
      CALL UCOPY(PLANI,PLANO(1,1,1),6)
      PLANO(2,3,1) = 0.
      PLANO(3,3,1) = 0.
      if (ISWIT(3).eq.11) then
        PLANO(1,3,1) = gxo(1)
        CALL EUFILP(1,RC,PLANI,PLANO)
        CALL ERTRAK(gxi,gpi,xxo,po,ipart,'EP')
        IF (IEPRED(1).NE.1) go to 999
        gp = 0.d0
        p  = 0.d0
        do i=1,3
          dx(i) = gxo(i)-xxo(i)
          gp = gp + gpo(i)**2
          p = p + po(i)**2
          dp(i) = gpo(i)/gpo(1)-po(i)/po(1)
        enddo
      elseif (ISWIT(3).eq.12) then
        PLANO(1,3,1) = gxi(1)
        CALL EUFILP(1,RC,PLANI,PLANO)
        CALL ERTRAK(gxo,gpo,xxo,po,ipart,'EPB')
        IF (IEPRED(1).NE.1) go to 999
        gp = 0.d0
        p  = 0.d0
        do i=1,3
          dx(i) = gxi(i)-xxo(i)
          gp = gp + gpi(i)**2
          p = p + po(i)**2
          dp(i) = gpi(i)/gpi(1)-po(i)/po(1)
        enddo
      endif
*
      do i=1,3
        call hfill(i,dx(i),0.,1.)
      enddo
      call hfill(4,dp(2),0.,1.)
      call hfill(5,dp(3),0.,1.)
      dpp =  1./sqrt(gp)-1./sqrt(p)
      dpop = sqrt(gp)*dpp
      call hfill(6,dpop,0.,1.)
      call hfill(16,dpp/sqrt(errout(1,1)),0.,1.)
      call hfill(12,dx(2)/sqrt(errout(13,1)),0.,1.)
      call hfill(13,dx(3)/sqrt(errout(15,1)),0.,1.)
      call hfill(14,dp(2)/sqrt(errout(6,1)),0.,1.)
      call hfill(15,dp(3)/sqrt(errout(10,1)),0.,1.)
      dd(1) = dpp
      dd(2) = dp(2)
      dd(3) = dp(3)
      dd(4) = dx(2)
      dd(5) = dx(3)
      k = 0
      do i=1,5
      do j=i,5
        k= k +1
        err(i,j) = errout(k,1)
        err(j,i) = err(i,j)
      enddo
      enddo
      call ucopy(err,ww,25)
      call rsinv(5,ww,5,ifail)
      k = 0
      do i=1,5
      do j=i,5
        k= k +1
cc        Exy = dd(j)**2*err(i,i) + dd(i)**2*err(j,j)
cc     +      + 2*dd(i)*dd(j)*err(i,j)
cc        if (Exy.gt.0) call hfill(100+k,dd(i)*dd(j)/sqrt(Exy),0.,1.)
        Exy = err(i,i) + err(j,j) + 2*err(i,j)
        if (Exy.gt.0) call hfill(100+k,(dd(i)+dd(j))/sqrt(Exy),0.,1.)
      enddo
      enddo
      chi2 = 0.d0
      do i=1,5
      do j=1,5
        chi2 = chi2 + dd(i)*ww(i,j)*dd(j)
      enddo
      enddo
      call hfill(100,prob(sngl(chi2),5),0.,1.)
*
999   continue
*
      END
