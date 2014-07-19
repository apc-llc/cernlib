*
* $Id: ertest.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: ertest.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE ERTEST
*
*     Main routine for a call to geane tracking alone.
*
*     Card  'kine' :  itype    momentum
*
#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
#include "geant321/gcflag.inc"
#include "geant321/ertrio.inc"
*
      REAL XXI(3), PII(3), XXO(3), PO(3), XIO(3),PIO(3)
      REAL RC(15), PLANI(3,2), PLANO(3,3,8)
      REAL PLAB(3)
      REAL XX(3,8),PP(3,8),DX(3),DP(3)
      REAL XST(5),XAR(5),YST(5),YAR(5),DEL(5),DIF(5)
      DOUBLE PRECISION TT(5,5)
*
      character * 4 chopt, cnamv(10)
*
      integer numv(10), ivol(10)
*
      DATA XXI/-109.998,0.0,0.0/,PLAB/0.0,0.0,0.0/
*
*
      JPA = LQ(JPART-IKINE)
      XMASS   = Q(JPA+7)
      PLAB(1) = PKINE(1)
      PLAB(2) = .1*PLAB(1)
      PLAB(3) = .1*PLAB(1)
      EVERT   = sqrt(dble(plab(1))**2+dble(xmass)**2) - xmass
      print *, 'particle ',ikine,' of momentum ',plab(1)
      overp = 1/sqrt(plab(1)**2+plab(2)**2+plab(3)**2)
*
      CALL VZERO (RC,15)
*
      CALL UCOPY(PLAB,PII,3)
*
      IF (ISWIT(4).EQ.1) THEN
        CALL VZERO(PLANI,6)
        PLANI(2,1) =  1.
        PLANI(3,2) =  1.
        XP = -62.
        DO IP=1,8
          CALL UCOPY(PLANI,PLANO(1,1,IP),6)
          XP = XP + 20.
          PLANO(1,3,IP) = XP
          PLANO(2,3,IP) = 0.
          PLANO(3,3,IP) = 0.
        ENDDO
        CALL EUFILP(8,RC,PLANI,PLANO)
      ELSE
        cnamv(1) = 'WBOX'
        numv(1) = 0
        ivol(1) = 1
        do i=2,10
          cnamv(i) = '    '
          numv(i) = 0
          ivol(i) = 1
        enddo
        CALL EUFILV(10,RC,cnamv,numv,ivol)
      ENDIF
*
      IF (ISWIT(3).EQ.2.OR.ISWIT(3).EQ.5) XXI(1) = - XXI(1)
*
      print *,'starting point ',xxi,pii
      IF (ISWIT(4).EQ.1) THEN
        chopt = 'EP  '
        IF (ISWIT(3).EQ.2.OR.ISWIT(3).EQ.5) chopt = 'EPB '
      ELSE
        chopt = 'EV  '
        IF (ISWIT(3).EQ.2.OR.ISWIT(3).EQ.5) chopt = 'EVB '
      ENDIF
      CALL ERTRAK(XXI,PII,XXO,PO,IKINE,CHOPT)
      print *,'arrival point ',xxo,po
*
      IF (ISWIT(3).GT.3) THEN
        xst(1) = erpin(1)
        xst(2) = erpin(2)
        xst(3) = erpin(3)
        xst(4) = xxi(2)
        xst(5) = xxi(3)
        xar(1) = erpout(1,ilpred)
        xar(2) = erpout(2,ilpred)
        xar(3) = erpout(3,ilpred)
        xar(4) = xxo(2)
        xar(5) = xxo(3)
        call ucopy(erdtrp(1,1,ilpred),tt,50)
      ENDIF
*
      print *, 'number of preditions ',ilpred
      do ip=1,ilpred
        print *, 'plane ',iepred(ip),' reached as ',ip
        print *,'position '
        print  '(3e15.5)',(erxout(i,ip),i=1,3)
        print *,'1/p, slopes '
        print  '(3e15.5)',(erpout(i,ip),i=1,3)
        print *,'arrival error '
        print  '(5e15.5)',(errout(i,ip),i=1,15)
        print *, 'transp matrix'
        print '(5e15.5)',((erdtrp(i,j,ip),i=1,5),j=1,5)
        if (iepred(ip).ne.0) then
          do i=1,3
            xx(i,iepred(ip)) = erxout(i,ip)
            pp(i,iepred(ip)) = erpout(i,ip)
          enddo
        endif
      enddo
*
      IF (ISWIT(3).EQ.3) THEN
        print *,'starting point ',xxo,po
        chopt = 'EPB '
        CALL ERTRAK(XXO,PO,XIO,PIO,IKINE,CHOPT)
        print *,'arrival point ',XIO,PIO
*
        print *, 'number of preditions ',ilpred
        do ip=1,ilpred
          print *, 'plane ',iepred(ip),' reached as ',ip
          print *,'position '
          print  '(3e15.5)',(erxout(i,ip),i=1,3)
          print *,'1/p, slopes '
          print  '(3e15.5)',(erpout(i,ip),i=1,3)
          print *,'arrival error '
          print  '(5e15.5)',(errout(i,ip),i=1,15)
          print *, 'transp matrix'
          print '(5e15.5)',((erdtrp(i,j,ip),i=1,5),j=1,5)
          if (iepred(ip).ne.0) then
            do i=1,3
              dx(i) = xx(i,iepred(ip)) - erxout(i,ip)
              dp(i) = pp(i,iepred(ip)) - erpout(i,ip)
            enddo
            print *,'dx',dx
            print *,'dp',dp
          endif
        enddo
      ENDIF
*
      IF(ISWIT(3).GT.3) THEN
*
        DEL(1) = 0.0
        DEL(2) = 0.0
        DEL(3) = 0.0
        DEL(1) = 0.3*overp
        DEL(2) = 0.1
        DEL(3) = 0.1
        DEL(4) = 1.
        DEL(5) = 1.
*
        pmax = 300.
*
        chopt = 'OP'
        IF (ISWIT(3).EQ.5) chopt = 'OPB '
*
        DO IE=1,NEVENT
          XXI(2) = DEL(4)*(2.*RNDM(XXI(2))-1.)
          XXI(3) = DEL(5)*(2.*RNDM(XXI(3))-1.)
10        p = 1/abs(overp + DEL(1)*(2.*RNDM(p)-1.))
          if (p.gt.pmax) go to 10
          PII(2) = p*(PLAB(2)*overp+DEL(2)*(2.*RNDM(PII(2))-1.))
          PII(3) = p*(PLAB(3)*overp+DEL(3)*(2.*RNDM(PII(3))-1.))
          p2 =  p**2-pii(2)**2-pii(3)**2
          if (p2.le.0) go to 10
          PII(1) = sqrt(p2)
          CALL ERTRAK(XXI,PII,XXO,PO,IKINE,CHOPT)
          Yst(1) = erpin(1)
          Yst(2) = erpin(2)
          Yst(3) = erpin(3)
          Yst(4) = xxi(2)
          Yst(5) = xxi(3)
          Yar(1) = erpout(1,ilpred)
          Yar(2) = erpout(2,ilpred)
          Yar(3) = erpout(3,ilpred)
          Yar(4) = xxo(2)
          Yar(5) = xxo(3)
          call vzero(dif,5)
          do i=1,5
            do j=1,5
              dif(i) = dif(i) + tt(i,j)*(yst(j)-xst(j))
            enddo
              dif(i) = Yar(i) - (xar(i)+dif(i))
              call hfill(i,dif(i),0.,1.)
              call hfill(10+i,dif(i)/(yar(i)-xar(i)),0.,1.)
              call hfill(100*i,(yst(i)-xst(i)),0.,1.)
          enddo
c          print *,' dsta ',(yst(j)-xst(j),j=1,5)
c          print *,' darr ',(yar(j)-xar(j),j=1,5)
c          print *,' diff ',dif
 
        ENDDO
      ENDIF
*
      END
