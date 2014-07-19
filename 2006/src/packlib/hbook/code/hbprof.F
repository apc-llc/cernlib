*
* $Id: hbprof.F,v 1.2 1998/10/05 14:38:54 couet Exp $
*
* $Log: hbprof.F,v $
* Revision 1.2  1998/10/05 14:38:54  couet
* - New option P for profile histograms. Implemented by:  Nello Nappi
*   <Nello.Nappi@cern.ch>
*
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/04 02/06/94  14.09.35  by  Rene Brun
*-- Author :
      SUBROUTINE HBPROF(IDD,CHTITL,NX,XX0,XX1,YMIN,YMAX,CHOPT)
*.==========>
*.           booking of a profile histogram
*.
*.    NOTE: The computation of the errors is based on a proposal by
*.          Stephane Coutu. below is a copy of the email with the proposal
*.          ONLY options 'S' and 'I' are implemented
*.
*.    I realized that there is another case where this kind of trouble
*.    occurs: if a bin has N data points all with the same value Y (especially
*.    possible when dealing with integers), the spread in Y for that bin
*.    is zero, and the uncertainty assigned is also zero, and the bin is
*.    ignored in making subsequent fits. If SQRT(Y) was the correct error
*.    in the case above, then SQRT(Y)/SQRT(N) would be the correct error here.
*.    In fact, any bin with non-zero number of entries N but with zero spread
*.    should have an uncertainty SQRT(Y)/SQRT(N).
*.
*.    Now, is SQRT(Y)/SQRT(N) really the correct uncertainty? I believe
*.    that it is only in the case where the Y variable is some sort
*.    of counting statistics, following a Poisson distribution. This should
*.    probably be set as the default case. However, Y can be any variable
*.    from an original NTUPLE, not necessarily distributed "Poissonly",
*.    and perhaps extra options could be offered with the command:
*.    PROFILE id title ncx xmin xmax ymin ymax [ chopt ]
*.    to allow the user to choose how errors are calculated. We could have,
*.    for example:
*.    CHOPT
*.     ' '  (Default) Errors are Spread/SQRT(N) for Spread.ne.0. ,
*.                      "     "  SQRT(Y)/SQRT(N) for Spread.eq.0,N.gt.0 ,
*.                      "     "  0.  for N.eq.0
*.     'S'            Errors are Spread  for Spread.ne.0. ,
*.                      "     "  SQRT(Y)  for Spread.eq.0,N.gt.0 ,
*.                      "     "  0.  for N.eq.0
*.     'I'            Errors are Spread/SQRT(N) for Spread.ne.0. ,
*.                      "     "  1./SQRT(12.*N) for Spread.eq.0,N.gt.0 ,
*.                      "     "  0.  for N.eq.0
*.     'P'            This option allows to overcome the severe precision 
*.                    problems which occurr in the error calculation when the 
*.                    standard deviation is much smaller than the mean. In such
*.                    a case, the use of the standard formula:
*.
*.                    < ( y - <y> )^2 > = < y^2> - <y>^2
*.
*.                    involves the calculation of a small difference between 
*.                    two large numbers. The problem is avoided by accumulating 
*.                    the sum of the squares of the deviations of y with respect
*.                    to the mean, instead of the sum of the squares of y. The
*                     prior knowledge of the mean is avoided by the use of an 
*.                    iterative formula making use of the running average.
*.                    This option has been implemented by:
*.                    Nello Nappi <Nello.Nappi@cern.ch>
*.
*.    The third case above corresponds to Integer Y values for which the
*.    uncertainty is +-0.5, with the assumption that the probability that Y
*.    takes any value between Y-0.5 and Y+0.5 is uniform (the same argument
*.    goes for Y uniformly distributed between Y and Y+1); this would be
*.    useful if Y is an ADC measurement, for example. Other, fancier options
*.    would be possible, at the cost of adding one more parameter to the PROFILE
*.    command. For example, if all Y variables are distributed according to some
*.    known Gaussian of standard deviation Sigma, then:
*.     'G'            Errors are Spread/SQRT(N) for Spread.ne.0. ,
*.                      "     "  Sigma/SQRT(N) for Spread.eq.0,N.gt.0 ,
*.                      "     "  0.  for N.eq.0
*.    For example, this would be useful when all Y's are experimental quantities
*.    measured with the same instrument with precision Sigma.
*.
*.    Stephane Coutu
*.    coutu@roo.physics.lsa.umich.edu
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcopt.inc"
      CHARACTER*(*) CHTITL,CHOPT
      DIMENSION IOPTS(3)
*.___________________________________________
      IERR=0
      IF(IDD.EQ.0)THEN
         CALL HBUG('ID=0 is an illegal identifier','HBPROF',IDD)
         RETURN
      ENDIF
*
*
*             Automatic boundaries adjustment
      ID=IDD
      X0=XX0
      X1=XX1
      IF(KBINSZ.NE.0)THEN
         CALL HBIN(X0,X1,NX,X0,X1,NXX,BWID)
         X1=X0+BWID*FLOAT(NX)
      ENDIF
      IF(X1.LE.X0)THEN
         CALL HBUG('XMIN.GE.XMAX','HBPROF',IDD)
      ENDIF
*
*             Check if ID already in the table
*
      NRHIST=IQ(LCDIR+KNRH)
      IDPOS=LOCATI(IQ(LTAB+1),NRHIST,ID)
      IF(IDPOS.GT.0)THEN
         CALL HBUG('+Already existing histogram replaced','HBPROF',IDD)
         CALL HDELET(IDD)
         NRHIST=IQ(LCDIR+KNRH)
         IDPOS=-IDPOS+1
      ENDIF
*
*             Get title length
*
      CALL HBTIT(CHTITL,NWTITL,NCHT)
      NWH=NX+1+KCON1
      NWID=NWTITL+KTIT1-1
      NTOT=NWH+NWID+43+2*NX
*
*             Enough space left ?
*
      CALL HSPACE(NTOT+1000,'HBPROF',IDD)
      IF(IERR.NE.0)                    GO TO 99
*
*             Enter ID in the list of ordered IDs
*
      IDPOS=-IDPOS+1
      IF(NRHIST.GE.IQ(LTAB-1))THEN
         CALL MZPUSH(IHDIV,LTAB,500,500,' ')
      ENDIF
      DO 10 I=NRHIST,IDPOS,-1
         IQ(LTAB+I+1)=IQ(LTAB+I)
         LQ(LTAB-I-1)=LQ(LTAB-I)
  10  CONTINUE
*
*             Build histogram data structure
*
      IF(LIDS.EQ.0)THEN
         CALL MZBOOK(IHDIV,LIDS,LCDIR,-2,'HIDP',1,1,NWID,IOH1,0)
         LCID=LIDS
      ELSE
         LLID=LQ(LCDIR-9)
         CALL MZBOOK(IHDIV,LCID,LLID,  0,'HIDP',1,1,NWID,IOH1,0)
      ENDIF
      LQ(LCDIR-9)=LCID
      CALL MZBOOK(IHDIV,LCONT,LCID,-1,'HCOP',2,2,NWH,IOCF1,0)
      CALL MZBOOK(IHDIV,LR1,LCONT,0,'HI1E',0,0,NX,3,0)
      CALL MZBOOK(IHDIV,LR2,LR1  ,0,'HI1N',0,0,NX,3,0)
*
      IQ(LCID-5)=ID
      IQ(LTAB+IDPOS)=ID
      LQ(LTAB-IDPOS)=LCID
      CALL SBIT1(IQ(LCID+KBITS),1)
      CALL SBIT1(IQ(LCID+KBITS),8)
      CALL SBIT1(IQ(LCID+KBITS),9)
      CALL HUOPTC(CHOPT,'SIP',IOPTS)
      IF(IOPTS(1).NE.0)CALL SBYT(1,IQ(LR1),1,2)
      IF(IOPTS(2).NE.0)CALL SBYT(2,IQ(LR1),1,2)
      IF(IOPTS(3).NE.0)CALL SBIT1(IQ(LR1),3) 
*
*             Automatic filling of statistics
*
      IF(ISTAF.NE.0)THEN
         CALL SBIT1(IQ(LCID+KBITS),7)
      ENDIF
*
      IF(NWTITL.NE.0)THEN
         CALL UCTOH(CHTITL,IQ(LCID+KTIT1),4,NCHT)
      ENDIF
*
      IQ(LCID+KNTOT)=NTOT
      IQ(LCID+KNCX)=NX
      Q (LCID+KXMIN)=X0
      Q (LCID+KXMAX)=X1
*
      Q (LCID+KXMAX+1)=FLOAT(NX)/(X1-X0)
      Q (LCID+KMIN1) =YMIN
      Q (LCID+KMAX1) =YMAX
      LCONT=LQ(LCID-1)
      IQ(LCONT+KNBIT)=32
      NRHIST=NRHIST+1
      IQ(LCDIR+KNRH)=NRHIST
*
  99  RETURN
      END
