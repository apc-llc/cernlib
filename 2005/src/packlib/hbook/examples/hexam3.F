*
* $Id: hexam3.F,v 1.2 1996/06/06 09:52:41 cernlib Exp $
*
* $Log: hexam3.F,v $
* Revision 1.2  1996/06/06 09:52:41  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
*CMZ :  4.10/05 22/11/89  18.48.26  by  Rene Brun
*-- Author :
      SUBROUTINE HEXAM3
*.==========>
*.           MORE BOOKING OPTIONS
*..=========> ( R.Brun )
*.
      CALL HTITLE('EXAMPLE NO = 3')
*
*             Get all histograms saved in example 2
*
      CALL HROPEN(1,'HEXAM','hexam.dat','U',1024,ISTAT)
      CALL HRIN(0,9999,0)
      CALL HMDIR('HEXAM3','S')
*
*             Print an index of all histograms that are now in memory
*
      CALL HINDEX
*
*             Reset hist 110 and 210.  adds more options
*
      CALL HRESET(110,' ')
      CALL HRESET(210,' ')
      CALL HIDOPT(110,'STAT')
      CALL HBARX(210)
      CALL HBPROX(210,0.)
      CALL HBSLIX(210,3,1000.)
      CALL HBANDY(210,0.1,0.5,0.)
      CALL HIDOPT(0,'1EVL')
*
*             New filling
*
      DO 10 I=1,2000
         CALL HFILL(110,HRNDM1(10),0.,1.)
         CALL HRNDM2(20,X,Y)
         CALL HFILL(210,X,Y,1.)
  10  CONTINUE
*
*             Print new contents using specialized printing routines
*             Same result could be obtained using HISTDO/HPRINT(0)/HPHS.
*
      CALL HPHIST(110,'HIST',1)
      CALL HPSCAT(210)
      CALL HPHIST(210,'PROX',1)
      CALL HPHIST(210,'BANY',1)
      CALL HPHIST(210,'SLIX',0)
*
*             Save all histograms in new directory HEXAM3
*
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('HEXAM')
      CLOSE (1)
*
      END
