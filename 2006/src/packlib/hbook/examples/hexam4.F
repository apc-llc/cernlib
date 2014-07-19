*
* $Id: hexam4.F,v 1.2 1996/06/06 09:52:42 cernlib Exp $
*
* $Log: hexam4.F,v $
* Revision 1.2  1996/06/06 09:52:42  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
*CMZ :  4.10/05 22/11/89  18.48.27  by  Rene Brun
*-- Author :
      SUBROUTINE HEXAM4
*.==========>
*.           TEST PRINTING OPTIONS
*..=========> ( R.Brun )
      DATA XMIN,XMAX/0.,1./
*.___________________________________________
*.
      CALL HTITLE('EXAMPLE NO = 4')
*
*             Get hist 110 from data base
*
      CALL HRGET(110,'hexam.dat',' ')
*
*             Book 2 new histograms
*
      CALL HBOOK1(1000,'TEST OF PRINTING OPTIONS',40,1.,41.,0.)
      CALL HBOOK1(2000,'TEST OF BIG BIN',20,XMIN,XMAX,0.)
      CALL HIDOPT(1000,'ERRO')
*
*             Fills new IDs
*
      DO 10 I=1,40
         J=2*I-1
         W=HI(110,J)+HI(110,J+1)
         CALL HFILL(1000,FLOAT(I),0.,W)
  10  CONTINUE
*
      DO 20 I=1,20
         J=5*I
         W=SQRT(HI(110,J))
         CALL HIX(2000,I,X)
         CALL HF1(2000,X,W)
  20  CONTINUE
*
*             Set various printing options
*
      CALL HIDOPT(110,'BLAC')
      CALL HIDOPT(110,'NPLO')
      CALL HIDOPT(110,'NPST')
      CALL HPHIST(110,'HIST',1)
      CALL HMAXIM(110,100.)
      CALL HIDOPT(110,'1EVL')
      CALL HIDOPT(110,'NPCH')
      CALL HPHIST(110,'HIST',1)
*
      CALL HIDOPT(1000,'NPCH')
      CALL HIDOPT(1000,'NPCO')
      CALL HPROT(1000,'HIST',1)
      CALL HIDOPT(1000,'LOGY')
      CALL HPRINT(1000)
      CALL HIDOPT(1000,'INTE')
      CALL HIDOPT(1000,'PERR')
      CALL HIDOPT(1000,'ROTA')
      CALL HPRINT(1000)
*
      CALL HBIGBI(2000,5)
      CALL HIDOPT(2000,'NPCO')
      CALL HIDOPT(2000,'NPLO')
      CALL HPRINT(2000)
*
      END
