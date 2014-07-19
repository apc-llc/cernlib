*
* $Id: pawdef.F,v 1.4 1999/08/31 08:47:17 couet Exp $
*
* $Log: pawdef.F,v $
* Revision 1.4  1999/08/31 08:47:17  couet
* - A new set of PAW commands using Neural Networks.
*   These new commands are using the MLPfit package.
*
* Revision 1.3  1999/04/08 09:37:57  couet
* - New version of the system function $HINFO and $HTITLE to make
*   them work on NT
*
* Revision 1.2  1997/10/23 13:00:21  mclareni
* NT mods
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWDEF
      EXTERNAL HCDIR
      EXTERNAL HEXIST
      EXTERNAL HNOENT
      EXTERNAL PHTIT
      EXTERNAL PHINFO
      EXTERNAL HPLGZO
      EXTERNAL HPLQOP
      EXTERNAL HPLQSE
      EXTERNAL HPLSET
      EXTERNAL IGQ
      EXTERNAL IGQWK
#if defined(CERNLIB_COMIS)
      EXTERNAL PACSFN
#endif
#ifndef CERNLIB_MSSTDCALL
      EXTERNAL PAQCUT
#else
	INTERFACE
	SUBROUTINE PAQCUT(name,n)
        !DEC$ ATTRIBUTES STDCALL,ALIAS:"_PAQCUT@12" :: PAQCUT
	CHARACTER*(*) name
	END
	END INTERFACE
#endif
      EXTERNAL PAXCUT
*.
*.          All the following routines have been generated
*.          automatically by the KUIP compiler from the PAW.CDF file
*.
      CALL VECDEF
      CALL HISDEF
      CALL FUNDEF
      CALL NTUDEF
      CALL GRADEF
      CALL PICDEF
      CALL ZEBDEF
      CALL FORDEF
      CALL NETDEF
      CALL PVEDEF
#if defined(CERNLIB_MLP)
      CALL MLPDEF
#endif
*      The obsolete menu is define here
      CALL OBSDEF
*
* define the PAW specific system functions
*
#if defined(CERNLIB_COMIS)
      CALL KUFDEF('$CALL(''fun(args)'')',
     +     'Call a Fortran REAL FUNCTION',
     +     'x=$CALL(s)',
     +     '-=R(s,$,i,x,d)',PACSFN)

      CALL KUFDEF('$ICALL(''ifun(args)'')',
     +     'Call an INTEGER FUNCTION',
     +     'i=$ICALL(s)',
     +     '-=I(s,$,i,x,d)',PACSFN)

      CALL KUFDEF('$LCALL(''lfun(args)'')',
     +     'Call a LOGICAL FUNCTION and return 0 or 1',
     +     'l=$LCALL(s)',
     +     '-=I(s,$,l,x,d)',PACSFN)

      CALL KUFDEF('$DCALL(''dfun(args)'')',
     +     'Call a DOUBLE PRECISION FUNCTION',
     +     'd=$DCALL(s)',
     +     '-=D(s,$,i,x,d)',PACSFN)
#endif

      CALL KUFDEF('$HCDIR()',
     +     'Current Hbook working directory',
     +     't=$HCDIR()',
     +     '-=R(t,$)',HCDIR)

      CALL KUFDEF('$HEXIST(id)',
     +     '1 if histogram ID exists or 0 otherwise',
     +     'l=$HEXIST(i)',
     +     'l=HEXIST(i)',HEXIST)

      CALL KUFDEF('$HINFO(id,''1DHISTO'')',
     +     '1 if ID is a 1D histogram or 0 otherwise',
     +     'x=$HINFO(i,S=1DHISTO)',
     +     '-=1DHISTO(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''2DHISTO'')',
     +     '1 if ID is a 2D histogram or 0 otherwise',
     +     'x=$HINFO(i,S=2DHISTO)',
     +     '-=2DHISTO(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''TABLE'')',
     +     '1 if ID is a table or 0 otherwise',
     +     'x=$HINFO(i,S=TABLE)',
     +     '-=TABLE(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''PROFILE'')',
     +     '1 if ID is a profile histogram or 0 otherwise',
     +     'x=$HINFO(i,S=PROFILE)',
     +     '-=PROFILE(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NTUPLE'')',
     +     '1 if ID is a Ntuple or 0 otherwise',
     +     'x=$HINFO(i,S=NTUPLE)',
     +     '-=NTUPLE(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''LOG'')',
     +     '1 if ID has LOG Y scale or 0 otherwise',
     +     'x=$HINFO(i,S=LOG)',
     +     '-=LOG(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''ENTRIES'')',
     +     'Number of entries',
     +     'k=$HINFO(i,S=ENTRIES)',
     +     '-=HNOENT(i,k)',HNOENT)

      CALL KUFDEF('$HINFO(id,''MEAN'')',
     +     'Mean value',
     +     'x=$HINFO(i,S=MEAN)',
     +     '-=MEAN(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''RMS'')',
     +     'Standard deviation',
     +     'x=$HINFO(i,S=RMS)',
     +     '-=RMS(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''EVENTS'')',
     +     'Number of equivalent events',
     +     'x=$HINFO(i,S=EVENTS)',
     +     '-=EVENTS(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''OVERFLOW'')',
     +     'Content of overflow channel',
     +     'x=$HINFO(i,S=OVERFLOW)',
     +     '-=OVERFLOW(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''UNDERFLOW'')',
     +     'Content of underflow channel',
     +     'x=$HINFO(i,S=UNDERFLOW)',
     +     '-=UNDERFLOW(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''MIN'')',
     +     'Minimum bin content',
     +     'x=$HINFO(i,S=MIN)',
     +     '-=HMIN(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''MAX'')',
     +     'Maximum bin content',
     +     'x=$HINFO(i,S=MAX)',
     +     '-=HMAX(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''SUM'')',
     +     'Total histogram content',
     +     'x=$HINFO(i,S=SUM)',
     +     '-=HSUM(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NSLIX'')',
     +     'Number of X slices',
     +     'x=$HINFO(i,S=NSLIX)',
     +     '-=NSLIX(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NSLIY'')',
     +     'Number of Y slices',
     +     'x=$HINFO(i,S=NSLIY)',
     +     '-=NSLIY(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NBANX'')',
     +     'Number of X bandes',
     +     'x=$HINFO(i,S=NBANX)',
     +     '-=NBANX(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NBANY'')',
     +     'Number of Y bandes',
     +     'x=$HINFO(i,S=NBANY)',
     +     '-=NBANY(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NPROX'')',
     +     'Projection X (0 or 1)',
     +     'x=$HINFO(i,S=NPROX)',
     +     '-=NPROX(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''NPROY'')',
     +     'Projection Y (0 or 1)',
     +     'x=$HINFO(i,S=NPROY)',
     +     '-=NPROY(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''XBINS'')',
     +     'Number of bins in X direction',
     +     'x=$HINFO(i,S=XBINS)',
     +     '-=XBINS(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''XMIN'')',
     +     'Lower histogram limit in X direction',
     +     'x=$HINFO(i,S=XMIN)',
     +     '-=XMIN(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''XMAX'')',
     +     'Upper histogram limit in X direction',
     +     'x=$HINFO(i,S=XMAX)',
     +     '-=XMAX(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''YBINS'')',
     +     'Number of bins in Y direction',
     +     'x=$HINFO(i,S=YBINS)',
     +     '-=YBINS(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''YMIN'')',
     +     'Lower histogram limit in Y direction',
     +     'x=$HINFO(i,S=YMIN)',
     +     '-=YMIN(x,$,i)',PHINFO)

      CALL KUFDEF('$HINFO(id,''YMAX'')',
     +     'Upper histogram limit in Y direction',
     +     'x=$HINFO(i,S=YMAX)',
     +     '-=YMAX(x,$,i)',PHINFO)

CCC      CALL KUFDEF('$HTITLE(id)',
CCC     +     'Histogram title',
CCC     +     't=$HTITLE(i)',
CCC     +     '-=HGIVE(i,t,I,R,R,I,R,R,I,I)',HGIVE)

      CALL KUFDEF('$HTITLE(id)',
     +     'Histogram title',
     +     's=$HTITLE(k)',
     +     '-=PHTIT(s,k)',PHTIT)


      CALL KUFDEF('$GRAFINFO(''XZONES'')',
     +     'Number of zones in X direction',
     +     'k=$GRAFINFO(S=XZONES)',
     +     '-=HPLGZO(k,I)',HPLGZO)

      CALL KUFDEF('$GRAFINFO(''YZONES'')',
     +     'Number of zones in Y direction',
     +     'k=$GRAFINFO(S=YZONES)',
     +     '-=HPLGZO(I,k)',HPLGZO)

      CALL KUFDEF('$GRAFINFO(''NT'')',
     +     'Current Normalization Transformation number',
     +     'x=$GRAFINFO(S=NT)',
     +     '-=NTNB(I,$,x)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''WNXMIN'')',
     +     'Lower X limit of window in current NT',
     +     'x=$GRAFINFO(S=WNXMIN)',
     +     '-=NTWN(I,$,x0)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''WNXMAX'')',
     +     'Upper X limit of window in current NT',
     +     'x=$GRAFINFO(S=WNXMAX)',
     +     '-=NTWN(I,$,x1)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''WNYMIN'')',
     +     'Lower Y limit of window in current NT',
     +     'x=$GRAFINFO(S=WNYMIN)',
     +     '-=NTWN(I,$,x2)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''WNYMAX'')',
     +     'Upper Y limit of window in current NT',
     +     'x=$GRAFINFO(S=WNYMAX)',
     +     '-=NTWN(I,$,x3)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''VPXMIN'')',
     +     'Lower X limit of viewport in current NT',
     +     'x=$GRAFINFO(S=VPXMIN)',
     +     '-=NTVP(I,$,x0)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''VPXMAX'')',
     +     'Upper X limit of viewport in current NT',
     +     'x=$GRAFINFO(S=VPXMAX)',
     +     '-=NTVP(I,$,x1)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''VPYMIN'')',
     +     'Lower Y limit of viewport in current NT',
     +     'x=$GRAFINFO(S=VPYMIN)',
     +     '-=NTVP(I,$,x2)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''VPYMAX'')',
     +     'Upper Y limit of viewport in current NT',
     +     'x=$GRAFINFO(S=VPYMAX)',
     +     '-=NTVP(I,$,x3)',IGQWK)

      CALL KUFDEF('$GRAFINFO(''TXALIH'')',
     +     'Horizontal text alignment',
     +     'x=$GRAFINFO(S=TXALIH)',
     +     '-=TXAL($,x0)',IGQ)

      CALL KUFDEF('$GRAFINFO(''TXALIV'')',
     +     'Vertical text alignment',
     +     'x=$GRAFINFO(S=TXALIV)',
     +     '-=TXAL($,x1)',IGQ)

      CALL KUFDEF('$GRAFINFO(''TXFONT'')',
     +     'Text font',
     +     'x=$GRAFINFO(S=TXFONT)',
     +     '-=TXFP($,x0)',IGQ)

      CALL KUFDEF('$GRAFINFO(''TXPREC'')',
     +     'Text precision',
     +     'x=$GRAFINFO(S=TXPREC)',
     +     '-=TXFP($,x1)',IGQ)

*--- TXAL and TXFP are returned by IGQ in 2-element arrays
      CALL KUFDEF(' ',
     +     ' ',
     +     'x=$GRAFINFO(S=?TXAL)',
     +     '-=TXAL(x,$,i)',PHINFO)

      CALL KUFDEF(' ',
     +     ' ',
     +     'x=$GRAFINFO(S=?TXFP)',
     +     '-=TXFP(x,$,i)',PHINFO)

      CALL KUFDEF('$GRAFINFO(''attr'')',
     +     'HPLOT/HIGZ attributes (see HELP SET for valid names)',
     +     'x=$GRAFINFO(u)',
     +     '-=HPLQSE(u,x)',HPLQSE)

      CALL KUFDEF('$OPTION(''option'')',
     +     '1 if the option is on 0 otherwise (see HELP OPTION)',
     +     'x=$OPTION(u)',
     +     '-=HPLQOP(u,x)',HPLQOP)

      CALL KUFDEF('$RGBINFO(icol,''R'')',
     +     'Weight of Red in color table',
     +     'x=$RGBINFO(x,S=R)',
     +     '-=RGB($,x1)',IGQ)

      CALL KUFDEF('$RGBINFO(icol,''G'')',
     +     'Weight of Green in color table',
     +     'x=$RGBINFO(x,S=G)',
     +     '-=RGB($,x2)',IGQ)

      CALL KUFDEF('$RGBINFO(icol,''B'')',
     +     'Weight of Blue in color table',
     +     'x=$RGBINFO(x,S=B)',
     +     '-=RGB($,x3)',IGQ)

      CALL KUFDEF('$CUT(n)',
     +     'Cut expression $n',
     +     's=$CUT(k)',
     +     '-=PAQCUT(s,k)',PAQCUT)

      CALL KUFDEF('$CUTEXPAND(string)',
     +     'Replace $n in the (quoted) string by $CUT(n)',
     +     's=$CUTEXPAND(t)',
     +     '-=PAXCUT(t,s)',PAXCUT)

      END
