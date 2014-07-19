*
* $Id: igtabl.F,v 1.6 2003/07/23 09:40:07 couet Exp $
*
* $Log: igtabl.F,v $
* Revision 1.6  2003/07/23 09:40:07  couet
* - The minimum value was not computed correctly, in some cases, when the
*   option LOGZ was requested. The starting value for the minimum is
*   taken in the first bin of the histogram. If this value was negative or
*   null it was set as the minimum even if LOGZ was on. Later on the minimum
*   was set as 0.001*ZMAX instead as the smaller positive value of the
*   histogram. So depending on the contents of the first bin, the plot produced
*   looked different.
*
* Revision 1.5  1998/12/07 13:38:31  couet
* - It is now possible to set the size of the characters in case of
*    option 'T' and 'C' (in percent of the bin heigh).
*
* Revision 1.4  1998/12/02 15:18:36  couet
* - Text is drawn bigger with T option
*
* Revision 1.3  1998/12/01 15:48:28  couet
* - Clean up: commented lines of code removed
*
* Revision 1.2  1996/08/06 15:14:11  couet
* - When ZMIN = ZMAX for a contour plot with a set of levels, a division
*  by 0 occured.
*
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  2.07/20 11/01/96  17.22.42  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGTABL(NX,NY,V,NPAR,PAR,CHOPTI)
*.===========>
*.
*.   This routine draws a table according to the value of CHOPT. The PAR input
*. parameter could be specified to change the aspect of the plot (see the des-
*. -cription below).  The position  of the plot on the screen  is given by the
*. viewport of the current  normalization transformation  selected (the window
*. is not used and could be anything).
*.
*. _Input parameters:
*.
*.  INTEGER NX      :  Number of cells in X.
*.  INTEGER NY      :  Number of cells in Y.
*.  REAL V(NX,NY)   :  Contains of the cells.
*.  INTEGER NPAR    :  Number of parameters in PAR
*.  REAL PAR(NPAR)  :  Array of real parameter.
*.                     If PAR(i)=0. or NPAR<i a default value is taken
*.  CHARACTER CHOPT :  Options (see bellow).
*.
*. +-------+---------------------------------------------------------+---------+
*. | CHOPT |                Corresponding values of PAR              | Default |
*. +-------+---------------------------------------------------------+---------+
*. |  'P'  | Points (scatter plot)                                   |         |
*. |       |   PAR(1)  = Marker type                                 |   1.    |
*. |       |   PAR(2)  = Maximum number of random points per cell    |   50.   |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'B'  | Boxes                                                   |         |
*. |       |   PAR(1)  = ...                                         |   ...   |
*. |       |   PAR(2)  = ...                                         |   ...   |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'R'  | aRrows                                                  |         |
*. |       |   PAR(1)  = ...                                         |   ...   |
*. |       |   PAR(2)  = ...                                         |   ...   |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'C'  | Contour plot                                            |         |
*. |       |   PAR(1)  = Nlevel (min=2 max=50)                       |   20.   |
*. |       |   PAR(2)  = 0 use colour to distinguish contours        |   0.    |
*. |       |               the line type use is 1.                   |         |
*. |       |             1.XXX use line style to distinguish contours|         |
*. |       |               The color index used is XXX.              |         |
*. |       |             2.XXX line style and colour are the same for|         |
*. |       |               all contours. The color index used is XXX |         |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. |       |   PAR(11) =   --+                                       |         |
*. |       |     .           |                                       |         |
*. |       |     .           +-> Levels to be drawn                  |         |
*. |       |     .           |                                       |         |
*. |       |   PAR(NPAR) = --+                                       |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'COL' | COLour plot                                             |         |
*. |       |   PAR(1)  = 0 use the standard 8 colours                |   0.    |
*. |       |             1 use versacolor pattern table              |         |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'T'  | Table (Text)                                            |         |
*. |       |   PAR(1)  = Text size (in % of the bin Y width)         |   0.3   |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'K'  | Character, the contains is one single character         |         |
*. |       |   PAR(1)  = Text size (in % of the bin Y width)         |   0.3   |
*. |       |   PAR(2)  = ...                                         |         |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label                   |   IYMIN |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'L'  | Lego (mode 0)                                           |         |
*. |  'LB' | Lego same as L but take care with BARO and BARW         |         |
*. |  'L1' | Lego with colors (mode 1)                               |         |
*. |  'L2' | Lego with colors (mode 2)                               |         |
*. |       |   PAR(1)  = THETA                                       |   30.   |
*. |       |   PAR(2)  = PHI                                         |   30.   |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label. For 1D histos    |   IYMIN |
*. |       |             PAR(5) is the HMAX of HPLOT                 |         |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |  'S'  | Surface (mode 0)                                        |         |
*. |  'S1' | Surface with colors (mode 1)                            |         |
*. |  'S2' | Surface with colors (mode 2)                            |         |
*. |  'S3' | Surface with line and color contour on top              |         |
*. |  'S4' | Surface with Gouraud shading                            |         |
*. |       |   PAR(1)  = THETA                                       |   30.   |
*. |       |   PAR(2)  = PHI                                         |   30.   |
*. |       |   PAR(3)  = XMIN Minimal X-axis label                   |   IXMIN |
*. |       |   PAR(4)  = XMAX Maximal Y-axis label                   |   IXMAX |
*. |       |   PAR(5)  = YMIN Minimal Y-axis label. For 1D histos    |   IYMIN |
*. |       |             PAR(5) is the HMAX of HPLOT                 |         |
*. |       |   PAR(6)  = YMAX Maximal Y-axis label                   |   IYMAX |
*. |       |   PAR(7)  = ZMIN Minimal Z value                        |   ZMIN  |
*. |       |   PAR(8)  = ZMAX Maximal Z value                        |   ZMAX  |
*. |       |   PAR(9)  = 1000*IXMIN + IXMAX (Usefull for ZOOM)       |   1-NX  |
*. |       |   PAR(10) = 1000*IYMIN + IYMAX (Usefull for ZOOM)       |   1-NY  |
*. +-------+---------------------------------------------------------+---------+
*. |       |   For legos and surfaces :                              |         |
*. |       |   PAR(11) = NDVX if = 0 the 3D axis are not drawn       |  510.   |
*. |       |                  This is used in HPLTAB to draw the     |         |
*. |       |                  contour plot with fill area.           |         |
*. |       |   PAR(12) = NDVY                                        |  510.   |
*. |       |   PAR(13) = NDVZ                                        |  510.   |
*. |       |   PAR(14) = XCOL                                        |    1.   |
*. |       |   PAR(15) = YCOL                                        |    1.   |
*. |       |   PAR(16) = ZCOL if > 1000 the box is filled with ZCOL  |    1.   |
*. |       |   PAR(17) = XTIC                                        |  0.02   |
*. |       |   PAR(18) = YTIC                                        |  0.02   |
*. |       |   PAR(19) = ZTIC                                        |  0.02   |
*. |       |   PAR(20) = VSIZ                                        |  0.02   |
*. |       |   PAR(21) = VFON                                        |    2.   |
*. |       |   PAR(22) = XVAL                                        |  0.02   |
*. |       |   PAR(23) = YVAL                                        |  0.02   |
*. |       |   PAR(24) = ZVAL                                        |  0.04   |
*. |       |   PAR(25) = Color                                       |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'POL' | Polar for LEGO and SURFACE                              |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'CYL' | Cylindrical for LEGO and SURFACE                        |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'SPH' | Spherical for LEGO and SURFACE                          |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'PSD' | Pseudo rapidity for LEGO and SURFACE                    |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'H'  | Data are compacted like in HPLOT. In this case, V is    |         |
*. |       | not the matrix to be drawn but V(1) contains the HBOOK  |         |
*. |       | ZEBRA link to the histogram to be drawn.                |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'I'  | IGTABL is called from IZDNB. In this case, V is not the |         |
*. |       | not the matrix to be drawn but V(1) contains the HIGZ   |         |
*. |       | adress in the picture data structure.                   |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'GX' | loG on X coordinates. A log WC should be defined before |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'GY' | loG on Y coordinates. A log WC should be defined before |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'GZ' | loG on Z coordinates                                    |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'A'  | 2nd vertical axis (legos and Surfaces only)             |         |
*. |       | axis (for the 2D representations)                       |         |
*. +-------+---------------------------------------------------------+---------+
*. |  '+'  | For stacked histogram (legos)                           |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'Z'  | Allows to display the Z scale                           |         |
*. +-------+---------------------------------------------------------+---------+
*. |  'E'  | Draw the errors (with colors)                           |         |
*. +-------+---------------------------------------------------------+---------+
*. | 'FB'  | With LEGO or SURFACE, it suppress the Front-Box         |         |
*. | 'BB'  | With LEGO or SURFACE, it suppress the Back-Box          |         |
*. +-------+---------------------------------------------------------+---------+
*.
*.  Remarks:
*.          If PAR(9)  < 0, IXMIN=-PAR(9)  and IXMAX=IQUEST(60)
*.          If PAR(10) < 0, IYMIN=-PAR(10) and IYMAX=IQUEST(61)
*.          For 1-Dim histograms, PAR(5) is the Y-Scale factor (HMAX of HPLOT)
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hipack.inc"
#include "higz/hicont.inc"
#include "higz/hihid.inc"
#include "higz/hilut.inc"

      PARAMETER       (YDIFF=1.,YLIGH1=10.)
      PARAMETER       (QA=0.15,QD=0.15,QS=0.8,NQS=1)

      EXTERNAL IHDFL1,IHDFR1,IHDFL2,IHDFR2
      EXTERNAL IHDF02,IHDF03,IGTAB2,IGTAB3,IGTAB6
      REAL IGCELL

      CHARACTER*(*) CHOPTI
      DIMENSION PAR(*),V(*)

      PARAMETER (MAXCOL=8,MAXPAT=39)
      PARAMETER (NMAX=500)
      PARAMETER (NRASTX=1000,NRASTY=800)

      DIMENSION XBUF(NMAX),YBUF(NMAX)
      DIMENSION X(4),Y(4),ZC(4),IR(4),XARR(102),YARR(102),ITARR(102)
      DIMENSION LUT(MAXCOL),IPATRN(MAXPAT)
      DIMENSION IOPT(30),IOPT2(16)
      DIMENSION RVAL(4)

      EQUIVALENCE (IOPCOL,IOPT( 1))
      EQUIVALENCE (IOPTP ,IOPT( 2)),(IOPTB ,IOPT( 3))
      EQUIVALENCE (IOPTC ,IOPT( 4)),(IOPTT ,IOPT( 5))
      EQUIVALENCE (IOPTK ,IOPT( 6)),(IOPTL ,IOPT( 7))
      EQUIVALENCE (IOPTS ,IOPT( 8)),(IOPTH ,IOPT( 9))
      EQUIVALENCE (IOPTA ,IOPT(10)),(IOPTPP,IOPT(11))
      EQUIVALENCE (IOPTR ,IOPT(12)),(IOPTS1,IOPT(13))
      EQUIVALENCE (IOPTS2,IOPT(14)),(IOPTL1,IOPT(15))
      EQUIVALENCE (IOPTL2,IOPT(16)),(IOPTGZ,IOPT(17))
      EQUIVALENCE (IOPTGX,IOPT(18)),(IOPTGY,IOPT(19))
      EQUIVALENCE (IOPTBR,IOPT(20)),(IOPTZ ,IOPT(21))
      EQUIVALENCE (IOPTS3,IOPT(22)),(IOPTS4,IOPT(23))
      EQUIVALENCE (IOPPOL,IOPT(24)),(IOPCYL,IOPT(25))
      EQUIVALENCE (IOPSPH,IOPT(26)),(IOPPSD,IOPT(27))
      EQUIVALENCE (IOPTBB,IOPT(28)),(IOPTFB,IOPT(29))
      EQUIVALENCE (IOPTE ,IOPT(30))

      LOGICAL LTEST,ZFSAV

      CHARACTER*80 CHOPT
      CHARACTER*12 CHAT
      CHARACTER*40 CHK

      DIMENSION RSTRS(MAXSTK),RSTGS(MAXSTK),RSTBS(MAXSTK)
      SAVE RSTRS,RSTGS,RSTBS,NBCSAV
      SAVE RSTRBS,RSTGBS,RSTBBS
      SAVE RSTRTS,RSTGTS,RSTBTS
      DATA NBCSAV /0/

      DATA LUT /0,5,7,3,6,2,4,1/
      DATA IPATRN /20,28,36,44,52,64,48,64,72,82,
     +            89,91,95,98,104,105,112,116,126,
     +            131,147,156,159,160,168,174,175,
     +            178,182,184,199,215,221,227,228,
     +            230,235,246,236/
*.______________________________________
*
      IQUEST(1)=0
      IF(NX.LE.0.OR.NY.LT.0)THEN
         CALL IGERR('Invalid array dimension','IGTABL')
         RETURN
      ENDIF
*
*             Determine options
*
      CHOPT=CHOPTI
      IOPCOL=INDEX(CHOPT,'COL')
      IF(IOPCOL.GT.0)CHOPT(IOPCOL:IOPCOL+2)='   '
      IOPPOL=INDEX(CHOPT,'POL')
      IF(IOPPOL.GT.0)CHOPT(IOPPOL:IOPPOL+2)='   '
      IOPCYL=INDEX(CHOPT,'CYL')
      IF(IOPCYL.GT.0)CHOPT(IOPCYL:IOPCYL+2)='   '
      IOPSPH=INDEX(CHOPT,'SPH')
      IF(IOPSPH.GT.0)CHOPT(IOPSPH:IOPSPH+2)='   '
      IOPPSD=INDEX(CHOPT,'PSD')
      IF(IOPPSD.GT.0)CHOPT(IOPPSD:IOPPSD+2)='   '
      IOPTFB=INDEX(CHOPT,'FB')
      IF(IOPTFB.GT.0)CHOPT(IOPTFB:IOPTFB+1)='  '
      IOPTBB=INDEX(CHOPT,'BB')
      IF(IOPTBB.GT.0)CHOPT(IOPTBB:IOPTBB+1)='  '
      IOPTBR=INDEX(CHOPT,'LB')
      IF(IOPTBR.GT.0)CHOPT(IOPTBR:IOPTBR+1)=' L'
      IOPTS1=INDEX(CHOPT,'S1')
      IF(IOPTS1.GT.0)CHOPT(IOPTS1:IOPTS1+1)='  '
      IOPTS2=INDEX(CHOPT,'S2')
      IF(IOPTS2.GT.0)CHOPT(IOPTS2:IOPTS2+1)='  '
      IOPTS3=INDEX(CHOPT,'S3')
      IF(IOPTS3.GT.0)CHOPT(IOPTS3:IOPTS3+1)='  '
      IOPTS4=INDEX(CHOPT,'S4')
      IF(IOPTS4.GT.0)CHOPT(IOPTS4:IOPTS4+1)='  '
      IOPTL1=INDEX(CHOPT,'L1')
      IF(IOPTL1.GT.0)CHOPT(IOPTL1:IOPTL1+1)='  '
      IOPTL2=INDEX(CHOPT,'L2')
      IF(IOPTL2.GT.0)CHOPT(IOPTL2:IOPTL2+1)='  '
      IOPTGZ=INDEX(CHOPT,'GZ')
      IF(IOPTGZ.GT.0)CHOPT(IOPTGZ:IOPTGZ+1)='  '
      IOPTGX=INDEX(CHOPT,'GX')
      IF(IOPTGX.GT.0)CHOPT(IOPTGX:IOPTGX+1)='  '
      IOPTGY=INDEX(CHOPT,'GY')
      IF(IOPTGY.GT.0)CHOPT(IOPTGY:IOPTGY+1)='  '
      IOPTZ=INDEX(CHOPT,'Z')
      IF(IOPTZ.GT.0)CHOPT(IOPTZ:IOPTZ)=' '
      IOPTI=INDEX(CHOPT,'I')
      IF(IOPTI.GT.0)CHOPT(IOPTI:IOPTI)=' '
      IOPTE=INDEX(CHOPT,'E')
      IOPTER=IOPTE
      IF(IOPTE.GT.0)CHOPT(IOPTE:IOPTE)=' '
      CALL UOPTC(CHOPT,'PBCTKLSHA+R',IOPT(2))
      ILEGO = IOPTL+IOPTL1+IOPTL2
      ISURF = IOPTS+IOPTS1+IOPTS2+IOPTS3+IOPTS4
      I3D   = ILEGO+ISURF
      IRAST = 0
*
*              Try to find if a drawing option has been selected
*              if not the option 'P' is selected.
*
      CALL UOPTC(CHOPT,'PBCTKLSR',IOPT2(2))
      IOPT2(1) =IOPCOL
      IOPT2(10)=IOPTS1
      IOPT2(11)=IOPTS2
      IOPT2(12)=IOPTL1
      IOPT2(13)=IOPTL2
      IOPT2(14)=IOPTS3
      IOPT2(15)=IOPTS4
      IOPT2(16)=IOPTE
      DO 10 I=1,16
         IF(IOPT2(I).NE.0)GOTO 20
   10 CONTINUE
      IOPTP=1
   20 CONTINUE
*
*             Initialise the COMMON HIPACK (used by IGCELL)
*
      NCX = NX
      NCY = NY
      IF(IOPTH.NE.0)THEN
         IPACK = 1
      ELSEIF(IOPTI.NE.0)THEN
         IPACK = 2
      ELSE
         IPACK = 0
      ENDIF
      IF(NCY.EQ.0)THEN
         IDIM1 = 1
         NCY   = 1
      ELSE
         IDIM1 = 0
      ENDIF
      ILOG  = 0
      ZLOW  = 0.
      ZHIGH = 0.
      IF(NPAR.GE.8)THEN
         ZLOW  = PAR(7)
         ZHIGH = PAR(8)
      ENDIF
*
      I4AXI = IOPTA
      IF(IOPTBR.NE.0)THEN
         RLEGBO = RBOF
         RLEGBW = RBWD
      ELSE
         RLEGBO = 0.
         RLEGBW = 1.
      ENDIF
#if defined(CERNLIB_MAIL)
*
*              Z part and MAIL part
*
      CHOPTM=CHOPTI
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZTABL(NX,NY,V,NPAR,PAR,IOPT)
#endif
      IF((.NOT.GFLAG).AND.(.NOT.PFLAG))RETURN
      ZFSAV=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Initialise the LOGX and LOGY option for 3D representations
*
      IF(I3D.NE.0)THEN
         IF(IOPTGX.NE.0)THEN
            ILOGX  = IOPTGX
            IOPTGX = 0
         ELSE
            ILOGX  = 0
         ENDIF
         IF(IOPTGY.NE.0)THEN
            ILOGY  = IOPTGY
            IOPTGY = 0
         ELSE
            ILOGY  = 0
         ENDIF
      ENDIF
*
*             Save the current environment and select
*             a logarithmic or normalized working space.
*
      CALL IZSAV
      X(1)=RVXMIN
      X(2)=RVXMAX
      Y(1)=RVYMIN
      Y(2)=RVYMAX
      IF(IOPTGX.NE.0)THEN
         X(1)=RWXMIN
         X(2)=RWXMAX
      ENDIF
      IF(IOPTGY.NE.0)THEN
         Y(1)=RWYMIN
         Y(2)=RWYMAX
      ENDIF
      CALL ISWN(INTR,X(1),X(2),Y(1),Y(2))
      CALL ISELNT(INTR)
*.______________________________________
*
*              Compute some usefull parameters
*
*   IXMIN = first chanel in X
*   IXMAX = last chanel in X
*   IYMIN = first chanel in Y
*   IYMAX = last chanel in Y
*   NXCHA = number of chanel in X
*   NYCHA = number of chanel in Y
*   XSTP  = step in X
*   YSTP  = step in Y
*   ZMIN  = minimum of the array V(IXMIN:IXMAX,IYMIN:IYMAX)
*   ZMAX  = maximum of the array V(IXMIN:IXMAX,IYMIN:IYMAX)
*   DZ    = difference between ZMAX and ZMIN
*   HMAX  = Margin on top of 1D histos (% of ZMAX)
*   RAD   = Conversion from degrees to radians
*
      IXMIN = 1
      IXMAX = NX
      IF(NPAR.GE.9)THEN
         IF(PAR(9).GT.0.)THEN
            IXMIN = INT(PAR(9)/1000)
            IXMAX = INT(PAR(9)-(1000*IXMIN))
         ELSEIF(PAR(9).LT.0.)THEN
            IXMIN = -PAR(9)
            IXMAX = IQUEST(60)
         ENDIF
      ENDIF
      IF(IDIM1.NE.0)THEN
         IYMIN = 0
         IYMAX = 0
      ELSE
         IYMIN = 1
         IYMAX = NY
         IF(NPAR.GE.10)THEN
            IF(PAR(10).GT.0.)THEN
               IYMIN = INT(PAR(10)/1000)
               IYMAX = INT(PAR(10)-(1000*IYMIN))
            ELSEIF(PAR(10).LT.0.)THEN
               IYMIN = -PAR(10)
               IYMAX = IQUEST(61)
            ENDIF
         ENDIF
      ENDIF
*
* In case of surfaces at least 2 channels are needed
*
      IF(ISURF.NE.0)THEN
         IF(IXMAX.EQ.IXMIN)THEN
            IXMAX=IXMAX+1
            IF(IXMAX.GT.NX)THEN
               IXMAX=NX
               IXMIN=IXMIN-1
            ENDIF
         ENDIF
         IF(IYMAX.EQ.IYMIN)THEN
            IYMAX=IYMAX+1
            IF(IYMAX.GT.NY)THEN
               IYMAX=NY
               IYMIN=IYMIN-1
            ENDIF
         ENDIF
      ENDIF
*
      NXCHA=IXMAX-IXMIN+1
      NYCHA=IYMAX-IYMIN+1
      IF(IXMIN.GT.IXMAX)THEN
         CALL IGERR('PAR(9) is not valid','IGTABL')
         GOTO 450
      ENDIF
      IF(IYMIN.GT.IYMAX.AND.NY.GT.0)THEN
         CALL IGERR('PAR(10) is not valid','IGTABL')
         GOTO 450
      ENDIF
      IF(IOPTGX.NE.0)THEN
         XSTP=(10**RWXMAX-10**RWXMIN)/FLOAT(IXMAX-IXMIN+1)
      ELSE
         XSTP=(RWXMAX-RWXMIN)/FLOAT(IXMAX-IXMIN+1)
      ENDIF
      IF(IOPTGY.NE.0)THEN
         YSTP=(10**RWYMAX-10**RWYMIN)/FLOAT(IYMAX-IYMIN+1)
      ELSE
         YSTP=(RWYMAX-RWYMIN)/FLOAT(IYMAX-IYMIN+1)
      ENDIF
*
* Compute ZMAX and ZMIN
*
      IF(ZHIGH.NE.ZLOW)THEN
         ZMAX=ZHIGH
         ZMIN=ZLOW
      ELSE
         ZMIN=IGCELL(NX,NY,V,IXMIN,IYMIN,1)
         ZMAX=ZMIN
         DO 40 J=IYMIN,IYMAX
            DO 30 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)
               IF(IOPTGZ.NE.0)THEN
                  IF(Z.GT.0)THEN
                     IF(ZMIN.LE.0)THEN
                        ZMIN = Z
                     ELSE
                        ZMIN=MIN(ZMIN,Z)
                     ENDIF
                  ENDIF
               ELSE
                  ZMIN=MIN(ZMIN,Z)
               ENDIF
               ZMAX=MAX(ZMAX,Z)
   30       CONTINUE
   40    CONTINUE
      ENDIF
      IF(IOPTGZ.NE.0)THEN
         IF(ZMAX.GT.0.)THEN
            IF(ZMIN.LE.0.)ZMIN=0.001*ZMAX
            ZLOW=ZMIN
            ZHIGH=ZMAX
            ZMAX=LOG10(ZMAX)
            ZMIN=LOG10(ZMIN)
         ELSE
            CALL IGERR('Negative logarithmic axis','IGTABL')
            GOTO 450
         ENDIF
      ENDIF
      IF(ZMAX.EQ.ZMIN)THEN
         EPS=ABS(ZMAX*0.1)
         IF(EPS.EQ.0)EPS=0.1
         ZMAX=ZMAX+EPS
         ZMIN=ZMIN-EPS
      ENDIF
      DZ=ZMAX-ZMIN
      IF(DZ.LE.0.)THEN
         CALL IGERR('ZMAX < ZMIN','IGTABL')
         GOTO 450
      ENDIF
      RQUEST(11) = ZMIN
      RQUEST(12) = ZMAX
      ILOG  = IOPTGZ
*
      XK=RWXMIN
      YK=RWYMIN
      IF(IOPTGX.NE.0)XK=10**RWXMIN
      IF(IOPTGY.NE.0)YK=10**RWYMIN
*
      HMAX=1.
      IF(IDIM1.NE.0)THEN
         HMAX=1.1
         IF(NPAR.GE.5)THEN
            IF(PAR(5).NE.0.)HMAX=1./PAR(5)
         ENDIF
      ENDIF
*
      RAD = ATAN(1.)*4./180.
*.______________________________________
*
*             Store the stacked histograms parameters
*
      IF(IOPTPP.NE.0)THEN
         NIDS=NIDS+1
         IF(NIDS.EQ.1)THEN
            XLAB1=FLOAT(IXMIN)
            XLAB2=FLOAT(IXMAX)
            YLAB1=FLOAT(IYMIN)
            YLAB2=FLOAT(IYMAX)
            IF(NPAR.GE.6)THEN
               IF(PAR(4).GT.PAR(3))THEN
                  XLAB1=PAR(3)
                  XLAB2=PAR(4)
               ENDIF
               IF(PAR(6).GT.PAR(5))THEN
                  YLAB1=PAR(5)
                  YLAB2=PAR(6)
               ENDIF
            ENDIF
         ENDIF
         IF(NIDS.EQ.10)THEN
            CALL IGERR('Too many stacked histograms','IGTABL')
            NIDS=9
            GOTO 450
         ENDIF
         IF(IPACK.NE.0)THEN
            LSTACK(NIDS)=INT(V(1))
         ELSE
            LSTACK(NIDS)=LOCF(V(1))
         ENDIF
         IXFCHA(NIDS)=IXMIN
         IYFCHA(NIDS)=IYMIN
         IXNCHA(NIDS)=NXCHA
         IYNCHA(NIDS)=NYCHA
         IF(NPAR.GE.25)THEN
            ISTCOL(NIDS)=PAR(25)
         ELSE
            ISTCOL(NIDS)=NIDS
         ENDIF
         GOTO 450
      ENDIF
*.______________________________________
*
*              Draw the table with a density of points (scatter plot)
*
      IF(IOPTP.NE.0)THEN
         FACN=1.
         LTEST=.FALSE.
         IF(ZMAX.GE.50..OR.ZMAX.LT.1.)THEN
            FACN=49./DZ
            LTEST=.TRUE.
         ENDIF
         IPMNUM=0
         DO 80 J=IYMIN,IYMAX
            DO 70 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)-ZMIN
               IF(Z.EQ.0.)GOTO 60
               K=INT(Z*FACN)
               IF(LTEST)K=K+1
               IF(K.GT.0)THEN
                  IF((K+IPMNUM).GE.NMAX)THEN
                     CALL IPM(IPMNUM,XBUF,YBUF)
                     IPMNUM=0
                  ENDIF
                  DO 50 LOOP=1,K
                     XBUF(IPMNUM+LOOP)=(RNDM(LOOP)*XSTP)+XK
                     IF(IOPTGX.NE.0)XBUF(IPMNUM+LOOP)=
     +                              LOG10(XBUF(IPMNUM+LOOP))
                     YBUF(IPMNUM+LOOP)=(RNDM(LOOP)*YSTP)+YK
                     IF(IOPTGY.NE.0)YBUF(IPMNUM+LOOP)=
     +                              LOG10(YBUF(IPMNUM+LOOP))
   50             CONTINUE
                  IPMNUM=IPMNUM+K
               ENDIF
   60          XK=XK+XSTP
   70       CONTINUE
            IF(IOPTGX.NE.0)THEN
               XK=10**RWXMIN
            ELSE
               XK=RWXMIN
            ENDIF
            YK=YK+YSTP
   80    CONTINUE
         IF(IPMNUM.GT.0)CALL IPM(IPMNUM,XBUF,YBUF)
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table with proportional boxes
*
      IF(IOPTB.NE.0)THEN
         XMID=XSTP/2.
         YMID=YSTP/2.
         DO 100 J=IYMIN,IYMAX
            DO 90 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)
               IF(Z.NE.ZMIN)THEN
                  ZRATIO = SQRT((Z-ZMIN)/DZ)
                  XU=(XMID*ZRATIO)+(XK+XMID)
                  XL=2*(XK+XMID)-XU
                  IF(IOPTGX.NE.0)THEN
                     XU=LOG10(XU)
                     XL=LOG10(XL)
                  ENDIF
                  YU=(YMID*ZRATIO)+(YK+YMID)
                  YL=2*(YK+YMID)-YU
                  IF(IOPTGY.NE.0)THEN
                     YU=LOG10(YU)
                     YL=LOG10(YL)
                  ENDIF
                  CALL IGBOX(XL,XU,YL,YU)
               ENDIF
               XK=XK+XSTP
   90       CONTINUE
            IF(IOPTGX.NE.0)THEN
               XK=10**RWXMIN
            ELSE
               XK=RWXMIN
            ENDIF
            YK=YK+YSTP
  100    CONTINUE
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table with arrows
*
      IF(IOPTR.NE.0)THEN
         XRG=RVXMIN
         YRG=RVYMIN
         XLN=RVXMAX-RVXMIN
         YLN=RVYMAX-RVYMIN
         CX=(XLN/NXCHA-0.03)/2.
         CY=(YLN/NYCHA-0.03)/2.
         DN=1.E-30
         CALL ISLN(1)
         DO 130 ID=1,2
            DO 120 J=IYMIN,IYMAX
               DO 110 I=IXMIN,IXMAX
                  IF(I.EQ.IXMIN)THEN
                     DX=IGCELL(NX,NY,V,I+1,J,1)-IGCELL(NX,NY,V,I,J,1)
                  ELSEIF(I.EQ.IXMAX)THEN
                     DX=IGCELL(NX,NY,V,I,J,1)-IGCELL(NX,NY,V,I-1,J,1)
                  ELSE
                     DX=(IGCELL(NX,NY,V,I+1,J,1)-
     +                   IGCELL(NX,NY,V,I-1,J,1))/2
                  ENDIF
                  IF(J.EQ.IYMIN)THEN
                     DY=IGCELL(NX,NY,V,I,J+1,1)-IGCELL(NX,NY,V,I,J,1)
                  ELSEIF(J.EQ.IYMAX)THEN
                     DY=IGCELL(NX,NY,V,I,J,1)-IGCELL(NX,NY,V,I,J-1,1)
                  ELSE
                     DY=(IGCELL(NX,NY,V,I,J+1,1)-
     +                   IGCELL(NX,NY,V,I,J-1,1))/2
                  ENDIF
                  IF(ID.EQ.1)THEN
                     DN=MAX(DN,ABS(DX),ABS(DY))
                  ELSEIF(ID.EQ.2)THEN
                     XC=XRG+XLN*(FLOAT(I-IXMIN+1)-0.5)/NXCHA
                     DXN=CX*DX/DN
                     X1=XC-DXN
                     X2=XC+DXN
                     YC=YRG+YLN*(FLOAT(J-IYMIN+1)-0.5)/NYCHA
                     DYN=CY*DY/DN
                     Y1=YC-DYN
                     Y2=YC+DYN
                     XBUF(1)=X1
                     XBUF(2)=X2
                     YBUF(1)=Y1
                     YBUF(2)=Y2
                     IF(ABS(X2-X1).GT.1.E-2.OR.ABS(Y2-Y1).GT.1.E-2)THEN
                        ANR=0.005*.5*SQRT(2/(DXN**2+DYN**2))
                        SI=ANR*(DXN+DYN)
                        CO=ANR*(DXN-DYN)
                        XBUF(3)=X2-SI
                        YBUF(3)=Y2+CO
                        CALL IPL(3,XBUF,YBUF)
                        XBUF(1)=X2
                        XBUF(2)=X2-CO
                        YBUF(1)=Y2
                        YBUF(2)=Y2-SI
                        CALL IPL(2,XBUF,YBUF)
                     ELSE
                        CALL IPL(2,XBUF,YBUF)
                     ENDIF
                  ENDIF
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table with color levels
*
      IF(IOPCOL.NE.0)THEN
         CALL ISFAIS(1)
         MAXCP=MAXCOL
         IF(INBCOL.GT.8)MAXCP=INBCOL-7
         IF(NPAR.GE.1)THEN
            IF(PAR(1).EQ.1.)THEN
               CALL ISFAIS(2)
               MAXCP=MAXPAT
            ENDIF
         ENDIF
         CALL IGSET('BORD',0.)
         DO 150 J=IYMIN,IYMAX
            DO 140 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)
               ICOL=INT(MAXCP*(Z-ZMIN)/DZ)
               IF(IFAIS.EQ.2)THEN
                  CALL ISFASI(IPATRN(ICOL))
               ELSE
                  IF(ICOL.GT.MAXCP-1)ICOL=MAXCP-1
                  IF(INBCOL.GT.8)THEN
                     CALL ISFACI(ICOL+8)
                  ELSE
                     CALL ISFACI(LUT(ICOL+1))
                  ENDIF
               ENDIF
               XU=XK+XSTP
               XL=XK
               IF(IOPTGX.NE.0)THEN
                  XU=LOG10(XU)
                  XL=LOG10(XL)
               ENDIF
               YU=YK+YSTP
               YL=YK
               IF(IOPTGY.NE.0)THEN
                  YU=LOG10(YU)
                  YL=LOG10(YL)
               ENDIF
               CALL IGBOX(XL,XU,YL,YU)
               XK=XK+XSTP
  140       CONTINUE
            IF(IOPTGX.NE.0)THEN
               XK=10**RWXMIN
            ELSE
               XK=RWXMIN
            ENDIF
            YK=YK+YSTP
  150    CONTINUE
         CALL IGSET('BORD',1.)
         CALL ISFAIS(0)
         CALL ISPLCI(1)
         CALL IGBOX(RWXMIN,RWXMAX,RWYMIN,RWYMAX)
         IF(IOPTZ.NE.0)THEN
            XX=0.01*(RWXMAX-RWXMIN)
            IF(INBCOL.GT.8)THEN
               IC1=8
               IC2=INBCOL
            ELSE
               IC1=0
               IC2=7
            ENDIF
            CALL ISCLIP(0)
            IF(IOPTGZ.NE.0)THEN
               CALL IGCOLM(RWXMAX+XX,RWXMAX+4.*XX,RWYMIN,RWYMAX
     +,                    IC1,IC2,10.**ZMIN,10.**ZMAX,'CAG')
            ELSE
               CALL IGCOLM(RWXMAX+XX,RWXMAX+4.*XX,RWYMIN,RWYMAX
     +,                    IC1,IC2,ZMIN,ZMAX,'CA')
            ENDIF
         ENDIF
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table as a contour plot
*
      IF(IOPTC.NE.0)THEN
         NCONT  = 20
         ILNSAV = ILN
         CALL ISLN(1)
         IF(NPAR.GE.1)THEN
            IF(PAR(1).GE.2..AND.PAR(1).LE.50.)NCONT=INT(PAR(1))
         ENDIF
         ITYPE = 0
         ICOL  = 1
         IF(NPAR.GE.2)THEN
            IF(PAR(2).GE.1..AND.PAR(2).LT.3.)THEN
               ITYPE = INT(PAR(2))
               ICOL  = INT((PAR(2)-ITYPE)*1000.+0.5)
               IF(ICOL.LE.0)ICOL=1
               CALL ISPLCI(ICOL)
               IF(ITYPE.EQ.2)CALL ISLN(ILNSAV)
            ENDIF
         ENDIF
         IF(IXMAX.EQ.IXMIN.OR.IYMAX.EQ.IYMIN)THEN
            CALL IGERR('At least 2 channels are needed in contour',
     +      'IGTABL')
            GOTO 450
         ENDIF
         XSTP=(RWXMAX-RWXMIN)/FLOAT(IXMAX-IXMIN+1)
         YSTP=(RWYMAX-RWYMIN)/FLOAT(IYMAX-IYMIN+1)
         IEND=1
         IF(NPAR.GT.10)THEN
            IEND=NPAR-10
            NCONT=12
            ZMINS=ZMIN
            ZMAXS=ZMAX
         ENDIF
         DO 240 ICONT=1,IEND
            IF(NPAR.GT.10)THEN
               IF(PAR(ICONT+10).GT.ZMAXS.OR.PAR(ICONT+10).LT.ZMINS)THEN
                  CALL IGERR('Z value outside limits','IGTABL')
                  GOTO 450
               ENDIF
               ZMAX=ZMAXS
               ZMIN=PAR(ICONT+10)-(ZMAXS-PAR(ICONT+10))
               IF(ZMIN.LT.ZMINS)THEN
                  ZMIN=ZMINS
                  ZMAX=PAR(ICONT+10)+(PAR(ICONT+10)-ZMINS)
               ENDIF
               ZHIGH=ZMAX
               ZLOW=ZMIN
               DZ=ZMAX-ZMIN
               IF (DZ.EQ.0) THEN
                  CALL IGERR('ZMIN = ZMAX ','IGTABL')
                  GOTO 450
               ENDIF
            ENDIF
            DO 230 J=IYMIN,IYMAX-1
               Y(1)=FLOAT(J-IYMIN)*YSTP+RWYMIN+YSTP/2.
               Y(2)=Y(1)
               Y(3)=FLOAT(J-IYMIN+1)*YSTP+RWYMIN+YSTP/2.
               Y(4)=Y(3)
               DO 220 I=IXMIN,IXMAX-1
                  ZC(1)=IGCELL(NX,NY,V,I,J,1)
                  ZC(2)=IGCELL(NX,NY,V,I+1,J,1)
                  ZC(3)=IGCELL(NX,NY,V,I+1,J+1,1)
                  ZC(4)=IGCELL(NX,NY,V,I,J+1,1)
                  IR(1)=INT((ZC(1)-ZMIN)*NCONT/DZ)
                  IR(2)=INT((ZC(2)-ZMIN)*NCONT/DZ)
                  IR(3)=INT((ZC(3)-ZMIN)*NCONT/DZ)
                  IR(4)=INT((ZC(4)-ZMIN)*NCONT/DZ)
                  IF(IR(1).NE.IR(2).OR.IR(2).NE.IR(3).OR.
     +               IR(3).NE.IR(4).OR.IR(4).NE.IR(1))THEN
                     X(1)=FLOAT(I-IXMIN)*XSTP+RWXMIN+XSTP/2.
                     X(4)=X(1)
                     X(2)=FLOAT(I-IXMIN+1)*XSTP+RWXMIN+XSTP/2.
                     X(3)=X(2)
                     N=LVMIN(ZC,4)
                     LJ=1
                     DO 160 IX=1,4
                        M=MOD(N,4)+1
                        LJ=LJ+2*IGTAB1(ZC(N),IR(N),X(N),Y(N),ZC(M),
     +                  IR(M),X(M),Y(M),XARR(LJ),YARR(LJ),ITARR(LJ),
     +                  NCONT)
                        N=M
  160                CONTINUE
                     N=LVMIN(ZC,4)
                     LJ=2
                     DO 170 IX=1,4
                        IF(N.EQ.1)THEN
                           M=4
                        ELSE
                           M=N-1
                        ENDIF
                        LJ=LJ+2*IGTAB1(ZC(N),IR(N),X(N),Y(N),ZC(M),
     +                  IR(M),X(M),Y(M),XARR(LJ),YARR(LJ),ITARR(LJ),
     +                  NCONT)
                        N=M
  170                CONTINUE
*
* Re-order endpoints
*
                     DO 200 IX=1,LJ-5,2
  180                   IF(ITARR(IX).NE.ITARR(IX+1))THEN
                           XSAVE=XARR(IX+1)
                           YSAVE=YARR(IX+1)
                           ITARS=ITARR(IX+1)
                           DO 190 JX=IX,LJ-5,2
                              XARR(JX+1)=XARR(JX+3)
                              YARR(JX+1)=YARR(JX+3)
                              ITARR(JX+1)=ITARR(JX+3)
  190                      CONTINUE
                           XARR(LJ-2)=XSAVE
                           YARR(LJ-2)=YSAVE
                           ITARR(LJ-2)=ITARS
                           GOTO 180
                        ENDIF
  200                CONTINUE
*
                     DO 210 IX=1,LJ-2,2
                        IF(NPAR.GT.10)THEN
                           IF(ITARR(IX).NE.6)THEN
                              GOTO 210
                           ELSE
                              MODE=MAX(MOD(ICONT,5),1)
                           ENDIF
                        ELSE
                           ICOL=(4*ITARR(IX))/NCONT+1
                           IF(ITYPE.EQ.0)MODE=ICOL
                           IF(ITYPE.EQ.1)THEN
                              MODE=MOD(ICOL,5)
                              IF(MODE.EQ.0)MODE=5
                           ENDIF
                        ENDIF
                        IF(ITYPE.EQ.0)CALL ISPLCI(MODE)
                        IF(ITYPE.EQ.1)CALL ISLN(MODE)
                        CALL IPL(2,XARR(IX),YARR(IX))
  210                CONTINUE
                  ENDIF
  220          CONTINUE
  230       CONTINUE
  240    CONTINUE
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table with the table content (INTEGER)
*
      IF(IOPTT.NE.0)THEN
         XK=RWXMIN+XSTP/2.
         YK=RWYMIN+YSTP/2.
         XK0=XK
         IF(NPAR.GE.2)THEN
            TSIZ=YSTP*PAR(1)
         ELSE
            TSIZ=YSTP*0.3
         ENDIF
         CALL ISCHH(TSIZ)
         CALL ISTXAL(2,3)
         CALL IGSET('TANG',0.)
         DO 260 J=IYMIN,IYMAX
            DO 250 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)
               IC=INT(Z)
               IF(IC.NE.0)THEN
                  CALL IZITOC(IC,CHAT)
                  CALL ITX(XK,YK,CHAT)
               ENDIF
               XK=XK+XSTP
  250       CONTINUE
            XK=XK0
            YK=YK+YSTP
  260    CONTINUE
         GOTO 440
      ENDIF
*.______________________________________
*
*              Draw the table with one character per cell
*
      IF(IOPTK.NE.0)THEN
         CHK='.+23456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*'
         XK=RWXMIN+XSTP/2.
         YK=RWYMIN+YSTP/2.
         XK0=XK
         IF(NPAR.GE.2)THEN
            TSIZ=YSTP*PAR(1)
         ELSE
            TSIZ=YSTP*0.3
         ENDIF
         CALL ISCHH(TSIZ)
         CALL ISTXAL(2,3)
         CALL IGSET('TANG',0.)
         DO 280 J=IYMIN,IYMAX
            DO 270 I=IXMIN,IXMAX
               Z=IGCELL(NX,NY,V,I,J,1)
               IC=NINT(Z+0.5)
               IF(IC.GT.37)IC=37
               IF(IC.GT.0)CALL ITX(XK,YK,CHK(IC:IC))
               XK=XK+XSTP
  270       CONTINUE
            XK=XK0
            YK=YK+YSTP
  280    CONTINUE
         GOTO 440
      ENDIF
*.______________________________________
*
*              3D representations
*
      IF(I3D.LE.0)GOTO 450
      CALL ISCLIP(0)
*
*              Restore the color map if necessary
*
      IF(NBCSAV.NE.0)THEN
         DO 290 I=1,NBCSAV
            CALL ISCR(1,ISTCO2(I),RSTRS(I),RSTGS(I),RSTBS(I))
  290    CONTINUE
         NBCSAV = 0
      ENDIF
      IF(ISTCOB.NE.0)THEN
         CALL ISCR(1,ISTCOB,RSTRBS,RSTGBS,RSTBBS)
         ISTCOB = 0
      ENDIF
      IF(ISTCOT.NE.0)THEN
         CALL ISCR(1,ISTCOT,RSTRTS,RSTGTS,RSTBTS)
         ISTCOT = 0
      ENDIF
*
*              Define the labels on the axis
*
      IF(NIDS.EQ.0)THEN
         XLAB1 = FLOAT(IXMIN)
         XLAB2 = FLOAT(IXMAX)
         YLAB1 = FLOAT(IYMIN)
         YLAB2 = FLOAT(IYMAX)
         IF(NPAR.GE.6)THEN
            IF(PAR(4).GT.PAR(3))THEN
               XLAB1 = PAR(3)
               XLAB2 = PAR(4)
            ENDIF
            IF(PAR(6).GT.PAR(5))THEN
               YLAB1 = PAR(5)
               YLAB2 = PAR(6)
            ENDIF
         ENDIF
         ZMINST = ZMIN
         ZMAXST = ZMAX
         IF(ILOG.NE.0)THEN
            ZMAXST = 10.**ZMAX
            ZMINST = 10.**ZMIN
         ENDIF
         IF(ILOGX.NE.0)THEN
            IF(XLAB2.GT.0.)THEN
               IF(XLAB1.LE.0.)XLAB1 = MIN(XLAB2/1000.,1.)
            ELSE
               ILOGX = 0
            ENDIF
         ENDIF
         IF(ILOGY.NE.0)THEN
            IF(YLAB2.GT.0.)THEN
               IF(YLAB1.LE.0.)YLAB1 = MIN(YLAB2/1000.,1.)
            ELSE
               ILOGY = 0
            ENDIF
         ENDIF
      ENDIF
*
*              Initialize the axis parameter
*
      XBUF(11) = 510.
      XBUF(12) = 510.
      XBUF(13) = 510.
      XBUF(14) = 1.
      XBUF(15) = 1.
      XBUF(16) = 1.
      XBUF(17) = 0.02
      XBUF(18) = 0.02
      XBUF(19) = 0.02
      XBUF(20) = 0.02
      XBUF(21) = 2.
      XBUF(22) = 0.02
      XBUF(23) = 0.02
      XBUF(24) = 0.04
      IF(NPAR.GT.10)THEN
         DO 300 I=11,NPAR
            XBUF(I) = PAR(I)
  300    CONTINUE
      ENDIF
      XBUF(25) = ILOGX
      XBUF(26) = ILOGY
      XBUF(27) = ILOG
*
*              Define the coordinate system
*
*    ISYS  = 1 : Cratesian Coordinate System
*    ISYS  = 2 : Polar Coordinate System
*    ISYS  = 3 : Cylindrical Coordinate System
*    ISYS  = 4 : Spherical Coordinate System
*    ISYS  = 5 : Pseudo rapidity
*    IRAST = 1 : "Raster screen" algorithm must be used
*                for hidden line drawing
*
      ISYS                = 1
      IF(IOPPOL.NE.0)ISYS = 2
      IF(IOPCYL.NE.0)ISYS = 3
      IF(IOPSPH.NE.0)ISYS = 4
      IF(IOPPSD.NE.0)ISYS = 5
      IF((IOPTL+IOPTS+IOPTS3).NE.0.AND.ISYS.GE.2)IRAST = 1
*
*              Define viewing angles
*
      THEDEG = 60.
      PHIDEG = -120.
      PSIDEG = 0.
      IF(NPAR.GE.2)THEN
         THEDEG =  90.-PAR(1)
         PHIDEG = -90.-PAR(2)
      ENDIF
*
      IDRGR = 1
      IF(NPAR.GE.25)THEN
         ISTCOL(NIDS+1) = PAR(25)
      ELSE
         IF(IOPTL.NE.0)THEN
            ISTCOL(NIDS+1) = 1
         ELSE
            ISTCOL(NIDS+1) = 0
         ENDIF
      ENDIF
*
*              Copy the color table in a INTEGER array. The
*              vector ITARR is used normally in the contour plot
*              algorithm so it can be used here without clashes.
*                If NPAR is less than 25 the LUT is used as the
*              color table.
*
      IF(NPAR.GE.25)THEN
         MAXCP = 0
         DO 310 I=25,NPAR
            IF(I.GT.100)THEN
               CALL IGERR('Color table too big','IGTABL')
               GOTO 450
            ENDIF
            ITARR(I-24) = INT(PAR(I))
            MAXCP       = MAXCP+1
  310    CONTINUE
      ELSE
         MAXCP=MAXCOL
         DO 320 I=1,MAXCOL
            ITARR(I)=LUT(I)
  320    CONTINUE
      ENDIF
*
*              If the errors are required and if one of the color
*              option is required, the colors represent the color,
*              so the minimum and maximum of the errors has to be
*              computed.
*
      IF (IOPTER.NE.0) THEN
         IF (IOPTS3+IOPTS1+IOPTS2+IOPTL2.NE.0) THEN
            Z1C = IGCELL(NX,NY,V,1,1,2)
            Z2C = Z1C
            DO 340 J=IYMIN,IXMAX
               DO 330 I=IXMIN,IXMAX
                  ZE = IGCELL(NX,NY,V,I,J,2)
                  IF (ZE.GT.Z2C) Z2C = ZE
                  IF (ZE.LT.Z1C) Z1C = ZE
  330          CONTINUE
  340       CONTINUE
         ENDIF
      ELSE
         Z1C = ZMIN
         Z2C = ZMAX*HMAX
      ENDIF
      RQUEST(11) = Z1C
      RQUEST(12) = Z2C
*
*              Create a buffer used by the "Raster screen" algorithm
*
      IF(IRAST.NE.0)THEN
         IF(IPACK.NE.0)LWV=INT(V(1))
         NWNEED = (NRASTX*NRASTY)/30+1
         CALL MZNEED(IXHIGZ,NWNEED+25,'G')
         IF (IQUEST(11).LT.0) THEN
            CALL IGERR('IGTABL','Not enough space in memory')
            GOTO 450
         ENDIF
         IF(LCG.NE.0)CALL MZDROP(IXHIGZ,LCG,' ')
         CALL MZBOOK(IXHIGZ,LCG,LCG,1,'TEMP',0,0,NWNEED,3,0)
         IF(IPACK.NE.0)V(1)=FLOAT(LWV)
      ENDIF
*
*              Initialise the adress NIDS+1 for IGCELL
*
      IF(IPACK.NE.0)THEN
         REFWRD(NIDS+1)=V(1)
      ELSE
         LSTACK(NIDS+1)=LOCF(V(1))
         IADRES(NIDS+1)=LSTACK(NIDS+1)-LOCF(REFWRD)
      ENDIF
*
* XSTP   = X step
* XSTP   = Y step
* RINRAD = Inner radius for Polar, Cylindrical, and Spherical representations
* DANG   = Delta angle for PSD option
*
      XSTP   = (XLAB2-XLAB1)/FLOAT(NXCHA)
      YSTP   = (YLAB2-YLAB1)/FLOAT(NYCHA)
      RINRAD = 0.5
      DANG   = 10.
*.______________________________________
*
*              Plotting surfaces
*
      IF(ISURF.NE.0)THEN
         IF(IOPTS2.NE.0.OR.IOPTS4.NE.0)IDRGR=0
         IF(IOPTS3.NE.0)IDRGR=3
         IXFCHA(1) = IXMIN
         IYFCHA(1) = IYMIN
         XVAL1     = XLAB1-(IXFCHA(1)-1)*XSTP
         XVAL2     = (NCX-NXCHA-IXFCHA(1)+1)*XSTP+XLAB2
         YVAL1     = YLAB1-(IYFCHA(1)-1)*YSTP
         YVAL2     = (NCY-NYCHA-IYFCHA(1)+1)*YSTP+YLAB2
         XBUF(1)   = -1.
         YBUF(1)   = 1.
         XBUF(2)   = -1.
         YBUF(2)   = 1.
         IF(IOPPOL.NE.0)THEN
            XBUF(3) = ZMIN
            YBUF(3) = ZMAX*HMAX
         ELSEIF(IOPCYL.NE.0)THEN
            IF(ILOGY.NE.0)THEN
               XBUF(3) = LOG10(YLAB1)
               YBUF(3) = LOG10(YLAB2)
            ELSE
               XBUF(3) = YLAB1
               YBUF(3) = YLAB2
            ENDIF
            PSIDEG  = 90.
         ELSEIF(IOPSPH.NE.0)THEN
            XBUF(3) = -1.
            YBUF(3) = 1.
            PSIDEG  = 90.
         ELSEIF(IOPPSD.NE.0)THEN
            XBUF(3) = -1./TAN(DANG*RAD)
            YBUF(3) = 1./TAN(DANG*RAD)
            PSIDEG  = 90.
         ELSE
            IF(ILOGX.NE.0)THEN
               XBUF(1) = LOG10(XLAB1)
               YBUF(1) = LOG10(XLAB2)
            ELSE
               XBUF(1) = XLAB1
               YBUF(1) = XLAB2
            ENDIF
            IF(ILOGY.NE.0)THEN
               XBUF(2) = LOG10(YLAB1)
               YBUF(2) = LOG10(YLAB2)
            ELSE
               XBUF(2) = YLAB1
               YBUF(2) = YLAB2
            ENDIF
            XBUF(3) = ZMIN
            YBUF(3) = ZMAX*HMAX
         ENDIF
         CALL ISWN3(INTR
     +,             XBUF(1),YBUF(1),XBUF(2),YBUF(2),XBUF(3),YBUF(3)
     +,             PHIDEG,THEDEG,PSIDEG,' ')
         CALL ISELNT(INTR)
         CALL IGTAB4(XBUF,YBUF,XBUF(16))
*
* Close the surface in case of non cartesian coordinates.
*
         IF(ISYS.GT.1.AND.IXMIN.EQ.1.AND.IXMAX.EQ.NX)NXCHA = NXCHA+1
*
* Draw the filled contour on top
*
         IF(IOPTS3.NE.0)THEN
            CALL IGLEV(MAXCP,Z1C,Z2C,ITARR,'C')
            IF(IOPPOL.NE.0)THEN
               CALL IHSURP(1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPCYL.NE.0)THEN
               CALL IHSURR(1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPSPH.NE.0)THEN
               CALL IHSURS(0,1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPPSD.NE.0)THEN
               CALL IHSURS(1,1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSE
               CALL IHSURC(90.,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ENDIF
            IDRGR=1
         ENDIF
*
         IF(IRAST.NE.0)THEN
            CALL IHRINI(-1.1,-1.1,1.1,1.1,NRASTX,NRASTY,Q(LCG+1))
         ELSE
            CALL IHSINI(-1.1,1.1)
         ENDIF
*
         IF((IOPTS1+IOPTS2+IOPTS4).NE.0)THEN
            CALL IGLEV(INT(XBUF(13)+0.1),XBUF(3),YBUF(3),0,'A')
            CALL ISPLCI(1)
            IF(ISYS.EQ.1.AND.IOPTBB.EQ.0)
     +         CALL IHBBOX(XBUF,YBUF,90.,IHDFL1)
         ENDIF
*
* Gouraud Shading surface
*
         IF(IOPTS4.NE.0)THEN
* Define light palette
            RVAL(1)=FLOAT(IFACI)
            CALL IGQ('RGB ',RVAL)
            NBCS = INBCOL
            IF(INBCOL.LT.30)CALL IGSET('NCOL',30.)
            NCOL  = MIN(20,INBCOL-8)
            ICOL1 = INBCOL-NCOL+1
            DCOL  = 1./(2.*NCOL)
            CALL IGRTOH(RVAL(2),RVAL(3),RVAL(4),RHUE,RLIGHT,RSATUR)
            DO 350 I=ICOL1,ICOL1+NCOL-1
               CALL IGHTOR(RHUE,.4+FLOAT(I-ICOL1+1)*DCOL,RSATUR,R,G,B)
               CALL ISCR(1,I,R,G,B)
  350       CONTINUE
* Set light sources
            CALL IHLIGH(0,YDIFF, 0.,0.,0.,IREP)
            CALL IHLIGH(1,YLIGH1,1.,1.,1.,IREP)
            CALL IHPROP(QA,QD,QS,NQS,IREP)
            FMIN = YDIFF*QA
            FMAX = YDIFF*QA + (YLIGH1+0.1)*(QD+QS)
            CALL IHCSPE(NCOL,FMIN,FMAX,ICOL1,1,IREP)
            IF(IOPPOL.NE.0)THEN
               CALL IHSURP(1,NXCHA-1,NYCHA-1,IGTAB6,IHDF02,'BF')
            ELSEIF(IOPCYL.NE.0)THEN
               CALL IHSURR(1,NXCHA-1,NYCHA-1,IGTAB6,IHDF02,'BF')
            ELSEIF(IOPSPH.NE.0)THEN
               CALL IHSURS(0,1,NXCHA-1,NYCHA-1,IGTAB6,IHDF02,'BF')
            ELSEIF(IOPPSD.NE.0)THEN
               CALL IHSURS(1,1,NXCHA-1,NYCHA-1,IGTAB6,IHDF02,'BF')
            ELSE
               CALL IHSURC(90.,NXCHA-1,NYCHA-1,IGTAB6,IHDF02,'BF')
            ENDIF
            IF(NBCS.LE.8)CALL IGSET('NCOL',FLOAT(NBCS))
*
* Draw the surface
*
         ELSE
            IF(IOPTS1+IOPTS2.NE.0)THEN
               CALL IGLEV(MAXCP,Z1C,Z2C,ITARR,'C')
            ELSE
               CALL IGLEV(INT(XBUF(13)+0.1),XBUF(3),YBUF(3),0,'A')
            ENDIF
            IF(IOPPOL.NE.0)THEN
               IF(IOPTS+IOPTS3.NE.0)
     +         CALL IHSURP(1,NXCHA-1,NYCHA-1,IGTAB2,IHDFR1,'FB')
               IF(IOPTS1+IOPTS2.NE.0)
     +         CALL IHSURP(1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPCYL.NE.0)THEN
               IF(IOPTS+IOPTS3.NE.0)
     +         CALL IHSURR(1,NXCHA-1,NYCHA-1,IGTAB2,IHDFR1,'FB')
               IF(IOPTS1+IOPTS2.NE.0)
     +         CALL IHSURR(1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPSPH.NE.0)THEN
               IF(IOPTS+IOPTS3.NE.0)
     +         CALL IHSURS(0,1,NXCHA-1,NYCHA-1,IGTAB2,IHDFR1,'FB')
               IF(IOPTS1+IOPTS2.NE.0)
     +         CALL IHSURS(0,1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSEIF(IOPPSD.NE.0)THEN
               IF(IOPTS+IOPTS3.NE.0)
     +         CALL IHSURS(1,1,NXCHA-1,NYCHA-1,IGTAB2,IHDFR1,'FB')
               IF(IOPTS1+IOPTS2.NE.0)
     +         CALL IHSURS(1,1,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ELSE
               IF(IOPTS+IOPTS3.NE.0)
     +         CALL IHSURC(90.,NXCHA-1,NYCHA-1,IGTAB2,IHDFL1,'FB')
               IF(IOPTS1+IOPTS2.NE.0)
     +         CALL IHSURC(90.,NXCHA-1,NYCHA-1,IGTAB2,IHDF02,'BF')
            ENDIF
         ENDIF
*
         IF(IOPTS+IOPTS3.NE.0)THEN
            CALL ISPLCI(1)
            IF(ISYS.EQ.1.AND.IOPTBB.EQ.0)
     +         CALL IHBBOX(XBUF,YBUF,90.,IHDFL1)
         ENDIF
         IF(ISYS.EQ.1)THEN
            CALL IHSINI(-1.1,1.1)
            IF(IOPTFB.EQ.0)CALL IHFBOX(XBUF,YBUF,90.,IHDFL2)
         ENDIF
         IF(XBUF(11).NE.0.)CALL IGTAB5(XBUF,YBUF,90.,XBUF(11))
         GOTO 430
      ENDIF
*.______________________________________
*
*              Preparation for stacked legos plot
*
      IXFCHA(NIDS+1)=IXMIN
      IYFCHA(NIDS+1)=IYMIN
      IF(ILOG.NE.0)ZMAXST=10.**ZMAX
      IF(ILOG.NE.0)ZMINST=10.**ZMIN
      IXNCHA(NIDS+1)=NXCHA
      IYNCHA(NIDS+1)=NYCHA
      DO 360 I=1,NIDS
         IF(IXNCHA(I).LT.NXCHA)NXCHA=IXNCHA(I)
         IF(IYNCHA(I).LT.NYCHA)NYCHA=IYNCHA(I)
  360 CONTINUE
*
* Initialise the color indexes for lighting model (option L1 only)
*
      IF(IOPTL1.NE.0)THEN
         IF(INBCOL.LE.2)THEN
            ISTCOT=ISTCOL(NIDS+1)
            ISTCOB=ISTCOL(1)
            DO 370 I=1,NIDS+1
               ISTCO2(I)=ISTCOL(I)
  370       CONTINUE
         ELSE
            NBCSAV = NIDS+1
            DO 380 I=1,NBCSAV
               CALL IGGFIN(ISTCO2(I))
               RVAL(1)   = FLOAT(ISTCO2(I))
               CALL IGQ('RGB ',RVAL)
               RSTRS(I)  = RVAL(2)
               RSTGS(I)  = RVAL(3)
               RSTBS(I)  = RVAL(4)
               RVAL(1)   = FLOAT(ISTCOL(I))
               CALL IGQ('RGB ',RVAL)
               CALL IGRTOH(RVAL(2),RVAL(3),RVAL(4),RHUE,RLIGHT,RSATUR)
               RLIGHT = RLIGHT*0.6
               CALL IGHTOR(RHUE,RLIGHT,RSATUR,RED,GREEN,BLUE)
               CALL ISCR(1,ISTCO2(I),RED,GREEN,BLUE)
               IF(I.EQ.1)THEN
                  CALL IGGFIN(ISTCOB)
                  RVAL(1) = ISTCOB
                  CALL IGQ('RGB ',RVAL)
                  RSTRBS  = RVAL(2)
                  RSTGBS  = RVAL(3)
                  RSTBBS  = RVAL(4)
                  RLIGHT=RLIGHT*1.4
                  CALL IGHTOR(RHUE,RLIGHT,RSATUR,RED,GREEN,BLUE)
                  CALL ISCR(1,ISTCOB,RED,GREEN,BLUE)
               ENDIF
  380       CONTINUE
            CALL IGGFIN(ISTCOT)
            RVAL(1) = ISTCOT
            CALL IGQ('RGB ',RVAL)
            RSTRTS  = RVAL(2)
            RSTGTS  = RVAL(3)
            RSTBTS  = RVAL(4)
            RLIGHT=RLIGHT*1.4
            CALL IGHTOR(RHUE,RLIGHT,RSATUR,RED,GREEN,BLUE)
            CALL ISCR(1,ISTCOT,RED,GREEN,BLUE)
         ENDIF
      ENDIF
*
* Initialise the NIDS first adress for IGCELL
*
      DO 390 I=1,NIDS
         IF(IPACK.NE.0)THEN
            REFWRD(I)=FLOAT(LSTACK(I))
         ELSE
            IADRES(I)=LSTACK(I)-LOCF(REFWRD)
         ENDIF
  390 CONTINUE
*
* Compute the ZMAX value for the complete stack
*
      IF(NIDS.NE.0)THEN
         ZMINST=ZMAXST
         DO 420 J=1,NYCHA
            DO 410 I=1,NXCHA
               Z=0.
               ZLIN=0.
               DO 400 K=1,NIDS+1
                  IXT=IXFCHA(K)+I-1
                  IYT=IYFCHA(K)+J-1
                  IF(IPACK.NE.0)THEN
                     Z1=IGCELL(NCX,NCY,REFWRD(K),IXT,IYT,1)
                  ELSE
                     Z1=IGCELL(NCX,NCY,REFWRD(IADRES(K)+1),IXT,IYT,1)
                  ENDIF
                  Z=Z+Z1
                  IF(ILOG.NE.0)ZLIN=ZLIN+10.**Z1
  400          CONTINUE
               ZMAX=MAX(ZMAX,Z)
               ZMIN=MIN(ZMIN,Z)
               IF(ILOG.NE.0)THEN
                  ZMAXST=MAX(ZMAXST,ZLIN)
                  ZMAX=LOG10(ZMAXST)
                  ZMINST=MIN(ZMINST,ZLIN)
                  ZMIN=LOG10(ZMINST)
               ENDIF
  410       CONTINUE
  420    CONTINUE
      ENDIF
*
* Compute the values (for X and Y axis) on the bin 1 and NC
*
      XVAL1 = XLAB1-(IXFCHA(1)-1)*XSTP
      XVAL2 = (NCX-NXCHA-IXFCHA(1)+1)*XSTP+XLAB2
      YVAL1 = YLAB1-(IYFCHA(1)-1)*YSTP
      YVAL2 = (NCY-NYCHA-IYFCHA(1)+1)*YSTP+YLAB2
*.______________________________________
*
*              Draw the Lego plot
*
      IF(ILEGO.NE.0)THEN
         XBUF(1) = -1.
         YBUF(1) = 1.
         XBUF(2) = -1.
         YBUF(2) = 1.
         IF(ISYS.EQ.2)THEN
            XBUF(3) = ZMIN
            YBUF(3) = ZMAX*HMAX
         ELSEIF(ISYS.EQ.3)THEN
            IF(ILOGY.NE.0)THEN
               XBUF(3) = LOG10(YLAB1)
               YBUF(3) = LOG10(YLAB2)
            ELSE
               XBUF(3) = YLAB1
               YBUF(3) = YLAB2
            ENDIF
            PSIDEG  = 90.
         ELSEIF(ISYS.EQ.4)THEN
            XBUF(3) = -1.
            YBUF(3) = 1.
            PSIDEG  = 90.
         ELSEIF(ISYS.EQ.5)THEN
            XBUF(3) = -1./TAN(DANG*RAD)
            YBUF(3) = 1./TAN(DANG*RAD)
            PSIDEG  = 90.
         ELSE
            IF(ILOGX.NE.0)THEN
               XBUF(1) = LOG10(XLAB1)
               YBUF(1) = LOG10(XLAB2)
            ELSE
               XBUF(1) = XLAB1
               YBUF(1) = XLAB2
            ENDIF
            IF(ILOGY.NE.0)THEN
               XBUF(2) = LOG10(YLAB1)
               YBUF(2) = LOG10(YLAB2)
            ELSE
               XBUF(2) = YLAB1
               YBUF(2) = YLAB2
            ENDIF
            XBUF(3) = ZMIN
            YBUF(3) = ZMAX*HMAX
         ENDIF
*
         CALL IGLEV(INT(XBUF(13)+0.1),XBUF(3),YBUF(3),0,'A')
         CALL ISWN3(INTR
     +,             XBUF(1),YBUF(1),XBUF(2),YBUF(2),XBUF(3),YBUF(3)
     +,             PHIDEG,THEDEG,PSIDEG,' ')
         CALL ISELNT(INTR)
         CALL IGTAB4(XBUF,YBUF,XBUF(16))
*
         IF(IRAST.NE.0)THEN
            CALL IHRINI(-1.1,-1.1,1.1,1.1,NRASTX,NRASTY,Q(LCG+1))
         ELSE
            CALL IHSINI(-1.1,1.1)
         ENDIF
*
         IF((IOPTL1+IOPTL2).NE.0)THEN
            CALL ISPLCI(1)
            IF(ISYS.EQ.1.AND.IOPTBB.EQ.0)
     +         CALL IHBBOX(XBUF,YBUF,90.,IHDFL1)
         ENDIF
         IF(IOPTL2.NE.0)CALL IGLEV(MAXCP,Z1C,Z2C,ITARR,'C')
*
         IF(ISYS.EQ.2)THEN
            IF(IOPTL.NE.0) CALL IHLEGP(1,NXCHA,NYCHA,IGTAB3,IHDFR2,'FB')
            IF(IOPTL1.NE.0)CALL IHLEGP(1,NXCHA,NYCHA,IGTAB3,IHDF03,'BF')
            IF(IOPTL2.NE.0)CALL IHLEGP(1,NXCHA,NYCHA,IGTAB3,IHDF02,'BF')
         ELSEIF(ISYS.EQ.3)THEN
            IF(IOPTL.NE.0) CALL IHLEGR(1,NXCHA,NYCHA,IGTAB3,IHDFR2,'FB')
            IF(IOPTL1.NE.0)CALL IHLEGR(1,NXCHA,NYCHA,IGTAB3,IHDF03,'BF')
            IF(IOPTL2.NE.0)CALL IHLEGR(1,NXCHA,NYCHA,IGTAB3,IHDF02,'BF')
         ELSEIF(ISYS.EQ.4)THEN
            IF(IOPTL.NE.0)
     +        CALL IHLEGS(0,1,NXCHA,NYCHA,IGTAB3,IHDFR2,'FB')
            IF(IOPTL1.NE.0)
     +        CALL IHLEGS(0,1,NXCHA,NYCHA,IGTAB3,IHDF03,'BF')
            IF(IOPTL2.NE.0)
     +        CALL IHLEGS(0,1,NXCHA,NYCHA,IGTAB3,IHDF02,'BF')
         ELSEIF(ISYS.EQ.5)THEN
            IF(IOPTL.NE.0)
     +        CALL IHLEGS(1,1,NXCHA,NYCHA,IGTAB3,IHDFR2,'FB')
            IF(IOPTL1.NE.0)
     +        CALL IHLEGS(1,1,NXCHA,NYCHA,IGTAB3,IHDF03,'BF')
            IF(IOPTL2.NE.0)
     +        CALL IHLEGS(1,1,NXCHA,NYCHA,IGTAB3,IHDF02,'BF')
         ELSE
            IF(IOPTL.NE.0)
     +        CALL IHLEGC(90.,NXCHA,NYCHA,IGTAB3,IHDFL2,'FB')
            IF(IOPTL1.NE.0)
     +        CALL IHLEGC(90.,NXCHA,NYCHA,IGTAB3,IHDF03,'BF')
            IF(IOPTL2.NE.0)
     +        CALL IHLEGC(90.,NXCHA,NYCHA,IGTAB3,IHDF02,'BF')
         ENDIF
*
         IF(IOPTL.NE.0)THEN
            CALL ISPLCI(1)
            IF(ISYS.EQ.1.AND.IOPTBB.EQ.0)
     +         CALL IHBBOX(XBUF,YBUF,90.,IHDFL1)
         ENDIF
         IF(ISYS.EQ.1)THEN
            CALL IHSINI(-1.1,1.1)
            IF(IOPTFB.EQ.0)CALL IHFBOX(XBUF,YBUF,90.,IHDFL2)
         ENDIF
         IF(XBUF(11).NE.0.)CALL IGTAB5(XBUF,YBUF,90.,XBUF(11))
         NIDS = 0
         GOTO 450
      ENDIF
*.______________________________________
*
*              Draw the colour table if required
*
  430 IF(IOPTZ.NE.0)THEN
         XX=0.01*(RWXMAX-RWXMIN)
         CALL ISCLIP(0)
         IF(IOPTGZ.NE.0)THEN
            CALL IGCOLM(RWXMAX+XX,RWXMAX+4.*XX,RWYMIN,RWYMAX
     +,                 MAXCP,ITARR,10.**Z1C,10.**Z2C,'CAPG')
         ELSE
            CALL IGCOLM(RWXMAX+XX,RWXMAX+4.*XX,RWYMIN,RWYMAX
     +,                 MAXCP,ITARR,Z1C,Z2C,'CAP')
         ENDIF
         GOTO 450
      ENDIF
*.______________________________________
*
*              Draw the axis for the 2D representation
*
  440 IF(IOPTA.NE.0)THEN
         CALL ISLN(1)
         CALL ISFAIS(0)
         CALL IGBOX(RWXMIN,RWXMAX,RWYMIN,RWYMAX)
         XLAB1=FLOAT(IXMIN)
         XLAB2=FLOAT(IXMAX)
         YLAB1=FLOAT(IYMIN)
         YLAB2=FLOAT(IYMAX)
         IF(NPAR.GE.6)THEN
            IF(PAR(4).GT.PAR(3))THEN
               XLAB1=PAR(3)
               XLAB2=PAR(4)
            ENDIF
            IF(PAR(6).GT.PAR(5))THEN
               YLAB1=PAR(5)
               YLAB2=PAR(6)
            ENDIF
         ENDIF
         AXFLAG=.FALSE.
         CHOPT=' '
         IF(IOPTGX.NE.0)CHOPT='G'
         CALL IGAXIS(RWXMIN,RWXMAX,RWYMIN,RWYMIN,XLAB1,XLAB2,510,
     +   CHOPT)
         AXFLAG=.TRUE.
         CHOPT=' '
         IF(IOPTGY.NE.0)CHOPT='G'
         CALL IGAXIS(RWXMIN,RWXMIN,RWYMIN,RWYMAX,YLAB1,YLAB2,510,
     +   CHOPT)
         AXFLAG=.FALSE.
      ENDIF
*
* Restore original graphic context
*
  450 IF(IRAST.NE.0)THEN
         CALL MZDROP(IXHIGZ,LCG,' ')
         LCG = 0
      ENDIF
      CALL IZSET
      ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
