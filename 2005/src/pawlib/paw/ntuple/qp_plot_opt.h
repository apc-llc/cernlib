/*
 *  qp_plot_opt.h  --
 *
 *  Original:  9-Nov-1995 16:42
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_plot_opt.h,v 1.3 1996/09/24 08:58:09 lecointe Exp $
 *
 *  $Log: qp_plot_opt.h,v $
 *  Revision 1.3  1996/09/24 08:58:09  lecointe
 *  Add option 'N' in NTUPLE/PLOT : Fill the 1D or 2D histogram without drawing
 *  anything
 *
 *  Revision 1.2  1996/04/23 18:38:52  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_PLOT_OPT
#define CERN_QP_PLOT_OPT


typedef struct _plot_options_ {

bool	star;	/* *      Draw a * at each channel. */
bool	add;	/* +      Add contents of ID to last plotted histogram. */
bool	diff;	/* +-     Draw the delta with the last plotted histogram. */
bool	sub;	/* -      Substract contents of ID to last plotted histogram. */
bool	A;	/* A      Axis labels and tick marks are not drawn. */
bool	B;	/* B      Select Bar chart format. */
bool	BB;	/* BB     Suppress the Back Box on 3D plots. */
bool	box;	/* BOX    Draw 2-Dim with proportional boxes. */
bool	C;	/* C      Draw a smooth curve. */
bool	chr;	/* CHAR   Draw 2-Dim with characters (a la HBOOK). */
bool	col;	/* COL    Draw 2-Dim with a color table. */
bool	cont;	/* CONT   Draw 2-Dim as a contour plot (15 levels). */
bool	cyl;	/* CYL    Cylindrical coordinates for 3D plots. */
bool	E;	/* E      Compute (HBARX) and draw error bars with current marker. */
bool	e0;	/* E0     Draw error bars without symbols clipping. */
bool	e1;	/* E1     Draw small lines at the end of the error bars. */
bool	e2;	/* E2     Draw error rectangles. */
bool	e3;	/* E3     Draw a filled area through the end points of the vertical error bars. */
bool	e4;	/* E4     Draw a smoothed filled area through the end points of the vertical error bars. */
bool	FB;	/* FB     Suppress the Front Box on 3D plots. */
bool	func;	/* FUNC   Draw only the associated function (not the histogram). */
bool	hist;	/* HIST   Draw only histogram (no errors or associated function). */
bool	K;	/* K      Must be given if option 'U' is given later. */
bool	line;	/* L      Connect channels contents by a line. */
bool	lego;	/* LEGO   Draw as a lego plot (angles are set via the command angle). */
bool	lego1;	/* LEGO1  Draw lego plot with light simulation. */
bool	lego2;	/* LEGO2  Draw lego plot with color levels. */
bool	P;	/* P      Draw the current polymarker at each channel or cell. */
bool	pol;	/* POL    Polar coordinates for 3D plots. */
bool	prof;	/* PROF   Fill a Profile histogram (mean option). */
bool	profi;	/* PROFI  Fill a Profile histogram (integer spread option). */
bool	profs;	/* PROFS  Fill a Profile histogram (spread option). */
bool	psd;	/* PSD    Pseudo-rapidity/phi coordinates for 3D plots. */
bool	S;	/* S      Superimpose plot on top of existing picture. */
bool	sph;	/* SPH    Spherical coordinates for 3D plots. */
bool	surf;	/* SURF   Draw as a surface plot (angles are set via the command angle). */
bool	surf1;	/* SURF1  Draw as a surface with color levels */
bool	surf2;	/* SURF2  Same as SURF1 but without cell lines. */
bool	surf3;	/* SURF3  Same as SURF but with the contour plot (in color) on top. */
bool	surf4;	/* SURF4  Draw as a surface with Gouraud shading. */
bool	text;	/* TEXT   Draw 2-Dim as a table. */
bool	U;	/* U      Update channels modified since last call. */
bool	Z;	/* Z      Used with COL or SURF, it draws the color map. */
bool	gouraud;/* G  Draw as a surface with Gouraud shading. */
bool	N;      /* No plot, just fill the histogram IDH */

/* not part of option strings but simmilar enough */

bool	logx;	/* logaritmic x axis */
bool	logy;	/* logaritmic y axis */
bool	logz;	/* logaritmic z axis */

} PlotOptions;


#define	PROFILE_OPTION( o )	( (o.prof) || (o.profi) || (o.profs) )


char *
qp_plot_opt_gen( PlotOptions * opt, bool hplot_only );

void
qp_plot_opt_scan( PlotOptions * opt, char * option );

#endif	/*	CERN_QP_PLOT_OPT	*/
