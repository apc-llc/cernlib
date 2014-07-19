/*
 *  qp_plot_opt.c  --
 *
 *  Original:  9-Nov-1995 17:29
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_plot_opt.c,v 1.6 2000/10/02 16:28:49 couet Exp $
 *
 *  $Log: qp_plot_opt.c,v $
 *  Revision 1.6  2000/10/02 16:28:49  couet
 *  - D is recognise as a valid option in NT/PLOT (meanning "Default") because
 *    it is used by Paw++. The query processor printed an error message saying
 *    it was not a valid option (in Paw++).
 *
 *  Revision 1.5  2000/09/29 13:32:38  couet
 *  - Allows ' ' as a separator in NT/Plot options. This prevent to have the
 *    message "Unknown option ' '" when ntuple plot is used with BOX option
 *    from the ntuple panel in Paw++
 *
 *  Revision 1.4  1996/09/24 08:58:08  lecointe
 *  Add option 'N' in NTUPLE/PLOT : Fill the 1D or 2D histogram without drawing
 *  anything
 *
 *  Revision 1.3  1996/05/14 12:23:30  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.2  1996/04/23 18:38:52  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<string.h>

#include	"cern_types.h"
#include	"qp_plot_opt.h"
#include	"str.h"
#include	"kuip_interface.h"
#include	"qp_report.h"


static bool
opt_set( char **p, char * opt, bool * flag )
{
	int	l;

	l = strlen( opt );

	if ( strncasecmp( *p, opt, l ) == 0 ) {
		*p += l;
		*flag = TRUE;
		return TRUE;
	}

	return FALSE;
}


void
qp_plot_opt_scan( PlotOptions * opt, char * option )
{
	char		*p;

	memset( opt, 0, sizeof(PlotOptions) );
	p = option;
	while ( *p != 0 ) {
		/* ordering on lenght of the option string is important */
		if ( opt_set( &p, "LEGO1", &opt->lego1 ) ) continue;
		if ( opt_set( &p, "LEGO2", &opt->lego2 ) ) continue;
		if ( opt_set( &p, "PROFI", &opt->profi ) ) continue;
		if ( opt_set( &p, "PROFS", &opt->profs ) ) continue;
		if ( opt_set( &p, "SURF1", &opt->surf1 ) ) continue;
		if ( opt_set( &p, "SURF2", &opt->surf2 ) ) continue;
		if ( opt_set( &p, "SURF3", &opt->surf3 ) ) continue;
		if ( opt_set( &p, "SURF4", &opt->surf4 ) ) continue;

		if ( opt_set( &p, "CHAR", &opt->chr ) ) continue;
		if ( opt_set( &p, "CONT", &opt->cont ) ) continue;
		if ( opt_set( &p, "FUNC", &opt->func ) ) continue;
		if ( opt_set( &p, "HIST", &opt->hist ) ) continue;
		if ( opt_set( &p, "LEGO", &opt->lego ) ) continue;
		if ( opt_set( &p, "PROF", &opt->prof ) ) continue;
		if ( opt_set( &p, "SURF", &opt->surf ) ) continue;
		if ( opt_set( &p, "TEXT", &opt->text ) ) continue;

		if ( opt_set( &p, "BOX", &opt->box ) ) continue;
		if ( opt_set( &p, "COL", &opt->col ) ) continue;
		if ( opt_set( &p, "CYL", &opt->cyl ) ) continue;
		if ( opt_set( &p, "POL", &opt->pol ) ) continue;
		if ( opt_set( &p, "PSD", &opt->psd ) ) continue;
		if ( opt_set( &p, "SPH", &opt->sph ) ) continue;

		if ( opt_set( &p, "+-", &opt->diff ) ) continue;
		if ( opt_set( &p, "BB", &opt->BB ) ) continue;
		if ( opt_set( &p, "E0", &opt->e0 ) ) continue;
		if ( opt_set( &p, "E1", &opt->e1 ) ) continue;
		if ( opt_set( &p, "E2", &opt->e2 ) ) continue;
		if ( opt_set( &p, "E3", &opt->e3 ) ) continue;
		if ( opt_set( &p, "E4", &opt->e4 ) ) continue;
		if ( opt_set( &p, "FB", &opt->FB ) ) continue;

		if ( opt_set( &p, "*", &opt->star ) ) continue;
		if ( opt_set( &p, "+", &opt->add ) ) continue;
		if ( opt_set( &p, "-", &opt->sub ) ) continue;
		if ( opt_set( &p, "A", &opt->A ) ) continue;
		if ( opt_set( &p, "B", &opt->B ) ) continue;
		if ( opt_set( &p, "C", &opt->C ) ) continue;
		if ( opt_set( &p, "E", &opt->E ) ) continue;
		if ( opt_set( &p, "G", &opt->gouraud ) ) continue;
		if ( opt_set( &p, "K", &opt->K ) ) continue;
		if ( opt_set( &p, "L", &opt->line ) ) continue;
		if ( opt_set( &p, "P", &opt->P ) ) continue;
		if ( opt_set( &p, "S", &opt->S ) ) continue;
		if ( opt_set( &p, "U", &opt->U ) ) continue;
		if ( opt_set( &p, "Z", &opt->Z ) ) continue;
		if ( opt_set( &p, "N", &opt->N ) ) continue;

		/* accept ',' and ' ' as separators and 'D' as default */
		if ( *p == ',' || *p == ' ' || *p == 'D') {
			p += 1;
			continue;
		}

		sf_report( "Unknown option '%c'\n", *p );
		p += 1;
	}
}


static void
opt_add( char **p, char *s, bool flag )
{
	if ( flag ) {
		*p = str_merge( *p, str_new(","), str_new( s ), (char *) 0 );
	}
}


char *
qp_plot_opt_gen( PlotOptions * opt, bool hplot_only )
{
	char *		p;

	p = str_new( "" );

	opt_add( &p, "LEGO1", opt->lego1 );
	opt_add( &p, "LEGO2", opt->lego2 );
	if ( !hplot_only ) opt_add( &p, "PROFI", opt->profi );
	if ( !hplot_only ) opt_add( &p, "PROFS", opt->profs );
	opt_add( &p, "SURF1", opt->surf1 );
	opt_add( &p, "SURF2", opt->surf2 );
	opt_add( &p, "SURF3", opt->surf3 );
	opt_add( &p, "SURF4", opt->surf4 );

	opt_add( &p, "CHAR", opt->chr );
	opt_add( &p, "CONT", opt->cont );
	opt_add( &p, "FUNC", opt->func );
	opt_add( &p, "HIST", opt->hist );
	opt_add( &p, "LEGO", opt->lego );
	if ( !hplot_only ) opt_add( &p, "PROF", opt->prof );
	opt_add( &p, "SURF", opt->surf );
	opt_add( &p, "TEXT", opt->text );

	opt_add( &p, "BOX", opt->box );
	opt_add( &p, "COL", opt->col );
	opt_add( &p, "CYL", opt->cyl );
	opt_add( &p, "POL", opt->pol );
	opt_add( &p, "PSD", opt->psd );
	opt_add( &p, "SPH", opt->sph );

	opt_add( &p, "+-", opt->diff );
	opt_add( &p, "BB", opt->BB );
	opt_add( &p, "E0", opt->e0 );
	opt_add( &p, "E1", opt->e1 );
	opt_add( &p, "E2", opt->e2 );
	opt_add( &p, "E3", opt->e3 );
	opt_add( &p, "E4", opt->e4 );
	opt_add( &p, "FB", opt->FB );

	opt_add( &p, "*", opt->star );
	opt_add( &p, "+", opt->add );
	opt_add( &p, "-", opt->sub );
	opt_add( &p, "A", opt->A );
	opt_add( &p, "B", opt->B );
	opt_add( &p, "C", opt->C );
	opt_add( &p, "E", opt->E );
	opt_add( &p, "G", opt->gouraud );
	opt_add( &p, "K", opt->K );
	opt_add( &p, "L", opt->line );
	opt_add( &p, "P", opt->P );
	opt_add( &p, "S", opt->S );
	opt_add( &p, "U", opt->U );
	opt_add( &p, "Z", opt->Z );

	return p;
}

