/*
 *  qp_command.c  --
 *	Implement commands for handling QPCmd structures.
 *
 *  Original:  6-Nov-1995 11:47
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_command.c,v 1.17 1996/08/30 10:04:27 lecointe Exp $
 *
 *  $Log: qp_command.c,v $
 *  Revision 1.17  1996/08/30 10:04:27  lecointe
 *  Restored Gouraud Shading in NT/PLOT
 *
 *  Revision 1.16  1996/08/21 12:55:33  lecointe
 *  Restore the spider plot in ntuple/scan
 *
 *  Revision 1.15  1996/05/14 12:23:25  maartenb
 *  - Fix prototypes.
 *
 *  - Fix static bool conversions
 *
 *  Revision 1.14  1996/04/23 18:38:09  maartenb
 *  - Add RCS keywords
 *
 *
 */

#include	<stdlib.h>
#include	<string.h>

#include	"qp_command.h"
#include	"qp_report.h"
#include	"str.h"


static float
bool_float( register void *p, void *d )
{
	if ( *(bool *) p ) {
		return 1.;
	} else {
		return 0.;
	}
}


static float
UInt32_float( register void *p, void *d )
{
	return *(UInt32 *) p;
}


static float
UInt64_float( register void *p, void *d )
{
	return *(UInt64 *) p;
}


static float
Int32_float( register void *p, void *d )
{
	return *(Int32 *) p;
}


static float
Int64_float( register void *p, void *d )
{
	return *(Int64 *) p;
}


static float
Float32_float( register void *p, void *d )
{
	return *(Float32 *) p;
}


static float
Float64_float( register void *p, void *d )
{
	if ( (*(Float64 *) p) > FLT_MAX || (*(Float64 *) p) < -FLT_MAX ) {
		return (*(Float64 *) p) > 0 ? FLT_MAX : -FLT_MAX;
	} else {
		return *(Float64 *) p;
	}
}


static float
String_float( void *p, void *d )
{
	SMap	sm = (SMap) d;
	char	key[MAX_LABEL_LEN+1];
	int	i;

	memcpy( key, (String) p, MAX_LABEL_LEN );
	key[MAX_LABEL_LEN] = '\0';
	if ( ! smap_map( sm, key, &i ) ) {
		i = -1;
	}

	return i + 0.5;
}


static void
bool_print( String b, void *p )
{
	if ( *(bool *) p ) {
		strcpy( b, "true" );
	} else {
		strcpy( b, "false" );
	}
}


static void
UInt32_print( String b, void *p )
{
	sprintf( b, "z'%x'", *(UInt32 *) p );
}


static void
UInt64_print( String b, void *p )
{
	sprintf( b, "z'%lx'", *(UInt64 *) p );
}


static void
Int32_print( String b, void *p )
{
	sprintf( b, "% d", *(Int32 *) p );
}


static void
Int64_print( String b, void *p )
{
	sprintf( b, "% ld", *(Int64 *) p );
}


static void
Float32_print( String b, void *p )
{
	sprintf( b, "%- .6g", *(Float32 *) p );
	if ( strchr( b, '.' ) == 0 )
		strcat( b, "." );
}


static void
Float32_dump( String b, void *p )
{
	sprintf( b, "%- .6e", *(Float32 *) p );
}


static void
Float64_print( String b, void *p )
{
	char	*s;

	sprintf( b, "%- .14lg", *(Float64 *) p );
	s = strchr( b, 'e' );
	if ( strchr( b, '.' ) == 0 )
		strcat( b, "." );
	if ( s != 0 ) {
		*s = 'd';
	}
}


static void
Float64_dump( String b, void *p )
{
	char	*s;

	sprintf( b, "%- .14le", *(Float64 *) p );
	s = strchr( b, 'e' );
	if ( s != 0 ) {
		*s = 'd';
	}
}


static void
String_print( String b, void *p )
{
	String	s;

	s = (String) p;

	if ( strlen( s ) <= 30 ) {
		sprintf( b, "'%*.*s'", QP_STR_MAX-2, QP_STR_MAX-2, s );
	} else {
		sprintf( b, "%*.*s", QP_STR_MAX, QP_STR_MAX, s );
	}
}


static void
String_dump( String b, void *p )
{
	String	s;

	s = (String) p;

	sprintf( b, "'%*.*s'", QP_STR_MAX, QP_STR_MAX, s );
}


int const	datatype_width[] = {
	5,	/* D_BOOL */
	11,	/* D_UINT */
	19,	/* D_ULONG */
	11,	/* D_INT */
	20,	/* D_LONG */
	12,	/* D_FLOAT */
	21,	/* D_DOUBLE */
	QP_STR_MAX, /* D_STR */
	0,	/* D_UNDEF */
};

CvtCallBack
datatype_to_cvtcallback( DataType dtype, void * cbd )
{
	CvtCallBack	cb;

	cb.d = cbd;

	switch ( dtype ) {	/* switch DataType */
	case D_BOOL: cb.f = bool_float; break;
	case D_UINT: cb.f = UInt32_float; break;
	case D_ULONG: cb.f = UInt64_float; break;
	case D_INT: cb.f = Int32_float; break;
	case D_LONG: cb.f = Int64_float; break;
	case D_FLOAT: cb.f = Float32_float; break;
	case D_DOUBLE: cb.f = Float64_float; break;
	case D_STR: cb.f = String_float; break;
	default:
		qp_abort( "datatype_to_cvtfun: Unknown datatype %d\n", dtype );
		break;
	}

	return cb;
}


PrintFun
datatype_to_printfun( DataType dtype )
{
	PrintFun	f;

	switch ( dtype ) {	/* switch DataType */
	case D_BOOL: f = bool_print; break;
	case D_UINT: f = UInt32_print; break;
	case D_ULONG: f = UInt64_print; break;
	case D_INT: f = Int32_print; break;
	case D_LONG: f = Int64_print; break;
	case D_FLOAT: f = Float32_print; break;
	case D_DOUBLE: f = Float64_print; break;
	case D_STR: f = String_print; break;
	default:
		qp_abort( "datatype_to_printfun: Unknown datatype %d\n", dtype);
		break;
	}

	return f;
}


PrintFun
datatype_to_dumpfun( DataType dtype )
{
	PrintFun	f;

	switch ( dtype ) {	/* switch DataType */
	case D_BOOL: f = bool_print; break;
	case D_UINT: f = UInt32_print; break;
	case D_ULONG: f = UInt64_print; break;
	case D_INT: f = Int32_print; break;
	case D_LONG: f = Int64_print; break;
	case D_FLOAT: f = Float32_dump; break;
	case D_DOUBLE: f = Float64_dump; break;
	case D_STR: f = String_dump; break;
	default:
		qp_abort( "datatype_to_printfun: Unknown datatype %d\n", dtype);
		break;
	}

	return f;
}

int
datatype_to_typecode( DataType dtype)
{

        int foo;
 
	switch ( dtype ) {	/* switch DataType */
	case D_BOOL:
	case D_UINT:
	case D_ULONG:
	case D_INT: 
	case D_LONG: foo=0; break;
	case D_FLOAT: 
	case D_DOUBLE: foo=1; break;
	case D_STR: foo=2; break;
	default:
		qp_abort( "datatype_to_typecode: Unknown datatype %d\n", dtype );
		break;
	}

	return foo;
}
QPCmd *
qpcmd_new( CmdType command )
{
	QPCmd	*cmd;

	cmd = (QPCmd *) calloc ( sizeof( QPCmd ), 1 );
	qp_assert( cmd != 0 );

	cmd->command = command;

	return cmd;
}


void
qpcmd_free( QPCmd * c )
{
	
	switch ( c->command ) {
	case CMD_NULL:
		break;
	case CMD_MINMAX:
		if ( c->u.minmax.el != 0 ) {
			int	i;

			for ( i=0 ; i < c->u.minmax.n ; i++ ) {
				if ( c->u.minmax.el[i].sm != 0 ) {
					smap_del( c->u.minmax.el[i].sm );
				}
			}

			free( (void *) c->u.minmax.el );
		}
		break;

	case CMD_MINMAXAVG:
		if ( c->u.minmaxavg.el != 0 ) {
			int	i;

			for ( i=0 ; i < c->u.minmaxavg.n ; i++ ) {
				if ( c->u.minmaxavg.el[i].sm != 0 ) {
					smap_del( c->u.minmaxavg.el[i].sm );
				}
			}

			free( (void *) c->u.minmaxavg.el );
		}
		break;

	case CMD_VFILL:
		break;
	case CMD_HFILL1:
		break;
	case CMD_HFILL2:
		break;
	case CMD_SCAT2:
		break;
	case CMD_SCAT3:
		break;
	case CMD_SCAT4:
		break;
	case CMD_GOURAUD:
		break;
	case CMD_SCAN:
			if ( c->u.scan.sep ) str_del( c->u.scan.sep );
			if ( c->u.scan.title ) str_del( c->u.scan.title );
			if ( c->u.scan.buf ) str_del( c->u.scan.buf );
			break;
	case CMD_SPIDERSCAN:
			if ( c->u.sp_scan.expr_str) free( c->u.sp_scan.expr_str );
			break;
	case CMD_LOOP:
		break;
	case CMD_DUMP:
			if ( c->u.dump.sep1 ) str_del( c->u.dump.sep1 );
			if ( c->u.dump.sep2 ) str_del( c->u.dump.sep2 );
			break;

	default:
		qp_abort( "qpcmd_free: Unknown command %d\n", c->command );
		break;
	}

	free( (void *) c );
}


void
qpcmd_getminmax( QPCmd * cmd, int i, float * min, float * max )
{
	QPCmdMinMax	*p;
	Extreme		*e;

	qp_assert( cmd->command == CMD_MINMAX );

	p = &cmd->u.minmax;

	qp_assert( 0 <= i && i < p->n );

	e = &p->el[i];

	switch ( e->dtyp ) {	/* switch DataType */
	case D_BOOL:
		*min = e->min.bval ? 1. : 0. ;
		*max = e->max.bval ? 1. : 0. ;
		break;
	case D_UINT:
		*min = e->min.uval;
		*max = e->max.uval;
		break;
	case D_ULONG:
		*min = e->min.luval;
		*max = e->max.luval;
		break;
	case D_INT:
		*min = e->min.ival;
		*max = e->max.ival;
		break;
	case D_LONG:
		*min = e->min.lival;
		*max = e->max.lival;
		break;
	case D_FLOAT:
		*min = e->min.fval;
		*max = e->max.fval;
		break;
	case D_DOUBLE:
		if ( (e->min.lfval > FLT_MAX) || (e->min.lfval < -FLT_MAX) ) {
			sf_report( "Cannot convert double value to float (%lg)\n",
				e->min.lfval );
			*min = e->min.lfval < 0 ? -FLT_MAX : FLT_MAX;
		} else {
			*min = e->min.lfval;
		}
		if ( (e->max.lfval > FLT_MAX) || (e->max.lfval < -FLT_MAX) ) {
			sf_report( "Cannot convert double value to float (%lg)\n",
				e->max.lfval );
			*max = e->max.lfval < 0 ? -FLT_MAX : FLT_MAX;
		} else {
			*max = e->max.lfval;
		}
		break;
	default:
		qp_abort( "qpcmd_getminmax: not implemented for %s\n",
			datatype_to_str( e->dtyp ) );
		break;
	}

}

void
qpcmd_getminmaxavg( QPCmd * cmd, int i, float * min, float * max, float * avg )
{
	QPCmdMinMaxAvg	*p;
	ExtremeAvg	*e;

	qp_assert( cmd->command == CMD_MINMAXAVG );

	p = &cmd->u.minmaxavg;

	qp_assert( 0 <= i && i < p->n );

	e = &p->el[i];

	*avg = (float) e->avg;	

	switch ( e->dtyp ) {	/* switch DataType */
	case D_BOOL:
		*min = e->min.bval ? 1. : 0. ;
		*max = e->max.bval ? 1. : 0. ;
		break;
	case D_UINT:
		*min = e->min.uval;
		*max = e->max.uval;
		break;
	case D_ULONG:
		*min = e->min.luval;
		*max = e->max.luval;
		break;
	case D_INT:
		*min = e->min.ival;
		*max = e->max.ival;
		break;
	case D_LONG:
		*min = e->min.lival;
		*max = e->max.lival;
		break;
	case D_FLOAT:
		*min = e->min.fval;
		*max = e->max.fval;
		break;
	case D_DOUBLE:
		if ( (e->min.lfval > FLT_MAX) || (e->min.lfval < -FLT_MAX) ) {
			sf_report( "Cannot convert double value to float (%lg)\n",
				e->min.lfval );
			*min = e->min.lfval < 0 ? -FLT_MAX : FLT_MAX;
		} else {
			*min = e->min.lfval;
		}
		if ( (e->max.lfval > FLT_MAX) || (e->max.lfval < -FLT_MAX) ) {
			sf_report( "Cannot convert double value to float (%lg)\n",
				e->max.lfval );
			*max = e->max.lfval < 0 ? -FLT_MAX : FLT_MAX;
		} else {
			*max = e->max.lfval;
		}
		break;
	default:
		qp_abort( "qpcmd_getminmax: not implemented for %s\n",
			datatype_to_str( e->dtyp ) );
		break;
	}

}


SMap
qpcmd_labels(  QPCmd * cmd, int i )
{
	qp_assert( cmd->u.minmax.el[i].dtyp == D_STR );

	return smap_copy( cmd->u.minmax.el[i].sm );
}
