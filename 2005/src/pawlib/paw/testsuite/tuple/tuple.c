/*
 *  tuple.c  --
 *	Generate a CWN with all kinds of types
 *
 *  Original: 21-Oct-1994 18:06
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 */

#include	<unistd.h>
#include	<stdlib.h>
#include	<stdio.h>
#include	<math.h>
#include	<string.h>
#include	<float.h>


#include	"cfortran.h"

/* #include	"packlib.h" */
#include	"hbook_interface.h"
#include	"c_quest.h"

#define HRENDC(FROT) \
	CCALLSFSUB1(HRENDC,hrendc,STRING,FROT)


void usage();


char	*prog_name;

#define R_OK	0
#define R_ERROR	1

#define PAWC_SIZE	800000
#define PAWC COMMON_BLOCK(PAWC,pawc)
struct {
	int	iq[PAWC_SIZE];
} PAWC;

int	the_id;


char	rwn_fields[][9] = {
	"x2",
	"x3",
	"x5",
	"x7",
	"rnd",
	"gauss",
	"x",
	"y",
	"z",
	"c"
};
int	n_rwn_fields = sizeof(rwn_fields)/sizeof(char[9]);

struct {
	float	x2, x3, x5, x7, rnd, gauss, x, y, z, c;
} my_rwn;


rwn_start( char * fname, int lrecl )
{
	int	ierr;

	HLIMIT( PAWC_SIZE );

	HBSET("BSIZE", lrecl, ierr);

	QUEST.iquest[9] = 65000;	/* iquest(10) (!) */

	HROPEN( 1, "TUPLE", fname, "NCQ", lrecl, ierr);
	if ( ierr != 0 ) {
		fprintf( stderr, "%s: hropen '%s' failed.\n",
			prog_name, fname );
		return R_ERROR;
	}

	HBOOKN( 20, "Maartens CWN Test Ntuple", n_rwn_fields, "//TUPLE",
		lrecl/4, rwn_fields );

	return R_OK;
}


rwn_end()
{
	int	icycle;

	HROUT( 20, icycle, " ");

	HRENDC("TUPLE");

	return R_OK;
}


void
create_rwn( char *fname, int lrecl, int limit )
{
	int		i;
	float		*buf = (float *) &my_rwn;
	static unsigned short int	xi[3];
	static unsigned short int	yi[3];
	double				gauss48();
	double				erand48();

	rwn_start( fname, lrecl );

	for( i=0 ; i < limit ; i++ ) {

		my_rwn.x2 = i % 2;
		my_rwn.x3 = i % 3;
		my_rwn.x5 = i % 5;
		my_rwn.x7 = i % 7;

		my_rwn.rnd = erand48( xi );;
		my_rwn.gauss = gauss48( xi );;


		my_rwn.x = 2 * erand48( yi );
		my_rwn.y = 2 * gauss48( yi );
		my_rwn.z = my_rwn.x * my_rwn.x + my_rwn.y * my_rwn.y;
		my_rwn.c = my_rwn.x + my_rwn.y;

		HFN( 20, buf );
	}


	rwn_end();
}


cwn_start( int id, char * fname, int lrecl )
{
	int	ierr;

	the_id = id;

	HLIMIT( PAWC_SIZE );

	HBSET("BSIZE", lrecl, ierr);

	QUEST.iquest[9] = 65000;	/* iquest(10) (!) */

	HROPEN( 1, "TUPLE", fname, "NCQ", lrecl, ierr);
	if ( ierr != 0 ) {
		fprintf( stderr, "%s: hropen '%s' failed.\n",
			prog_name, fname );
		return R_ERROR;
	}

	HBNT( the_id, "Maartens CWN Test Ntuple", " ");

	return R_OK;
}


struct {
	float	x;
	float	y;
	float	z;
	float	c;
} nt_basic;


nt_book_basic() {

	HBNAME( the_id, "nt_basic", (int *) &nt_basic,
		"x:R,"
		"y:R,"
		"z:R,"
		"c:R"
		);
}


nt_fill_basic( unsigned long ievt )
{
	static unsigned short int	xi[3];
	double				gauss48();

	nt_basic.x = 2 * erand48( xi );
	nt_basic.y = 2 * gauss48( xi );
	nt_basic.z = nt_basic.x * nt_basic.x + nt_basic.y * nt_basic.y;
	nt_basic.c = nt_basic.x + nt_basic.y;
}


struct {
	int	b2;
	int	brnd;
	int	ib7;
	int	bvec[5];
	int	bmat[4][3];
	int	bvvec[ /* ib7 */ 6];
	int	bvmat[ /* ib7 */ 6][3];
} nt_bool;


nt_book_bool() {

	HBNAME( the_id, "nt_bool", (int *) &nt_bool,
		"b2:L,"
		"brnd:L,"
		"ib7[0,6]:I,"
		"bvec(5):L,"
		"bmat(3,4):L,"
		"bvvec(ib7):L,"
		"bvmat(3,ib7):L"
		);
}


nt_fill_bool( unsigned long ievt )
{
	static unsigned short int	xi[3];
	int				i,j;

	nt_bool.b2 = ievt % 2 ;
	nt_bool.ib7 = ievt % 7 ;
	nt_bool.brnd = nrand48( xi ) % 2;

	for( i=0 ; i < 5 ; i++ )
		nt_bool.bvec[i] = i % 2  ;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_bool.bmat[i][j] = (j + i) % 2 ;

	for( i=0 ; i < nt_bool.ib7 ; i++ )
		nt_bool.bvvec[i] = i % 2 ;

	for( i=0 ; i < nt_bool.ib7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_bool.bvmat[i][j] = (j + i) % 2 ;
}


struct {
	int	pu2;
	int	pu3;
	int	pu5;
	int	pu7;
	int	purnd;
	int	puvec[5];
	int	pumat[4][3];
	int	puvvec[ /* u7 */ 6];
	int	puvmat[ /* u7 */ 6][3];
} nt_puint;


nt_book_puint() {

	HBNAME( the_id, "nt_uint", (int *) &nt_puint,
		"pu2[0,1]:U,"
		"pu3[0,2]:U,"
		"pu5[0,4]:U,"
		"pu7[0,6]:U,"
		"purnd[0,511]:U,"
		"puvec(5)[0,100]:U,"
		"pumat(3,4)[0,100]:U,"
		"puvvec(pu7)[0,100]:U,"
		"puvmat(3,pu7)[0,100]:U"
		);
}


nt_fill_puint( unsigned long ievt )
{
	static unsigned short int	xi[3];
	int				i,j;

	nt_puint.pu2 = ievt % 2 ;
	nt_puint.pu3 = ievt % 3 ;
	nt_puint.pu5 = ievt % 5 ;
	nt_puint.pu7 = ievt % 7 ;
	nt_puint.purnd = nrand48( xi ) % 512;

	for( i=0 ; i < 5 ; i++ )
		nt_puint.puvec[i] = i + 1 ;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_puint.pumat[i][j] = (j+1)*10 + i + 1 ;

	for( i=0 ; i < nt_puint.pu7 ; i++ )
		nt_puint.puvvec[i] = i + 1 ;

	for( i=0 ; i < nt_puint.pu7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_puint.puvmat[i][j] = (j+1)*10 + i + 1 ;
}


int	uval_list[] = { 0, 0xdeadbeaf, 0xffffffff };
int	uval_list_len = sizeof(uval_list)/sizeof(int);


struct {
	int	uval;
	int	u2;
	int	u3;
	int	u5;
	int	u7;
	int	urnd;
	int	uvec[5];
	int	umat[4][3];
	int	uvvec[ /* u7 */ 6];
	int	uvmat[ /* u7 */ 6][3];
} nt_uint;


nt_book_uint() {

	HBNAME( the_id, "nt_uint", (int *) &nt_uint,
		"uval:U,"
		"u2:U,"
		"u3:U,"
		"u5:U,"
		"u7[0,6]:U,"
		"urnd:U,"
		"uvec(5):U,"
		"umat(3,4):U,"
		"uvvec(u7):U,"
		"uvmat(3,u7):U"
		);
}


nt_fill_uint( unsigned long ievt )
{
	static unsigned short int	xi[3];
	int				i,j;

	nt_uint.uval = uval_list[ ievt % uval_list_len ];
	nt_uint.u2 = 1000 + ievt % 2 ;
	nt_uint.u3 = 1000 + ievt % 3 ;
	nt_uint.u5 = 1000 + ievt % 5 ;
	nt_uint.u7 = ievt % 7 ;
	nt_uint.urnd = nrand48( xi ) ;

	for( i=0 ; i < 5 ; i++ )
		nt_uint.uvec[i] = 1000 + i + 1 ;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_uint.umat[i][j] = 1000 + (j+1)*10 + i + 1 ;

	for( i=0 ; i < nt_uint.u7 ; i++ )
		nt_uint.uvvec[i] = 1000 + i + 1 ;

	for( i=0 ; i < nt_uint.u7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_uint.uvmat[i][j] = 1000 + (j+1)*10 + i + 1 ;
}


int	ival_list[] = { 0x80000001, -12345, 0, 12345, 0x7fffffff };
int	ival_list_len = sizeof(ival_list)/sizeof(int);


struct {
	int	ival;
	int	i2;
	int	i3;
	int	i5;
	int	i7;
	int	irnd;
	int	ivec[5];
	int	imat[4][3];
	int	ivvec[ /* i7 */ 6];
	int	ivmat[ /* i7 */ 6][3];
	int	ievt;
} nt_int;


nt_book_int() {

	HBNAME( the_id, "nt_int", (int *) &nt_int,
		"ival:I,"
		"i2:I,"
		"i3:I,"
		"i5:I,"
		"i7[0,6]:I,"
		"irnd:I,"
		"ivec(5):I,"
		"imat(3,4):I,"
		"ivvec(i7):I,"
		"ivmat(3,i7):I,"
		"ievt:I"
		);
}


nt_fill_int( unsigned long ievt )
{
	static unsigned short int	xi[3];
	int				ixor = 0, i,j;

	nt_int.ival = ival_list[ ievt % ival_list_len ];
	nt_int.i2 = 2000 + ievt % 2;
	nt_int.i3 = 2000 + ievt % 3;
	nt_int.i5 = 2000 + ievt % 5;
	nt_int.i7 = ievt % 7;
	nt_int.irnd = jrand48( xi );

	for( i=0 ; i < 5 ; i++ )
		nt_int.ivec[i] = 2000 + i + 1;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_int.imat[i][j] = 2000 + (j+1)*10 + i + 1;

	for( i=0 ; i < nt_int.i7 ; i++ )
		nt_int.ivvec[i] = 2000 + i + 1;

	for( i=0 ; i < nt_int.i7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_int.ivmat[i][j] = 2000 + (j+1)*10 + i + 1;

	nt_int.ievt = ievt + 1;
}


float	fval_list[] = { -FLT_MAX, -12345., -FLT_MIN, 0., FLT_MIN, 12345., FLT_MAX };
int	fval_list_len = sizeof(fval_list)/sizeof(float);


struct {
	float	fval;
	float	f2;
	float	f3;
	float	f5;
	float	f7;
	int	if7;
	float	frnd;
	float	fvec[5];
	float	fmat[4][3];
	float	fvvec[ /* if7 */ 6];
	float	fvmat[ /* if7 */ 6][3];

	/* special fields */
	float	gauss;
	float	d3gauss[3];
	float	d3torus[3];
} nt_float;


nt_book_float() {

	HBNAME( the_id, "nt_float", (int *) &nt_float,
		"fval:R,"
		"f2:R,"
		"f3:R,"
		"f5:R,"
		"f7:R,"
		"if7[0,6]:I,"
		"frnd:R,"
		"fvec(5):R,"
		"fmat(3,4):R,"
		"fvvec(if7):R,"
		"fvmat(3,if7):R,"

		"gauss:R,"
		"d3gauss(3):R,"
		"d3torus(3):R"
		);
}


nt_fill_float( unsigned long ievt )
{
	static unsigned short int	xi[3];
	double				gauss48();
	int				i, j;

	nt_float.fval = fval_list[ ievt % fval_list_len ];
	nt_float.f2 = 3000. + fmod( (double) ievt, 2. );
	nt_float.f3 = 3000. + fmod( (double) ievt, 3. );
	nt_float.f5 = 3000. + fmod( (double) ievt, 5. );
	nt_float.f7 = 3000. + fmod( (double) ievt, 7. );
	nt_float.if7 = ievt % 7;
	nt_float.frnd = erand48( xi );

	for( i=0 ; i < 5 ; i++ )
		nt_float.fvec[i] = 3000. + i + 1;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_float.fmat[i][j] = 3000. + (j+1)*10. + i + 1;

	for( i=0 ; i < nt_float.if7 ; i++ )
		nt_float.fvvec[i] = 3000. + i + 1;

	for( i=0 ; i < nt_float.if7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_float.fvmat[i][j] = 3000. + (j+1)*10 + i + 1;

	nt_float.gauss = gauss48( xi );
	for( i=0; i < 3 ; i++ )
		nt_float.d3gauss[i] = gauss48(xi);
}


double	dval_list[] = { -DBL_MAX, -12345., -DBL_MIN, 0., DBL_MIN, 12345., DBL_MAX };
int	dval_list_len = sizeof(dval_list)/sizeof(double);

struct {
	double	dval;
	double	d2;
	double	d3;
	double	d5;
	double	d7;
	int	id7;
	int	idummy;
	double	drnd;
	double	dvec[5];
	double	dmat[4][3];
	double	dvvec[ /* id7 */ 6];
	double	dvmat[ /* id7 */ 6][3];

	/* special fields */
	double	dgauss;
	double	dd3gauss[3];
	double	dd3torus[3];
} nt_double;


nt_book_double() {

	HBNAME( the_id, "nt_double", (int *) &nt_double,
		"dval:R*8,"
		"d2:R*8,"
		"d3:R*8,"
		"d5:R*8,"
		"d7:R*8,"
		"id7[0,6]:I,"
		"idummy:I,"
		"drnd:R*8,"
		"dvec(5):R*8,"
		"dmat(3,4):R*8,"
		"dvvec(id7):R*8,"
		"dvmat(3,id7):R*8,"

		"dgauss:R*8,"
		"dd3gauss(3):R*8,"
		"dd3torus(3):R*8"
		);
}


nt_fill_double( unsigned long ievt )
{
	static unsigned short int	xi[3];
	double				gauss48();
	int				i, j;

	nt_double.dval = dval_list[ ievt % dval_list_len ];
	nt_double.d2 = 4000. + fmod( (double) ievt, 2. );
	nt_double.d3 = 4000. + fmod( (double) ievt, 3. );
	nt_double.d5 = 4000. + fmod( (double) ievt, 5. );
	nt_double.d7 = 4000. + fmod( (double) ievt, 7. );
	nt_double.id7 = ievt % 7;
	nt_double.drnd = erand48( xi );

	for( i=0 ; i < 5 ; i++ )
		nt_double.dvec[i] = 4000. + i + 1;

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_double.dmat[i][j] = 4000. + (j+1)*10. + i + 1;

	for( i=0 ; i < nt_double.id7 ; i++ )
		nt_double.dvvec[i] = 4000. + i + 1;

	for( i=0 ; i < nt_double.id7 ; i++ )
		for( j=0 ; j < 3 ; j++ )
			nt_double.dvmat[i][j] = 4000. + (j+1)*10 + i + 1;

	nt_double.dgauss = gauss48( xi );
	for( i=0; i < 3 ; i++ )
		nt_double.dd3gauss[i] = gauss48(xi);
}


char *	cval_list[] = { "My dummy str" };
int	cval_list_len = sizeof(cval_list)/sizeof(char *);


struct {
	char	cval[32];
	char	c2[4];
	char	c3[8];
	char	c5[12];
	char	c7[16];
	int	ic7;
	char	crnd[32];
	char	cvec[5][32];
	char	cmat[4][3][32];
	char	cvvec[ /* ic7 */ 6][32];
	char	cvmat[ /* ic7 */ 6][3][32];
} nt_f_str;


struct {
	char	cval[33];
	char	c2[5];
	char	c3[9];
	char	c5[13];
	char	c7[17];
	int	ic7;
	char	crnd[33];
	char	cvec[5][33];
	char	cmat[4][3][33];
	char	cvvec[ /* ic7 */ 6][33];
	char	cvmat[ /* ic7 */ 6][3][33];
} nt_str;


nt_book_str() {

	HBNAME( the_id, "nt_str", (int *) &nt_f_str,
		"cval:C*32,"
		"c2:C*4,"
		"c3:C*8,"
		"c5:C*12,"
		"c7:C*16,"
		"ic7[0,6]:I,"
		"crnd:C*32,"
		"cvec(5):C*32,"
		"cmat(3,4):C*32,"
		"cvvec(ic7):C*32,"
		"cvmat(3,ic7):C*32"
		);
}

char	*count[] = {
	"zero",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
	"ten"
};

nt_fill_str( unsigned long ievt )
{
	static unsigned short int	xi[3];
	int				i,j;
	double				gauss48();

	strcpy( nt_str.cval, cval_list[ ievt % cval_list_len ] );
	strcpy( nt_str.c2, count[ ievt % 2 ] );
	strcpy( nt_str.c3, count[ ievt % 3 ] );
	strcpy( nt_str.c5, count[ ievt % 5 ] );
	strcpy( nt_str.c7, count[ ievt % 7 ] );

	nt_f_str.ic7 = ievt % 7 ;

	strcpy( nt_str.crnd, count[ (int) (6 + 4.*gauss48( xi )) ] );

	for( i=0 ; i < 5 ; i++ ) {
			char	buf[33];
			sprintf( buf, "(%s)", count[ i+1 ] );
			strcpy( nt_str.cvec[i], buf );
		}

	for( i=0 ; i < 4 ; i++ )
		for( j=0 ; j < 3 ; j++ ) {
			char	buf[33];
			sprintf( buf, "(%s,%s)", count[ j+1 ], count[ i+1 ] );
			strcpy( nt_str.cmat[i][j], buf );
		}

	for( i=0 ; i < nt_uint.u7 ; i++ ) {
			char	buf[33];
			sprintf( buf, "(%s)", count[ i+1 ] );
			strcpy( nt_str.cvvec[i], buf );
		}

	for( i=0 ; i < nt_uint.u7 ; i++ )
		for( j=0 ; j < 3 ; j++ ) {
			char	buf[33];
			sprintf( buf, "(%s,%s)", count[ j+1 ], count[ i+1 ] );
			strcpy( nt_str.cvmat[i][j], buf );
		}

	C2FCBSTR( nt_str.cval, nt_f_str.cval, 0 );
	C2FCBSTR( nt_str.c2, nt_f_str.c2, 0 );
	C2FCBSTR( nt_str.c3, nt_f_str.c3, 0 );
	C2FCBSTR( nt_str.c5, nt_f_str.c5, 0 );
	C2FCBSTR( nt_str.c7, nt_f_str.c7, 0 );
	C2FCBSTR( nt_str.crnd, nt_f_str.crnd, 0 );
	C2FCBSTR( nt_str.cvec, nt_f_str.cvec, 1 );
	C2FCBSTR( nt_str.cmat, nt_f_str.cmat, 2 );
	C2FCBSTR( nt_str.cvvec, nt_f_str.cvvec, 1 );
	C2FCBSTR( nt_str.cvmat, nt_f_str.cvmat, 2 );

}


cwn_end()
{
	int	icycle;

	HROUT( 10, icycle, " ");

	HRENDC("TUPLE");

	return R_OK;
}


char	all_flags[] = "bool,puint,uint,int,float,double,str";

int	main( argc, argv )
int	argc;
char	*argv[];
{
	unsigned long	i, limit = 100, base = 2 * 3 * 5 * 7;
	int		lrecl = 1024;
	int	do_bool = 0, do_puint = 0, do_uint = 0, do_int = 0;
	int	do_float = 0, do_double = 0, do_str = 0, do_rwn = 0;
	int		c;
	extern char	*optarg;
	extern int	optind, optopt;
	char		*fname = "tuple.hbook";
	char		*flags = all_flags;
	char		buf[512], *p;


	prog_name = argv[0];

	while( (c = getopt(argc, argv, "t:n:o:l:r")) != -1 ) {
		switch( c ) {
		case 't':
			flags = optarg;
			break;
		case 'l':
			lrecl = atoi( optarg );
			break;
		case 'n':
			limit = atol( optarg ) / base;
			break;
		case 'o':
			fname = optarg;
			break;
		case 'r':
			do_rwn = 1;
			break;
		case '?':
			fprintf(stderr, "Unrecognized option: -%c\n", optopt);
			usage();
			break;
		default:
			abort();
		}
	}

	if ( optind != argc ) {
		usage();
	}

	limit *= base;

	if ( do_rwn ) {
		create_rwn( fname, lrecl, limit );
		exit( 0 );
	}

	strcpy( buf, "," );
	strcat( buf, flags );
	p = strtok( buf, "," );

	if ( p != 0 ) do {
		if ( strcmp( p, "bool" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_bool = 1;
		} else if ( strcmp( p, "puint" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_puint = 1;
		} else if ( strcmp( p, "uint" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_uint = 1;
		} else if ( strcmp( p, "int" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_int = 1;
		} else if ( strcmp( p, "float" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_float = 1;
		} else if ( strcmp( p, "double" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_double = 1;
		} else if ( strcmp( p, "str" ) == 0 )  {
			fprintf( stderr, "Using type %s\n", p );
			do_str = 1;
		} else {
			fprintf( stderr, "%s: type '%s' unknown\n",
				prog_name, p );
		}
		p = strtok( (char *) 0, "," );
	} while ( p != 0 );

	
	cwn_start( 10, fname, lrecl );

	nt_book_basic();

	if ( do_bool )
		nt_book_bool();
	if ( do_puint )
		nt_book_puint();
	if ( do_uint )
		nt_book_uint();
	if ( do_int )
		nt_book_int();
	if ( do_float )
		nt_book_float();
	if ( do_double )
		nt_book_double();
	if ( do_str )
		nt_book_str();

	fprintf( stderr, "Creating event 1 to %ld\n", limit );
	for( i=0 ; i < limit ; i++ ) {

		nt_fill_basic(i);

		if ( do_bool )
			nt_fill_bool(i);

		if ( do_puint )
			nt_fill_puint(i);

		if ( do_uint )
			nt_fill_uint(i);

		if ( do_int )
			nt_fill_int(i);

		if ( do_float )
			nt_fill_float(i);

		if ( do_double )
			nt_fill_double(i);

		if ( do_str )
			nt_fill_str(i);

		HFNT( 10 );

		if ( (i % 1000) == 0 ) {
			printf( "%10ld\n", i );
		}
	}


	cwn_end();
	return 0;
}


double
gauss48( unsigned short int xi[3] ) {
	double	sum;
	int	i;

	for( sum = 0., i=0 ; i < 12 ; i++ )
		sum += erand48( xi );
	
	return ( sum - 6. ) / 4.;
}


void
usage() {
	fprintf( stderr, "usage: %s [-n 10000] [-o out.hbook] "
		"[-l 1024] [-t type1,type2,...] [-r]\n", prog_name );
	fprintf( stderr, "-n number : the number is rounded down "
		"to a multiple of 2*3*5*7\n" );
	fprintf( stderr, "type1 : can be %s\n", all_flags );
	exit(1);
}


dummy_qnext()
{
	fprintf( stderr, "Dummy QNEXT called ...\n" );
	abort();
}


FCALLSCSUB0(dummy_qnext,QNEXT,qnext)
