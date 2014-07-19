/*
 *  qp_exe_stack.h  --
 *	Definitions and macros for the runtime stack
 *	handling.
 *
 *  Original: 17-Jan-1995 11:12
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_stack.h,v 1.10 1996/09/09 09:11:43 lecointe Exp $
 *
 *  $Log: qp_exe_stack.h,v $
 *  Revision 1.10  1996/09/09 09:11:43  lecointe
 *  Split "qp_execute.c"in two smaller files, for optimization purpose.
 *  "qp_exec_global.h" contains the global variables that were in
 *  "qp_execute.c", and their initialization (if any)
 *
 *  Revision 1.9  1996/07/24 13:49:39  dinofm
 *  Just a few optimization in the qp_exe_evt function and stack handling
 *  macros. Warning, this optimizations would affect the 'hand-made'
 *  timing statistics.
 *
 *  Revision 1.8  1996/05/13 16:23:42  maartenb
 *  - Add 4+ dimension dynamic array access.
 *
 *  Revision 1.7  1996/04/29 11:43:35  maartenb
 *  - Add a macro to push a generic shape.
 *
 *  Revision 1.6  1996/04/26 09:09:12  maartenb
 *  - Implemented dynamic access to 3dim arrays.
 *
 *  Revision 1.5  1996/04/23 18:38:35  maartenb
 *  - Add RCS keywords
 *
 *
 */


#define	MAX_FRAME_PTR	100
#define	STACK_SIZE	100000


/* the stack grow downwards */
#ifdef INIT_STACK_TO_NULL
Extern Int32	*stack=0;
#else
Extern Int32	*stack;
#endif

/* the frame list extends upward  i=1,2,3,... */
/* frame[i] is the first used word */
Extern int	frame[MAX_FRAME_PTR+1];
Extern int	frame_size[MAX_FRAME_PTR+1];
Extern DataType	frame_type[MAX_FRAME_PTR+1];

/* framep == 0 means no frames on stack */
/* frame[0] is used to align the first stack frame */
Extern int	framep;	

/* Stack to check shape conformance at runtime */

typedef struct {
	int	ndim;
	int	range[MAX_DIMENSION];
} Shape; 

Extern Shape	shape_stack[MAX_FRAME_PTR+1];
Extern Shape	*shape_sp;

/****/

Extern bool	db_stack;


#define	SET_FRAME_TYPE( type )						\
	do {								\
if(db_stack) sf_report("SET_FRAME_TYPE: Type=%s [%d]\n",datatype_to_str(type),framep);\
		frame_type[framep] = type;				\
	} while( 0 )

#define NEW_FRAME( type, size, ptr )					\
	{								\
		int	newfp;					\
		if ( framep == MAX_FRAME_PTR ) {			\
			sf_report( "qp_execute_seq: MAX_FRAME_PTR "	\
				"exceeded\n");				\
			*errp = R_INTERNAL_ERROR;			\
		}							\
		newfp = (frame[framep] - datatype_size[type]*(size)) & MAX_ALIGN;	\
		if ( newfp < 0 ) {					\
			sf_report( "qp_execute_seq: Stack overflow" );	\
			*errp = R_INTERNAL_ERROR;			\
		}							\
		framep += 1;						\
		frame[framep] = newfp;					\
		frame_size[framep] = (size);				\
		frame_type[framep] = type;				\
		ptr = (void *) (stack + newfp);				\
	} 

#define STACK_INIT							\
	do {								\
		framep = 0;						\
		shape_sp = shape_stack;					\
	} while ( 0 )
	
#define STACK_ALLOC							\
	do {								\
if(db_stack) sf_report("STACK_ALLOC (%d words)\n", STACK_SIZE);/**/\
	stack = (Int32 *) calloc( sizeof(Int32), STACK_SIZE );		\
	if ( stack == 0 ) {						\
		sf_report( "STACK_ALLOC: Cannot allocate stack\n" );	\
		*errp = R_ALLOC_ERROR;					\
	}								\
	frame[0] = STACK_SIZE; /* first unusable word !	*/		\
	framep = 0;							\
	} while ( 0 )

#define POP_FRAME(N)							\
	do {								\
if(db_stack) sf_report("POP_FRAME: n=%d [%d]\n", N, framep - N);/**/\
		if ( framep >= (N) ) {					\
			framep -= N;					\
		} else {						\
			sf_report( "POP_FRAME(%d): Failed framep = %d\n",\
				N, framep );				\
			*errp = R_ALLOC_ERROR;				\
		}							\
	} while (0)

#define SQUEEZE_FRAME( N )					\
	do {								\
		Int32		*new, *old = &stack[frame[framep]];	\
		int		size = frame_size[framep];		\
		DataType	type = frame_type[framep];		\
if(db_stack) sf_report("SQUEEZE_FRAME: type=%s, n=%d [%d]\n",datatype_to_str(type),N,framep-((N)+1));\
									\
		if ( framep > (N) ) {					\
			framep -= (N)+1;				\
			NEW_FRAME(type,size,new);			\
			(void) memmove( new, old,			\
			(size_t) size*4*datatype_size[type] );	\
		} else {						\
			sf_report( "SQUEEZE_FRAME(%d): Failed framep = %d\n",\
				N, framep );				\
			*errp = R_ALLOC_ERROR;				\
		}							\
	} while (0)


#define	SHAPE_PUSH_1( N )						\
	do {								\
if(db_stack) sf_report("SHAPE_PUSH_1: (%d) [%d]\n", N, shape_sp-shape_stack+1 );/**/\
		(++shape_sp)->ndim = 1;					\
		shape_sp->range[0] = N;					\
	} while(0)


#define	SHAPE_PUSH_2( n1, n2 )						\
	do {								\
if(db_stack) sf_report("SHAPE_PUSH_2: (%d,%d) [%d]\n", n1, n2, shape_sp-shape_stack+1 );/**/\
		(++shape_sp)->ndim = 2;					\
		shape_sp->range[0] = n1;				\
		shape_sp->range[1] = n2;				\
	} while(0)


#define	SHAPE_PUSH_3( n1, n2, n3 )					\
	do {								\
if(db_stack) sf_report("SHAPE_PUSH_3: (%d,%d,%d) [%d]\n", n1, n2, n3, shape_sp-shape_stack+1 );/**/\
		(++shape_sp)->ndim = 3;					\
		shape_sp->range[0] = n1;				\
		shape_sp->range[1] = n2;				\
		shape_sp->range[2] = n3;				\
	} while(0)


#define	SHAPE_PUSH_N( _n, _nv )					\
	do { int i;								\
if(db_stack) { sf_report("SHAPE_PUSH_N: n=%d [%d] (", _n, shape_sp-shape_stack+1 );/**/\
for( i=0 ; i<_n ; i++ ) { printf( " %d", _nv[i] ); } printf("\n" ); }	\
		(++shape_sp)->ndim = _n;				\
		for ( i=0 ; i < _n ; i++ ) {				\
			shape_sp->range[i] = _nv[i];			\
		}							\
	} while(0)


#define	SHAPE_LOAD(pc)							\
	do {								\
		register int	*p, i = *pc++;				\
if(db_stack) sf_report("SHAPE_LOAD: [%d]\n", shape_sp-shape_stack+1 );/**/\
		(++shape_sp)->ndim = i;					\
		p = shape_sp->range;					\
		for( ; i > 0 ; i-- ) {					\
			*p++ = *pc++;					\
		}							\
	} while (0)


#define	SHAPE_ADJUST(I)							\
	do {								\
if(db_stack) sf_report("SHAPE_ADJUST: i=%d [%d]\n", I, shape_sp-shape_stack );/**/\
		shape_sp->range[shape_sp->ndim-1] = I;			\
	} while (0);	

#define SHAPE_CHECK_N(_n,flag)						\
	do {								\
		int	i, j, d = 0, n = _n, *p[MAX_EXPRS];		\
		Shape	*sp = shape_sp;					\
if(db_stack) sf_report("SHAPE_CHECK_N: n=%d [%d]\n", _n, shape_sp-shape_stack );/**/\
									\
		flag = TRUE;						\
		d = sp->ndim;	/* conformance checked at compile time */\
		for ( i=0 ; i < n ; i++, sp-- ) {			\
			p[i] = sp->range;				\
			qp_assert( d == sp->ndim );			\
		}							\
		for ( j=0 ; j < d ; j++, p[0]++ ) {			\
			for ( i=1 ; i < n ; i++ ) {			\
				if ( *(p[i]++) != *p[0] ) {		\
					flag = FALSE;			\
					break;				\
				}					\
			}						\
			if ( !flag ) break;				\
		}							\
	} while (0)


#define	SHAPE_POP(N)							\
	do {								\
if(db_stack) sf_report("SHAPE_POP: n=%d [%d]\n", N, shape_sp-shape_stack-N);/**/\
		shape_sp -= N;						\
	} while(0)
